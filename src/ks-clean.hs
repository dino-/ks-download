-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

{-# LANGUAGE FlexibleContexts, OverloadedStrings #-}

import Control.Concurrent ( threadDelay )
import Control.Lens ( (^.), (&), (.~) )
import Control.Monad ( filterM )
import Control.Monad.Trans ( MonadIO )
import Data.Aeson ( FromJSON, Value (Object), (.:), (.:?), (.!=), parseJSON )
import Data.Bson.Generic ( fromBSON )
import Data.Maybe ( mapMaybe )
import Data.String.Conv ( toS )
import Data.Text ( Text )
import Data.Time ( getCurrentTimeZone )
import Data.Version ( showVersion )
import Database.Mongo.Util ( lastStatus )
import Database.MongoDB ( Host (..), Pipe, PortID (PortNumber), (=:),
   access, auth, connect, delete, deleteOne, find, insertMany_, rest,
   select, slaveOk, sort )
import Network.Wreq ( Response, asJSON, defaults, getWith, param, responseBody )
import Options.Applicative ( execParser )
import Paths_ks_download ( version )
import System.Environment ( setEnv )
import System.IO
   ( BufferMode ( NoBuffering )
   , hSetBuffering, stdout, stderr
   )
import Text.Printf ( printf )

import KS.Clean.OptsOld ( Options (optArchive, optBeforeDate, optConfDir), optsOld )
import KS.Data.Document ( Document (..) )
import KS.Data.Inspection ( date )
import KS.Data.Place ( Place (name, place_id) )
import qualified KS.Database.Mongo.Config as MC
import KS.Database.Mongo.Util ( coll_inspections_all, coll_inspections_archived,
   coll_inspections_recent )
import KS.Locate.Config ( Config (googleApiKey, logPriority),
   keyString, loadConfig )
import KS.Locate.Locate
import KS.Log
import KS.Util ( dayToDateInt, nDaysAgo, withRetry )


main :: IO ()
main = do
   -- No buffering, it messes with the order of output
   mapM_ (flip hSetBuffering NoBuffering) [ stdout, stderr ]

   options <- execParser optsOld

   -- Load the config file
   locateConf <- loadConfig . optConfDir $ options

   -- Start the log
   initLogging $ logPriority locateConf
   noticeM lname $
      printf "ks-clean version %s started" (showVersion version)
   logStartMsg lname
   noticeM lname line

   mongoConf <- MC.loadMongoConfig . optConfDir $ options

   -- Get a connection to Mongo, they call it a 'pipe'
   pipe <- connect $ Host (MC.ip mongoConf)
      (PortNumber . fromIntegral . MC.port $ mongoConf)

   -- Authenticate with mongo, log the auth state
   (access pipe slaveOk (MC.database mongoConf)
      $ auth (MC.username mongoConf) (MC.password mongoConf)) >>=
      \tf -> noticeM lname $ "Authenticated with Mongo: " ++ (show tf)

   -- Figure out the real date to work with for examining places
   setEnv "TZ" "America/New_York"  -- FIXME This is really not cool to do
   nineMonthsAgo <- dayToDateInt <$> (nDaysAgo 270 =<< getCurrentTimeZone)
   let beforeDate = maybe nineMonthsAgo id $ optBeforeDate options
   noticeM lname $ printf "Checking inspections on or before %d" beforeDate

   oldEsts <- getOldEstablishments mongoConf pipe beforeDate
   --oldEsts <- reverse . take 50 . reverse <$> getOldEstablishments mongoConf pipe (fromJust . optBeforeDate $ options)
   --oldEsts <- take 10 <$> getOldEstablishments mongoConf pipe beforeDate
   noticeM lname $ printf "Number of old establishments found: %d" (length oldEsts)
   closedEsts <- filterM (isClosed locateConf) oldEsts
   noticeM lname $ printf "Number closed: %d" (length closedEsts)
   if (optArchive options)
      then mapM_ (archiveEstablishment mongoConf pipe) closedEsts
      else noticeM lname $ "Database was NOT modified"

   noticeM lname line
   logStopMsg lname


getOldEstablishments :: MC.MongoConfig -> Pipe -> Int -> IO [Document]
getOldEstablishments mongoConf pipe day =
   access pipe slaveOk (MC.database mongoConf) $
      mapMaybe fromBSON <$> (rest =<< find (select
         [ "inspection.date" =: [ "$lte" =: day ] ]
         coll_inspections_recent)
         { sort = [ "inspection.date" =: (1 :: Int) ] }
         )


data ClosedStatus = PlaceOpen | PlaceClosed | BadData

instance FromJSON ClosedStatus where
   parseJSON (Object o) = do
      mbStatus <- o .:? "status"
      closed <- case (mbStatus :: Maybe Text) of
         -- Super-duper old ones appear to be NOT_FOUND, we believe. So closed, yes, True
         Just "NOT_FOUND" -> return True
         _ -> (o .: "result") >>= (\o' -> o' .:? "permanently_closed" .!= False)
      if closed then return PlaceClosed else return PlaceOpen
   parseJSON _ = return BadData


isClosed :: Config -> Document -> IO Bool
isClosed locateConf doc = do
   let key = toS . keyString . googleApiKey $ locateConf
   let placeID = place_id . place $ doc
   let url = "https://maps.googleapis.com/maps/api/place/details/json"

   let wopts = defaults & param "key" .~ [key] & param "placeid" .~ [placeID]

   er <- withRetry 3 2 (getWith wopts url >>= asJSON) (warningM lname)

   -- Slow down hits to Google
   threadDelay 250000  -- 0.25s

   either placeLookupFailed placeLookupSucceeded er

   where
      placeLookupFailed :: String -> IO Bool
      placeLookupFailed msg = do
         errorM lname $ printf "ERROR: %s\n  Looking up %s" (msg :: String) (formatForLog doc)
         return False


      placeLookupSucceeded :: Response ClosedStatus -> IO Bool
      placeLookupSucceeded r = do
         let s = r ^. responseBody
         case s of
            PlaceOpen -> do
               noticeM lname $ printf "OPEN:   %s" $ formatForLog doc
               return False
            PlaceClosed -> do
               noticeM lname $ printf "CLOSED: %s" $ formatForLog doc
               return True
            BadData -> do
               errorM lname "Got back unexpected JSON"
               return False


archiveEstablishment :: MC.MongoConfig -> Pipe -> Document -> IO Bool
archiveEstablishment mongoConf pipe doc =
   access pipe slaveOk (MC.database mongoConf) $ do
      liftIO $ noticeM lname $ printf "%s\nEditing data for %s" line (formatForLog doc)

      -- Find all in inspections_all with the target Places ID
      ds <- rest =<< find (select
         [ "place.place_id" =: (place_id . place $ doc) ]
         coll_inspections_all)

      -- Insert those records into inspections_archive
      insertMany_ coll_inspections_archived ds
      insertionStatus <- lastStatus

      case insertionStatus of
         Left msg -> do
            liftIO $ criticalM lname $ printf "ERROR Something went wrong with the insertion, aborting without removing anything!\n%s" msg
            return False
         Right _ -> do
            liftIO $ noticeM lname $ printf "Inspection documents successfully inserted into %s" (toS coll_inspections_archived :: String)

            -- Remove from inspections_all with the target Places ID
            delete (select
               [ "place.place_id" =: (place_id . place $ doc) ]
               coll_inspections_all)
            lastStatus >>= displayStatus ("Removed documents from " ++ (toS coll_inspections_all :: String))

            -- Remove from inspections_recent with the target Places ID
            deleteOne (select
               [ "place.place_id" =: (place_id . place $ doc) ]
               coll_inspections_recent)
            lastStatus >>= displayStatus ("Removed document from " ++ (toS coll_inspections_recent :: String))

            return True


formatForLog :: Document -> String
formatForLog doc = printf "%s %d %s" ((toS . place_id . place $ doc) :: String)
   (date . inspection $ doc) ((toS . name . place $ doc) :: String)


displayStatus :: (MonadIO m) => String -> Either String String -> m ()
displayStatus prefix e = either displayMsg displayMsg e
   where displayMsg msg = liftIO $ noticeM lname $ printf "%s: %s" prefix msg
