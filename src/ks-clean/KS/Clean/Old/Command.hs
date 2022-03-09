-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

{-# LANGUAGE FlexibleContexts, OverloadedStrings #-}

module KS.Clean.Old.Command
   ( run
   )
   where

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
import Database.MongoDB ( Pipe, (=:), access, delete, deleteOne,
   find, insertMany_, rest, select, slaveOk, sort )
import Network.Wreq ( Response, asJSON, defaults, getWith, param, responseBody )
import Paths_ks_download ( version )
import System.Environment ( setEnv )
import Text.Printf ( printf )

import KS.Clean.Old.Options ( OldOptions (optArchive, optBeforeDate, optConfDir) )
import KS.Data.Document ( Document (..) )
import KS.Data.Inspection ( date )
import KS.Data.Place ( Place (name, place_id) )
import KS.Database.Mongo.Util ( coll_inspections_all, coll_inspections_archived,
   coll_inspections_recent, mongoConnect )
import KS.Locate.Config ( Config (googleApiKey, logPriority),
   keyString, loadConfig )
import KS.Locate.Locate
import KS.Log
import KS.Util ( Seconds (..), TryCount (Remaining), dayToDateInt, nDaysAgo, withRetry )


run :: OldOptions -> IO ()
run options = do
   -- Load the config file
   locateConf <- loadConfig . optConfDir $ options

   -- Start the log
   initLogging $ logPriority locateConf
   noticeM lname $
      printf "ks-clean version %s started, command: old" (showVersion version)
   logStartMsg lname
   noticeM lname line

   conn <- mongoConnect (noticeM lname) . optConfDir $ options

   -- Figure out the real date to work with for examining places
   setEnv "TZ" "America/New_York"  -- FIXME This is really not cool to do
   nineMonthsAgo <- dayToDateInt <$> (nDaysAgo 270 =<< getCurrentTimeZone)
   let beforeDate = maybe nineMonthsAgo id $ optBeforeDate options
   noticeM lname $ printf "Checking inspections on or before %d" beforeDate

   oldEsts <- getOldEstablishments conn beforeDate
   --oldEsts <- reverse . take 50 . reverse <$> getOldEstablishments conn (fromJust . optBeforeDate $ options)
   --oldEsts <- take 10 <$> getOldEstablishments conn beforeDate
   noticeM lname $ printf "Number of old establishments found: %d" (length oldEsts)
   closedEsts <- filterM (isClosed locateConf) oldEsts
   noticeM lname $ printf "Number closed: %d" (length closedEsts)
   if (optArchive options)
      then mapM_ (archiveEstablishment conn) closedEsts
      else noticeM lname $ "Database was NOT modified"

   noticeM lname line
   logStopMsg lname


getOldEstablishments :: (Pipe, Text) -> Int -> IO [Document]
getOldEstablishments (pipe, database) day =
   access pipe slaveOk database $
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

   er <- withRetry (Remaining 5) (Seconds 3) (getWith wopts url >>= asJSON) (warningM lname)

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


archiveEstablishment :: (Pipe, Text) -> Document -> IO Bool
archiveEstablishment (pipe, database) doc =
   access pipe slaveOk database $ do
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
