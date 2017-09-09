-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

{-# LANGUAGE FlexibleContexts, OverloadedStrings #-}

module KS.Clean.Closed.Command
   ( run
   )
   where

import Control.Arrow ( (***) )
import Control.Lens ( (^.), (&), (.~) )
import Control.Monad.Reader
import Control.Monad.Trans ( MonadIO )
import Data.Aeson ( FromJSON, Value (Object), (.:), (.:?), (.!=), parseJSON )
import qualified Data.Bson as Bson
import Data.Bson.Generic ( fromBSON, toBSON )
import Data.Maybe ( fromJust, isJust, listToMaybe, mapMaybe )
import Data.String.Conv ( toS )
import Data.Text ( Text )
import Data.Version ( showVersion )
import Database.Mongo.Util ( lastStatus )
import Database.MongoDB hiding ( Document, isClosed, options ) -- FIXME
{-
import Database.MongoDB ( Pipe, (=:), access, count, delete, deleteOne, find,
   findAndModify, insertMany_, rest, select, slaveOk, sort )
-}
import KS.Data.BSON ( combineId, separateId )
import Network.Wreq ( Response, asJSON, defaults, getWith, param, responseBody )
import Paths_ks_download ( version )
import Text.Printf ( printf )

import KS.Clean.Closed.Options
import KS.Data.Document ( Document (..) )
import qualified KS.Data.Feedback as F
import KS.Data.Inspection ( date )
import KS.Data.Place ( Place (name, place_id) )
import KS.Database.Mongo.Util ( coll_feedback, coll_inspections_all, coll_inspections_archived,
   coll_inspections_recent, mongoConnect )
import KS.Locate.Config ( Config (googleApiKey, logPriority),
   keyString, loadConfig )
import KS.Log
import KS.Util ( withRetry )


{-
data AppConfig = AppConfig
   { acPipe :: Pipe
   , acDatabase :: Text
   , acOptions :: ClosedOptions
   , acLocateConf :: Config
   }

type ClosedApp = ReaderT AppConfig IO

runClosedApp :: AppConfig -> ClosedApp a -> IO (Either String a)
runClosedApp conf act = runReaderT act conf
-}


run :: ClosedOptions -> IO ()
run options = do
   -- Load the config file
   locateConf <- loadConfig . optConfDir $ options

   -- Start the log
   initLogging $ logPriority locateConf
   noticeM lname $
      printf "ks-clean version %s started, command: closed" (showVersion version)
   logStartMsg lname
   --noticeM lname line

   conn <- mongoConnect . optConfDir $ options

   idFPairS <- getNewClosedFeedback conn
   --mapM_ (resolveFeedback conn options locateConf) $ take 1 idFPairS
   mapM_ (resolveFeedback conn options locateConf) idFPairS

   noticeM lname line
   logStopMsg lname


getNewClosedFeedback :: (Pipe, Text) -> IO [(Bson.Document, F.Feedback)]
getNewClosedFeedback (pipe, database) =
   access pipe slaveOk database $ do
      wholeDocs <- (rest =<< find (select
         (toBSON F.New ++ toBSON F.Closed)
         coll_feedback)
         { sort = [ "date" =: (1 :: Int) ] }
         )
      return $ map ((id *** (fromJust . fromBSON)) . separateId) wholeDocs


resolveFeedback :: (Pipe, Text) -> ClosedOptions -> Config -> (Bson.Document, F.Feedback) -> IO ()
resolveFeedback (pipe, database) options locateConf (idDoc, fb) = do
   noticeM lname line
   noticeM lname "Checking feedback record:"
   noticeM lname . show $ fb

   let mpid = F.place_id fb
   newStatus <- case mpid of
      Nothing -> do
         warningM lname "This Closed feedback record has NO PLACES ID, this is BAD DATA"
         warningM lname "It will be marked as Resolved, but something wrong happened somewhere"
         return F.Resolved
      Just placeId -> do
         mdoc <- retrievePlace (pipe, database) placeId
         case mdoc of
            Just doc -> do
               isClosed' <- isClosed locateConf doc
               if isClosed'
                  then if (optArchive options)
                     then do
                        _ <- archiveEstablishment (pipe, database) doc
                        return ()
                     else noticeM lname $ "Inspection record was NOT modified"
                  else warningM lname "Place is not closed, bad feedback"
               return F.Resolved
            Nothing -> do
               noticeM lname "Place is not in our database, duplicate feedback"
               return F.Duplicate

   -- Set feedback status to Resolved/Duplicate
   if optArchive options
      then do
         result <- access pipe slaveOk database $ findAndModify (select idDoc coll_feedback)
            (combineId (idDoc, toBSON (fb { F.status = newStatus })))

         noticeM lname "Result of setting Feedback record status:"
         noticeM lname . show $ result
      else do
         noticeM lname $ printf "Archive was not was specified in the args, no change to the Feedback record"


retrievePlace :: (Pipe, Text) -> Text -> IO (Maybe Document)
retrievePlace (pipe, database) placeId = do
   (countAll, mdoc) <- access pipe slaveOk database $ do
      let query = [ "place.place_id" =: placeId ]
      (,) <$> (count (select query coll_inspections_all))
         <*> (listToMaybe . mapMaybe fromBSON <$>
            (rest =<< find (select query coll_inspections_recent)))

   when ((isJust mdoc) && (countAll < 1)) $ do
      warningM lname $ printf "Record exists in recent but none in all for:\n%s\n" (formatForLog (fromJust mdoc))
   return mdoc


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
   --threadDelay 250000  -- 0.25s

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
