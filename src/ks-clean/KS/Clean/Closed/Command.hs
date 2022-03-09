-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

{-# LANGUAGE FlexibleContexts, OverloadedStrings #-}

module KS.Clean.Closed.Command
   ( run
   )
   where

import Control.Arrow ( (***) )
import Control.Lens ( (^.), (&), (.~) )
import Control.Monad ( unless, when )
import Control.Monad.Except ( runExceptT, throwError )
import Control.Monad.Reader ( ReaderT, runReaderT, asks )
import Control.Monad.Trans ( MonadIO, lift, liftIO )
import Data.Aeson ( FromJSON, Value (Object), (.:), (.:?), (.!=),
   parseJSON )
import qualified Data.Bson as Bson
import Data.Bson.Generic ( fromBSON, toBSON )
import Data.Maybe ( fromJust, isJust, listToMaybe, mapMaybe )
import Data.String.Conv ( toS )
import Data.Text ( Text )
import Data.Version ( showVersion )
import Database.Mongo.Util ( lastStatus )
import Database.MongoDB ( Pipe, (=:), access, count, delete,
   deleteOne, find, findAndModify, insertMany_, rest, select, slaveOk,
   sort )
import KS.Data.BSON ( combineId, separateId )
import Network.Wreq ( Response, asJSON, defaults, getWith, param,
   responseBody )
import Paths_ks_download ( version )
import Text.Printf ( printf )

import KS.Clean.Closed.Options ( ClosedOptions (optArchive, optConfDir) )
import KS.Data.Document ( Document (..) )
import qualified KS.Data.Feedback as F
import KS.Data.Inspection ( date )
import KS.Data.Place ( Place (name, place_id) )
import KS.Database.Mongo.Util ( coll_feedback, coll_inspections_all,
   coll_inspections_archived, coll_inspections_recent, mongoConnect )
import KS.Locate.Config ( Config (googleApiKey, logPriority),
   keyString, loadConfig )
import KS.Log ( Priority (NOTICE, WARNING), criticalM, errorM, initLogging, line, lname,
   logM, logStartMsg, logStopMsg, noticeM, warningM )
import KS.Util ( Seconds (..), TryCount (Remaining), withRetry )


data AppConfig = AppConfig
   { acPipe :: Pipe
   , acDatabase :: Text
   , acOptions :: ClosedOptions
   , acLocateConf :: Config
   }

type ClosedApp = ReaderT AppConfig IO

runClosedApp :: AppConfig -> ClosedApp a -> IO a
runClosedApp env act = runReaderT act env


run :: ClosedOptions -> IO ()
run options = do
   -- Load the config file
   locateConf <- loadConfig . optConfDir $ options

   -- Start the log
   initLogging $ logPriority locateConf
   noticeM lname line
   noticeM lname $
      printf "ks-clean version %s started, command: closed" (showVersion version)
   logStartMsg lname

   (pipe, database) <- mongoConnect (noticeM lname) . optConfDir $ options
   runClosedApp (AppConfig pipe database options locateConf) $ do
      idFPairS <- getNewClosedFeedback
      when (null idFPairS) $ liftIO $ do
         noticeM lname line
         noticeM lname "No New, Closed Feedback records to process at this time"

      mapM_ resolveFeedback idFPairS
      -- For debugging, only let one through per run
      --mapM_ resolveFeedback $ take 1 idFPairS

   noticeM lname line
   logStopMsg lname


getNewClosedFeedback :: ClosedApp [(Bson.Document, F.Feedback)]
getNewClosedFeedback = do
   pipe <- asks acPipe
   database <- asks acDatabase
   liftIO $ access pipe slaveOk database $ do
      wholeDocs <- (rest =<< find (select
         (toBSON F.New ++ toBSON F.Closed)
         coll_feedback)
         { sort = [ "date" =: (1 :: Int) ] }
         )
      return $ map ((id *** (fromJust . fromBSON)) . separateId) wholeDocs


type ResolveFailure = (Priority, String, F.Status)

resolveFeedback :: (Bson.Document, F.Feedback) -> ClosedApp ()
resolveFeedback (idDoc, fb) = do
   liftIO $ do
      noticeM lname line
      noticeM lname "Checking feedback record:"
      noticeM lname . show $ fb

   options <- asks acOptions

   -- resolutionResult :: Either ResolveFailure F.Feedback
   resolutionResult <- runExceptT $ do
      placeId <- maybe (throwError (WARNING, "This Closed feedback record has NO PLACES ID, this is BAD DATA\nIt will be marked as Resolved, but something wrong happened somewhere", F.Resolved))
         return $ F.place_id fb
      doc <- maybe
         (throwError (NOTICE, "Place is not in our database, duplicate feedback", F.Duplicate))
         return =<< (lift $ retrievePlace placeId)
      isClosed' <- lift $ isClosed doc
      unless isClosed' (throwError (WARNING, "Place is not closed, bad feedback", F.Resolved))
      unless (optArchive options)
         (throwError (WARNING, "Inspection record was NOT modified", F.Resolved))
      _ <- lift $ archiveEstablishment doc
      return F.Resolved

   -- Log errors if necessary and extract the new status for the Feedback record
   newStatus <- either handleResolutionFailure return resolutionResult

   -- Set feedback status to Resolved/Duplicate, whatever was determined above
   pipe <- asks acPipe
   database <- asks acDatabase
   liftIO $ if optArchive options
      then do
         result <- access pipe slaveOk database $ findAndModify (select idDoc coll_feedback)
            (combineId (idDoc, toBSON (fb { F.status = newStatus })))

         noticeM lname "Result of setting Feedback record status:"
         noticeM lname . show $ result
      else do
         noticeM lname $ printf "Archive was not was specified in the args, no change to the Feedback record"

   where
      handleResolutionFailure :: ResolveFailure -> ClosedApp F.Status
      handleResolutionFailure (prio, msg, newStatus) = do
         liftIO $ logM lname prio msg
         return newStatus


retrievePlace :: Text -> ClosedApp (Maybe Document)
retrievePlace placeId = do
   pipe <- asks acPipe
   database <- asks acDatabase
   (countAll, mdoc) <- liftIO $ access pipe slaveOk database $ do
      let query = [ "place.place_id" =: placeId ]
      (,) <$> (count (select query coll_inspections_all))
         <*> (listToMaybe . mapMaybe fromBSON <$>
            (rest =<< find (select query coll_inspections_recent)))

   when ((isJust mdoc) && (countAll < 1)) $ do
      liftIO $ warningM lname $ printf "Record exists in recent but none in all for:\n%s\n" (formatForLog (fromJust mdoc))
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


isClosed :: Document -> ClosedApp Bool
isClosed doc = do
   key <- asks $ toS . keyString . googleApiKey . acLocateConf
   let placeID = place_id . place $ doc
   let url = "https://maps.googleapis.com/maps/api/place/details/json"

   let wopts = defaults & param "key" .~ [key] & param "placeid" .~ [placeID]

   er <- liftIO $ withRetry (Remaining 5) (Seconds 3) (getWith wopts url >>= asJSON) (warningM lname)

   liftIO $ either placeLookupFailed placeLookupSucceeded er

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


archiveEstablishment :: Document -> ClosedApp Bool
archiveEstablishment doc = do
   pipe <- asks acPipe
   database <- asks acDatabase
   liftIO $ access pipe slaveOk database $ do
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
