-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

{-# LANGUAGE OverloadedStrings #-}

module KS.Clean.Remove.Command
   ( run
   )
   where

import Control.Monad ( when )
import Control.Monad.Reader ( ReaderT, runReaderT, asks )
import Control.Monad.Trans ( MonadIO, liftIO )
import Data.Bson.Generic ( fromBSON, toBSON )
import Data.Maybe ( catMaybes )
import Data.String.Conv ( toS )
import Data.Text ( Text )
import Database.Mongo.Util ( lastStatus )
import Database.MongoDB ( Collection, Pipe, (=:), access, delete,
   deleteOne, find, insertMany_, modify, rest, select, slaveOk, sort )
import KS.Data.BSON ( separateId )
import Text.Printf ( printf )

import KS.Clean.Remove.Options ( RemoveOptions (optConfDir, optPlaceId) )
import KS.Data.Document ( Document (..) )
import qualified KS.Data.Feedback as F
import KS.Data.Place ( Place (name, place_id, vicinity) )
import KS.Database.Mongo.Util ( coll_feedback, coll_inspections_all,
   coll_inspections_archived, coll_inspections_recent, mongoConnect )
import KS.Log ( line )


data AppConfig = AppConfig
   { acPipe :: Pipe
   , acDatabase :: Text
   , acOptions :: RemoveOptions
   }

type RemoveApp = ReaderT AppConfig IO

runRemoveApp :: AppConfig -> RemoveApp a -> IO a
runRemoveApp env act = runReaderT act env


run :: RemoveOptions -> IO ()
run options = do
   (pipe, database) <- mongoConnect (putStrLn) . optConfDir $ options
   runRemoveApp (AppConfig pipe database options) $ do
      -- Retrieve and show everything for this Places ID
      anyAll <- retrieveDocs coll_inspections_all >>= displayDocs
      anyRecent <- retrieveDocs coll_inspections_recent >>= displayDocs
      anyFeedback <- retrieveFeedback >>= displayFeedback

      if (anyAll || anyRecent || anyFeedback)
         then do
            -- Prompt the user about removing it all
            deleting <- liftIO $ do
               putStr "\nArchive and delete these records in the database? (type 'yes' to confirm) "
               reply <- getLine
               putStrLn ""
               return $ if reply == "yes" then True else False

            -- Remove it all
            when deleting $ archiveEstablishment >> resolveFeedback
         else
            liftIO $ putStrLn "Nothing to do, exiting"


retrieveDocs :: Collection -> RemoveApp (Collection, [Document])
retrieveDocs coll = do
   pipe <- asks acPipe
   database <- asks acDatabase
   placeId <- asks (optPlaceId . acOptions)
   bdocs <- liftIO $ access pipe slaveOk database $ do
      rest =<< find (select [ "place.place_id" =: placeId ] coll)
         { sort = [ "inspection.date" =: (1 :: Int) ] }

   let docs = catMaybes . map (fromBSON . snd . separateId) $ bdocs
   return (coll, docs)


displayDocs :: (Collection, [Document]) -> RemoveApp Bool
displayDocs (coll, docs) = liftIO $ do
   printf "\n%s\nRecords from %s\n" line (toS coll :: String)
   mapM_ printBoth docs

   -- Return True if there are any documents
   return . not . null $ docs

   where
      printBoth doc = do
         putStrLn line
         print . inspection $ doc
         putStrLn . formatPlace . place $ doc

      formatPlace :: Place -> String
      formatPlace pl = printf "Place\n   %s | %s\n   %s"
         (toS . name $ pl :: String)
         (toS . place_id $ pl :: String)
         (toS . vicinity $ pl :: String)


retrieveFeedback :: RemoveApp [F.Feedback]
retrieveFeedback = do
   pipe <- asks acPipe
   database <- asks acDatabase
   placeId <- asks (optPlaceId . acOptions)
   liftIO $ access pipe slaveOk database $ do
      wholeDocs <- (rest =<< find (select
         (toBSON F.New ++ [ "place_id" =: placeId ])
         coll_feedback)
         { sort = [ "date" =: (1 :: Int) ] }
         )
      return $ catMaybes . map (fromBSON . snd . separateId) $ wholeDocs


displayFeedback :: [F.Feedback] -> RemoveApp Bool
displayFeedback fs = liftIO $ do
   printf "\n%s\nRecords from feedback\n" line
   mapM_ formatFeedback fs
   putStrLn ""

   -- Return True if there are any documents
   return . not . null $ fs

   where
      formatFeedback fb = printf "%s\nFeedback\n   %s | %s\n   %d %s %s\n   %s"
         line
         (show . F.name $ fb) (show . F.place_id $ fb)
         (F.date fb) (show . F.issue_type $ fb) (F.device_id fb)
         (show . F.comment $ fb)
         


archiveEstablishment :: RemoveApp Bool
archiveEstablishment = do
   pipe <- asks acPipe
   database <- asks acDatabase
   placeId <- asks (optPlaceId . acOptions)
   liftIO $ access pipe slaveOk database $ do
      liftIO $ printf "\n%s\nArchiving Places ID: %s\n" line (toS placeId :: String)

      -- Find all in inspections_all with the target Places ID
      ds <- rest =<< find (select
         [ "place.place_id" =: placeId ]
         coll_inspections_all)

      -- Insert those records into inspections_archive
      insertMany_ coll_inspections_archived ds
      insertionStatus <- lastStatus

      case insertionStatus of
         Left msg -> do
            liftIO $ printf "ERROR Something went wrong with the insertion, aborting without removing anything!\n%s\n" msg
            return False
         Right _ -> do
            liftIO $ printf "Inspection documents successfully inserted into %s\n" (toS coll_inspections_archived :: String)

            -- Remove from inspections_all with the target Places ID
            delete (select
               [ "place.place_id" =: placeId ]
               coll_inspections_all)
            lastStatus >>= displayStatus ("Removed documents from " ++ (toS coll_inspections_all :: String))

            -- Remove from inspections_recent with the target Places ID
            deleteOne (select
               [ "place.place_id" =: placeId ]
               coll_inspections_recent)
            lastStatus >>= displayStatus ("Removed document from " ++ (toS coll_inspections_recent :: String))

            return True


resolveFeedback :: RemoveApp ()
resolveFeedback = do
   -- Set feedback status to Resolved
   pipe <- asks acPipe
   database <- asks acDatabase
   placeId <- asks (optPlaceId . acOptions)
   liftIO $ access pipe slaveOk database $ do
      modify
         (select (toBSON F.New ++ [ "place_id" =: placeId ]) coll_feedback)
         [ "$set" =: (toBSON F.Resolved) ]
      lastStatus >>= displayStatus "Result of setting Feedback record status"


displayStatus :: (MonadIO m) => String -> Either String String -> m ()
displayStatus prefix e = either displayMsg displayMsg e
   where displayMsg msg = liftIO $ printf "%s: %s\n" prefix msg
