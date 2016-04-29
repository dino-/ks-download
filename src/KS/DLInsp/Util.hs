-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

{-# LANGUAGE OverloadedStrings #-}

module KS.DLInsp.Util ( setDates ) where

import Control.Monad ( liftM2, mplus )
import Control.Monad.Trans ( liftIO )
import Data.Maybe ( listToMaybe )
import Data.Time ( Day, getCurrentTimeZone )
import Database.MongoDB ( Host (..), Limit, PortID (PortNumber), (=:),
   access, at, auth, connect, find, limit, rest, select, slaveOk, sort )

import KS.Database.Mongo.Config as MC
import KS.Database.Mongo.Util ( coll_inspections_recent )
import KS.Util ( dateIntToDay, today, twoDaysAgo )


setDates :: (FilePath, String) -> Maybe Day -> Maybe Day
   -> IO (Maybe Day, Maybe Day)
setDates confDirSrc oldStartDate oldEndDate = do
   tz <- getCurrentTimeZone

   newStartDate <- firstSuccess [return oldStartDate
      , lastDateFromDB confDirSrc, (Just <$> twoDaysAgo tz)]
   newEndDate   <- firstSuccess [return oldEndDate, (Just <$> today tz)]

   return (newStartDate, newEndDate)

   where
      firstSuccess :: [IO (Maybe Day)] -> IO (Maybe Day)
      firstSuccess = foldl (liftM2 mplus) (return Nothing)


lastDateFromDB :: (FilePath, String) -> IO (Maybe Day)
lastDateFromDB (confDir, source) = do
   mongoConf <- MC.loadMongoConfig confDir

   pipe <- connect $ Host (MC.ip mongoConf)
      (PortNumber . fromIntegral . MC.port $ mongoConf)

   access pipe slaveOk (MC.database mongoConf) $ do
      auth (MC.username mongoConf) (MC.password mongoConf) >>=
         \tf -> liftIO $ putStrLn $ "Authenticated with Mongo: " ++ (show tf)

      matchingDocuments <- rest =<< find (select
         [ "inspection.inspection_source" =: source] coll_inspections_recent)
         { sort = [ "inspection.date" =: (-1 :: Int) ]
         , limit = (1 :: Limit)
         }

      return $
         dateIntToDay <$>  -- ..convert the Int into a Day
         (listToMaybe .  -- ..just the first one if present..
         map (\doc -> "date" `at` ("inspection" `at` doc)) $  -- ..extract the dates..
         matchingDocuments)  -- Documents from the db..
