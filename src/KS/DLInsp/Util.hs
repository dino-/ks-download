-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

{-# LANGUAGE OverloadedStrings #-}

module KS.DLInsp.Util ( setDates ) where

import Control.Monad ( liftM2, mplus )
import Data.Maybe ( listToMaybe )
import Data.Time ( Day, getCurrentTimeZone )
import Database.MongoDB ( Limit, (=:), access, at, find, limit,
   rest, select, slaveOk, sort )

import KS.Database.Mongo.Util ( coll_inspections_recent, mongoConnect )
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
   (pipe, database) <- mongoConnect putStrLn confDir

   access pipe slaveOk database $ do
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
