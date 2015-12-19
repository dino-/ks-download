-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

{-# LANGUAGE OverloadedStrings #-}

{- This is for inserting inspections into MongoDB
-}

import Control.Monad ( when )
import Control.Monad.Trans ( liftIO )
--import Data.Aeson ( Object, FromJSON, decodeStrict, parseJSON )
import Data.Aeson ( decodeStrict, encode )
import Data.Aeson.Bson ( aesonifyValue, toAeson, toBson )
--import Data.Aeson.Bson ( toBson )
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as BL
--import Data.Either ( isLeft )
--import Data.List ( isPrefixOf )
import Data.Time ( UTCTime, getCurrentTime )
import Data.Time.Clock.POSIX ( utcTimeToPOSIXSeconds )
import Data.Maybe ( fromJust )
import qualified Data.Text as T
import Database.MongoDB hiding ( options )
--import System.Directory ( doesFileExist, getDirectoryContents )
import System.Environment ( getArgs )
import System.Exit ( ExitCode (..), exitSuccess, exitWith )
--import System.FilePath
import System.IO
   ( BufferMode ( NoBuffering )
   , hSetBuffering, stdout, stderr
   )
import System.IO.Error ( tryIOError )
--import Text.Printf ( printf )

import KS.Data.BSON ( docToBSON )
--import qualified KS.Data.Document as D
--import qualified KS.Data.Place as P
import qualified KS.Database.Mongo.Config as MC
import KS.Database.Mongo.Util ( parseLastError )
import KS.Log
import KS.RegionUpd.Opts
import qualified KS.RegionUpd.Region as R


main :: IO ()
main = do
   -- No buffering, it messes with the order of output
   mapM_ (flip hSetBuffering NoBuffering) [ stdout, stderr ]

   (options, _) <- getArgs >>= parseOpts
   when (optHelp options) $ putStrLn usageText >> exitSuccess

   initLogging $ optLogPriority options
   noticeM lname line
   logStartMsg lname

   mongoConf <- MC.loadMongoConfig $ optConfDir options

   -- Get a connection to Mongo, they call it a 'pipe'
   pipe <- connect . host . MC.ip $ mongoConf

   -- Authenticate with mongo, show the auth state on stdout
   (access pipe UnconfirmedWrites (MC.database mongoConf)
      $ auth (MC.username mongoConf) (MC.password mongoConf)) >>=
      \tf -> putStrLn $ "Authenticated with Mongo: " ++ (show tf)

   _ <- updateRegions mongoConf pipe
   --print regions
   --mapM_ print regions
   --mapM_ (debugM lname . show) regions

   {-
   exitCode <- do
      failures <- mapM (loadAndInsert config pipe) files
      if any (== True) failures
         then return $ ExitFailure 1
         else return ExitSuccess
   -}

   close pipe

   logStopMsg lname

   --exitWith exitCode


{-
updateRegions :: MC.MongoConfig -> Pipe -> IO Bool
updateRegions mc pipe = do
   -- Retrieve all region info (the state and county info)
   -- regionDocs :: [Data.Bson.Document]
   regionDocs <- access pipe slaveOk (MC.database mc) $ rest =<<
      find (select [] "regions")
   let aesonValues = map aesonifyValue regionDocs
   let as = ??  -- [Data.Aeson.Types.Object]
   let regions = ??  -- [R.Region]
   --let regions = ((map (parseJSON . toAeson) regionDocs) :: [R.Region])

   -- Get the stats for all regions in recent_inspections
   computedStats <- access pipe slaveOk (MC.database mc) $ aggregate
      "recent_inspections" [mkStatsQuery]
   debugM lname $ show computedStats

   -- Get the date right now
   now <- getCurrentTime

   -- Construct the regional_stats documents
   --let newDocs = map (mkRegionalStats now regionDocs) computedStats
   let newDocs = map (mkRegionalStats now regions) computedStats
   mapM_ (debugM lname . show) newDocs

   -- Upsert them into the regional_data collection

   -- If this is the first of the month,
   -- insert the documents into regional_data_history as well

   return False
-}
updateRegions :: MC.MongoConfig -> Pipe -> IO Bool
updateRegions mc pipe = do
   -- Retrieve all region info (the state and county info)
   regionDocs <- access pipe slaveOk (MC.database mc) $ rest =<<
      find (select [] "regions")

   -- Get the stats for all regions in recent_inspections
   computedStats <- access pipe slaveOk (MC.database mc) $ aggregate
      "recent_inspections" [mkStatsQuery]
   --debugM lname "computedStats:"
   --debugM lname $ show computedStats

   -- Get the date right now
   now <- getCurrentTime

   -- Construct the regional_stats documents
   let newDocs = map (mkRegionalStats now regionDocs) computedStats
   debugM lname "newDocs:"
   mapM_ (debugM lname . show) newDocs

   --let jsStrings = map (encode . toAeson) newDocs
   --mapM_ BL.putStrLn jsStrings

   -- Upsert them into the regional_data collection
   results <- liftIO $ mapM (updateStatsDocument mc pipe) newDocs
   return $ any (== False) results

   -- If this is the first of the month,
   -- insert the documents into regional_data_history as well

   --return False


updateStatsDocument :: MC.MongoConfig -> Pipe -> Document -> IO Bool
updateStatsDocument mc pipe doc = do
   result <- access pipe slaveOk (MC.database mc) $ do
      upsert (select [ "source" =: (("source" `at` doc) :: T.Text) ]
         "regional_data") doc
      parseLastError <$> runCommand [ "getLastError" =: (1::Int) ]

   either
      (\e -> errorM lname e >> return False)
      (\m -> noticeM lname m >> return True)
      result


{- FIXME This is here as an example for doing save and upsert

loadAndInsert :: C.Config -> Pipe -> FilePath -> IO Bool
loadAndInsert config pipe path = do
   edoc <- tryIOError $ D.loadDocument path

   result <- case edoc of
      Left ex   -> return . Left $ show ex
      Right doc ->
         access pipe UnconfirmedWrites (C.mongoDatabase config) $ do
            -- Insert the inspection into the all_inspections collection
            save "inspections" $ docToBSON doc
            sr <- parseLastError `fmap` runCommand [ "getLastError" =: (1::Int) ]

            -- Insert or modify the one document in recent_inspections
            upsert (select ["place.place_id" =: (P.place_id . D.place $ doc)]
               "recent_inspections") $ docToBSON doc
            ur <- parseLastError `fmap` runCommand [ "getLastError" =: (1::Int) ]

            -- Combine the results
            return $ sr >> ur

   printf "%s %s\n" path (either id id $ result)

   return . isLeft $ result
-}


{-
mkRegionalStats :: UTCTime -> [R.Region] -> Document -> Document
mkRegionalStats now regions stats =
   [ "source" =: region
   , "doctype" =: ("regional_stats" :: T.Text)
   , "date" =: now
   , "state" =: (R.state regionInfo)
   , "county" =: (R.county regionInfo)
   , "count_total" =: (("count_total" `at` stats) :: Int)
   , "count_a1" =: (("count_a1" `at` stats) :: Int)
   , "count_a2" =: (("count_a2" `at` stats) :: Int)
   , "count_a3" =: (("count_a3" `at` stats) :: Int)
   , "count_a4" =: (("count_a4" `at` stats) :: Int)
   , "count_b" =: (("count_b" `at` stats) :: Int)
   , "count_c" =: (("count_c" `at` stats) :: Int)
   , "min_score" =: (("min_score" `at` stats) :: Float)
   , "max_score" =: (("max_score" `at` stats) :: Float)
   , "avg_score" =: (("avg_score" `at` stats) :: Float)
   ]

   where
      region :: T.Text
      region = "_id" `at` stats

      --(state, county) = (R.state regionInfo, R.county regionInfo)

      regionInfo = lookupRegion regions region
-}
mkRegionalStats :: UTCTime -> [Document] -> Document -> Document
mkRegionalStats now regionDocs stats =
   [ "source" =: region
   , "doctype" =: ("regional_stats" :: T.Text)
   -- Manually convert this UTCTime into epoch as Int
   , "date" =: ((round . utcTimeToPOSIXSeconds $ now) :: Int)
   , "state" =: (state :: T.Text)
   , "county" =: (county :: T.Text)
   , "count_total" =: (("count_total" `at` stats) :: Int)
   , "count_a1" =: (("count_a1" `at` stats) :: Int)
   , "count_a2" =: (("count_a2" `at` stats) :: Int)
   , "count_a3" =: (("count_a3" `at` stats) :: Int)
   , "count_a4" =: (("count_a4" `at` stats) :: Int)
   , "count_b" =: (("count_b" `at` stats) :: Int)
   , "count_c" =: (("count_c" `at` stats) :: Int)
   , "min_score" =: (("min_score" `at` stats) :: Float)
   , "max_score" =: (("max_score" `at` stats) :: Float)
   , "avg_score" =: (("avg_score" `at` stats) :: Float)
   ]

   where
      region :: T.Text
      region = "_id" `at` stats

      (state, county) = ("state" `at` regionInfo, "county" `at` regionInfo)

      regionInfo = lookupRegion regionDocs region


{-
lookupRegion :: [R.Region] -> T.Text -> R.Region
lookupRegion (r : rs) region
   | region == R._id r = r
   | otherwise = lookupRegion rs region
lookupRegion [] region = error $ "No region found for: " ++ (T.unpack region)
-}
lookupRegion :: [Document] -> T.Text -> Document
lookupRegion (d : ds) region
   | region == "_id" `at` d = d
   | otherwise = lookupRegion ds region
lookupRegion [] region = error $ "No region found for: " ++ (T.unpack region)


mkStatsQuery :: Document
mkStatsQuery = toBson . fromJust . decodeStrict . B.pack . unlines $
   [ "   {  \"$group\":"
   , "         {  \"_id\": \"$inspection.inspection_source\""
   , "         , \"min_score\": { \"$min\": \"$inspection.score\" }"
   , "         , \"max_score\": { \"$max\": \"$inspection.score\" }"
   , "         , \"avg_score\": { \"$avg\": \"$inspection.score\" }"
   , "         , \"count_total\": { \"$sum\": 1 }"
   , "         , \"count_a4\": { \"$sum\": { \"$cond\":"
   , "            [ { \"$gte\": [\"$inspection.score\", 97.5] }"
   , "            , 1"
   , "            , 0"
   , "            ] } }"
   , "         , \"count_a3\": { \"$sum\": { \"$cond\":"
   , "            [ { \"$and\":"
   , "               [ { \"$gte\": [\"$inspection.score\", 95.0] }"
   , "               , { \"$lt\": [\"$inspection.score\", 97.5] }"
   , "               ] }"
   , "            , 1"
   , "            , 0"
   , "            ] } }"
   , "         , \"count_a2\": { \"$sum\": { \"$cond\":"
   , "            [ { \"$and\":"
   , "               [ { \"$gte\": [\"$inspection.score\", 92.5] }"
   , "               , { \"$lt\": [\"$inspection.score\", 95.0] }"
   , "               ] }"
   , "            , 1"
   , "            , 0"
   , "            ] } }"
   , "         , \"count_a1\": { \"$sum\": { \"$cond\":"
   , "            [ { \"$and\":"
   , "               [ { \"$gte\": [\"$inspection.score\", 90.0] }"
   , "               , { \"$lt\": [\"$inspection.score\", 92.5] }"
   , "               ] }"
   , "            , 1"
   , "            , 0"
   , "            ] } }"
   , "         , \"count_b\": { \"$sum\": { \"$cond\":"
   , "            [ { \"$and\":"
   , "               [ { \"$gte\": [\"$inspection.score\", 80.0] }"
   , "               , { \"$lt\": [\"$inspection.score\", 90.0] }"
   , "               ] }"
   , "            , 1"
   , "            , 0"
   , "            ] } }"
   , "         , \"count_c\": { \"$sum\": { \"$cond\":"
   , "            [ { \"$lt\": [\"$inspection.score\", 80.0] }"
   , "            , 1"
   , "            , 0"
   , "            ] } }"
   , "         }"
   , "   }"
   ]
