-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

{-# LANGUAGE OverloadedStrings #-}

{- This is for constructing and inserting regional statistics data
   for KitchenSnitch into MongoDB
-}

import Control.Monad ( when )
import Control.Monad.Trans ( liftIO )
import Data.Aeson ( decodeStrict )
import Data.Aeson.Bson ( toBson )
import qualified Data.ByteString.Char8 as B
import Data.Maybe ( fromJust )
import qualified Data.Text as T
import Data.Time ( getCurrentTime )
import Database.MongoDB hiding ( options )
import KS.Data.Common ( utcTimeToEpoch )
import System.Environment ( getArgs )
import System.Exit ( ExitCode (..), exitSuccess, exitWith )
import System.IO
   ( BufferMode ( NoBuffering )
   , hSetBuffering, stdout, stderr
   )
import Text.Printf ( printf )

import qualified KS.Database.Mongo.Config as MC
import KS.Database.Mongo.Util ( parseLastError )
import KS.Log
import KS.RegionUpd.Opts
import qualified KS.RegionUpd.Region as R
--import qualified KS.RegionUpd.RegionalStats as RS


coll_regional_data :: Collection

--coll_regional_data = "regional_data"
coll_regional_data = "test_regional_data"  -- For development


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
   (access pipe slaveOk (MC.database mongoConf)
      $ auth (MC.username mongoConf) (MC.password mongoConf)) >>=
      \tf -> noticeM lname $ "Authenticated with Mongo: " ++ (show tf)
   result <- updateRegions mongoConf pipe

   close pipe

   logStopMsg lname

   exitWith . toExitCode $ result


{-
updateRegions :: MC.MongoConfig -> Pipe -> IO Bool
updateRegions mc pipe = do
   -- Retrieve all region info (the state and county info)
   -- regionDocs :: [Data.Bson.Document]
   regionDocs <- access pipe slaveOk (MC.database mc) $ rest =<<
      find (select [] "regions")

   -- (failures, regions) :: ([String], [Region])
   let (failures, regions) = partitionEithers . map (resultToEither . fromBSON) $ regionDocs

   -- Report any failures to the log. This should NEVER happen!
   when (not $ null failures) $
      errorM lname "There were failures when retrieving the regions..."
   mapM_ (\e -> errorM lname e) failures

   -- Get the stats for all regions in recent_inspections
   computedStats <- access pipe slaveOk (MC.database mc) $ aggregate
      "recent_inspections" [mkStatsQuery]

   -- Get the date right now
   now <- utcTimeToEpoch <$> getCurrentTime

   -- Construct the regional_stats documents
   let newDocs = map (mkRegionalStats now regions) computedStats

   -- Report what we're about to do
   infoM lname $ printf "Inserting these stats into the %s collection:"
      (T.unpack coll_regional_data)
   mapM_ (infoM lname . show) newDocs

   -- Upsert them into the regional_data collection
   statsResults <- liftIO $ mapM (updateStatsDocument mc pipe) newDocs

   -- If this is the first of the month,
   -- insert the documents into regional_data_history as well
   -- historyResults <- ...

   return $ all (== True) statsResults
-}
updateRegions :: MC.MongoConfig -> Pipe -> IO Bool
updateRegions mc pipe = do
   -- Get the stats for all regions in recent_inspections
   computedStats <- access pipe slaveOk (MC.database mc) $ aggregate
      "recent_inspections" [mkStatsQuery]

   -- Get the date right now
   now <- utcTimeToEpoch <$> getCurrentTime

   -- Construct the regional_stats documents
   let newDocs = map (mkRegionalStats now R.regions) computedStats

   -- Report what we're about to do
   infoM lname $ printf "Inserting these stats into the %s collection:"
      (T.unpack coll_regional_data)
   mapM_ (infoM lname . show) newDocs

   -- Upsert them into the regional_data collection
   statsResults <- liftIO $ mapM (updateStatsDocument mc pipe) newDocs

   -- If this is the first of the month,
   -- insert the documents into regional_data_history as well
   -- historyResults <- ...

   return $ all (== True) statsResults


updateStatsDocument :: MC.MongoConfig -> Pipe -> Document -> IO Bool
updateStatsDocument mc pipe doc = do
   result <- access pipe slaveOk (MC.database mc) $ do
      upsert (select [ "source" =: (("source" `at` doc) :: T.Text) ]
         coll_regional_data) doc
      parseLastError <$> runCommand [ "getLastError" =: (1::Int) ]

   either
      (\e -> errorM lname e >> return False)
      (\m -> noticeM lname m >> return True)
      result
{-
updateStatsDocument :: MC.MongoConfig -> Pipe -> RS.RegionalStats -> IO Bool
updateStatsDocument mc pipe doc = do
   result <- access pipe slaveOk (MC.database mc) $ do
      upsert (select [ "source" =: (RS.source doc) ]
         coll_regional_data) (toBSON doc)
      parseLastError <$> runCommand [ "getLastError" =: (1::Int) ]

   either
      (\e -> errorM lname e >> return False)
      (\m -> noticeM lname m >> return True)
      result
-}


--mkRegionalStats :: Int -> [R.Region] -> Document -> Document
mkRegionalStats :: Int -> R.Regions -> Document -> Document
mkRegionalStats now regions stats =
   [ "source" =: region
   , "doctype" =: ("regional_stats" :: T.Text)
   , "date" =: now
   , "state" =: state
   , "county" =: county
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

      (state, county) = fromJust . R.lookup region $ regions
      --regionInfo = lookupRegion regions region
{-
mkRegionalStats :: Int -> [R.Region] -> Document -> RS.RegionalStats
mkRegionalStats now regions stats = RS.RegionalStats
   { RS.source = region
   , RS.doctype = "regional_stats"
   , RS.date = now
   , RS.state = (R.state regionInfo)
   , RS.county = (R.county regionInfo)
   , RS.count_total = ("count_total" `at` stats)
   , RS.count_a1 = ("count_a1" `at` stats)
   , RS.count_a2 = ("count_a2" `at` stats)
   , RS.count_a3 = ("count_a3" `at` stats)
   , RS.count_a4 = ("count_a4" `at` stats)
   , RS.count_b = ("count_b" `at` stats)
   , RS.count_c = ("count_c" `at` stats)
   , RS.min_score = ("min_score" `at` stats)
   , RS.max_score = ("max_score" `at` stats)
   , RS.avg_score = ("avg_score" `at` stats)
   }

   where
      region :: T.Text
      region = "_id" `at` stats

      regionInfo = lookupRegion regions region
-}


{-
lookupRegion :: [R.Region] -> T.Text -> R.Region
lookupRegion (r : rs) region
   | region == R._id r = r
   | otherwise = lookupRegion rs region
lookupRegion [] region = error $ "No region found for: " ++ (T.unpack region)
-}


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


toExitCode :: Bool -> ExitCode
toExitCode True  = ExitSuccess
toExitCode False = ExitFailure 1
