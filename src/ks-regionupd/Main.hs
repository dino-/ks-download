-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

{-# LANGUAGE OverloadedStrings #-}

{- This is for constructing and inserting regional statistics data
   for KitchenSnitch into MongoDB
-}

import Control.Exception.Safe (Exception, throwM)
import Control.Monad ( when )
import Control.Monad.Trans ( liftIO )
import Data.Aeson ( decodeStrict )
import Data.Aeson.Bson ( toBson )
import qualified Data.ByteString.Char8 as B
import Data.Maybe ( fromJust )
import qualified Data.Text as T
import Data.Time ( getCurrentTime, utcToLocalZonedTime, zonedTimeToLocalTime )
import Data.Time.Calendar ( toGregorian )
import Data.Time.LocalTime ( LocalTime (localDay) )
import Data.Typeable ( Typeable )
import Data.Version ( showVersion )
import Database.Mongo.Util ( lastStatus )
import Database.MongoDB hiding ( options )
import Paths_ks_download ( version )
import System.Environment ( getArgs, setEnv )
import System.Exit ( ExitCode (..), exitFailure, exitSuccess, exitWith )
import System.IO
   ( BufferMode ( NoBuffering )
   , hSetBuffering, stdout, stderr
   )
import Text.Printf ( printf )
import Text.Regex ( matchRegex, mkRegex )

import KS.Database.Mongo.Util
   ( coll_inspections_recent, coll_stats_recent, mongoConnect )
import KS.Log
import KS.RegionUpd.Opts
import KS.SourceConfig
   ( SourceConfig (centroid, displayName, timeZone), loadConfig )


main :: IO ()
main = do
   -- No buffering, it messes with the order of output
   mapM_ (flip hSetBuffering NoBuffering) [ stdout, stderr ]

   (options, args) <- getArgs >>= parseOpts
   when (optHelp options) $ putStrLn usageText >> exitSuccess
   confDir <- case args of
      (c : _) -> return c
      _       -> putStrLn usageText >> exitFailure

   initLogging $ optLogPriority options
   noticeM lname line
   noticeM lname $
      printf "ks-regionupd version %s started" (showVersion version)
   logStartMsg lname

   conn@(pipe, _) <- mongoConnect (noticeM lname) confDir

   result <- updateRegions confDir conn

   close pipe

   logStopMsg lname

   exitWith . toExitCode $ result


updateRegions :: FilePath -> (Pipe, T.Text) -> IO Bool
updateRegions confDir conn@(pipe, database) = do
   -- Get the stats for all regions
   computedStats <- access pipe slaveOk database $ aggregate
      coll_inspections_recent [mkStatsQuery]

   -- Construct the regional_stats documents
   newDocs <- mapM (mkRegionalStats confDir) computedStats

   -- Report what we're about to do
   infoM lname $ printf "Inserting these stats into the %s collection:"
      (T.unpack coll_stats_recent)
   mapM_ (infoM lname . show) newDocs

   -- Upsert them into the regional_data collection
   statsResults <- liftIO $ mapM (updateStatsDocument conn) newDocs

   -- If this is the first of the month,
   -- insert the documents into regional_data_history as well
   -- historyResults <- ...

   return $ all (== True) statsResults


updateStatsDocument :: (Pipe, T.Text) -> Document -> IO Bool
updateStatsDocument (pipe, database) doc = do
   result <- access pipe slaveOk database $ do
      upsert (select [ "source" =: (("source" `at` doc) :: T.Text) ]
         coll_stats_recent) doc
      lastStatus

   either
      (\e -> errorM lname e >> return False)
      (\m -> noticeM lname m >> return True)
      result


data CountyParseE = CountyParseE
  deriving Typeable
instance Show CountyParseE where
  show CountyParseE = "Error parsing county and state"
instance Exception CountyParseE


mkRegionalStats :: FilePath -> Document -> IO Document
mkRegionalStats confDir stats = do
   let source = "_id" `at` stats
   sourceConfig <- loadConfig confDir source

   -- Get today's date
   setEnv "TZ" $ timeZone sourceConfig
   (y, m, d) <- toGregorian . localDay . zonedTimeToLocalTime
      <$> (utcToLocalZonedTime =<< getCurrentTime)
   let today = read $ printf "%d%02d%02d" y m d

   let displayName' = T.unpack . displayName $ sourceConfig
   (county, state) <- case (matchRegex (mkRegex "^(.+) County, (.+)$") displayName') of
      Just (c : s : _) -> return (c, s)
      _                -> throwM CountyParseE

   return $
      [ "source" =: source
      , "doctype" =: ("regional_stats" :: T.Text)
      , "date" =: (today :: Int)
      , "location" =: centroid sourceConfig
      , "display_name" =: displayName sourceConfig
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
