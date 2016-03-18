-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

{-# LANGUAGE OverloadedStrings #-}

{- This is for constructing and inserting regional statistics data
   for KitchenSnitch into MongoDB
-}

--import Control.Monad ( forM )
import Control.Monad.State
--import Control.Monad.Trans ( liftIO )
--import Data.Aeson ( decodeStrict )
--import Data.Aeson.Bson ( toBson )
import qualified Data.Bson as BSON
import Data.Bson.Generic ( fromBSON )
--import qualified Data.ByteString.Char8 as B
import Data.Maybe ( fromJust )
import qualified Data.Text as T
import Data.Time
import Data.Time.Clock.POSIX
--import Data.Time.Format ( defaultTimeLocale )
import Data.Version ( showVersion )
import Database.MongoDB hiding ( options )
--import KS.Data.Common ( utcTimeToEpoch )
import KS.Data.BSON ( combineId, separateId )
import qualified KS.Data.Document as D
import qualified KS.Data.Inspection as I
import qualified KS.Data.Place as P
import Paths_ks_download ( version )
import System.Environment ( getArgs )
--import System.Exit ( ExitCode (..), exitFailure, exitSuccess, exitWith )
import System.Exit ( ExitCode (..), exitWith )
import System.IO
   ( BufferMode ( NoBuffering )
   , hSetBuffering, stdout, stderr
   )
import Text.Printf ( printf )
import Text.Regex ( mkRegex, subRegex )

import qualified KS.Database.Mongo.Config as MC
--import KS.Database.Mongo.Util ( parseLastError )


--coll_regional_data :: Collection

--coll_regional_data = "regional_data"
--coll_regional_data = "test_regional_data"  -- For development


coll_inspections :: Collection
coll_inspections = "inspections_datefix"


main :: IO ()
main = do
   -- No buffering, it messes with the order of output
   mapM_ (flip hSetBuffering NoBuffering) [ stdout, stderr ]

   (confDir : _) <- getArgs

   printf "ks-datefix version %s started\n" (showVersion version)

   mongoConf <- MC.loadMongoConfig confDir

   -- Get a connection to Mongo, they call it a 'pipe'
   pipe <- connect . host . MC.ip $ mongoConf

   -- Authenticate with mongo, show the auth state on stdout
   (access pipe slaveOk (MC.database mongoConf)
      $ auth (MC.username mongoConf) (MC.password mongoConf)) >>=
      \tf -> putStrLn $ "Authenticated with Mongo: " ++ (show tf)
   result <- updateRecords mongoConf pipe

   close pipe

   exitWith . toExitCode $ result


type Fix a = (StateT D.Document IO) a

runFix :: D.Document -> Fix a -> IO a
runFix st ev = evalStateT ev st


updateRecords :: MC.MongoConfig -> Pipe -> IO Bool
updateRecords mc pipe = do
   rs <- access pipe slaveOk (MC.database mc) $ rest =<<
      find (select [] coll_inspections)
         { sort = [ "inspection.date" =: (1 :: Int) ]
         -- , limit = (50 :: Limit)
         }

   --let ds = (map (fromJust . fromBSON) $ take 50 rs) :: [D.Document]
   let ds = (map (fromJust . fromBSON) rs) :: [D.Document]
   --print ds
   mapM_ updateRecord ds
   {-
   _ <- forM ds $ (\d -> do
      --let placeID = P.place_id . D.place $ d
      let detail = I.detail . D.inspection $ d

      let ut = posixSecondsToUTCTime . realToFrac . I.date . D.inspection $ d
      date <- formatTime defaultTimeLocale "%FT%T%z" <$> utcToLocalZonedTime ut

      --printf "%s %s\n" (T.unpack placeID) date
      --printf "%s  %s\n" date detail
      printf "<tr><td>%s</td><td><a href=\"%s\">%s</a></td></tr>\n" date detail detail
      )
   -}
      

   return True


{-
updateRecord :: D.Document -> IO ()
updateRecord oldDoc = runFix oldDoc $ do
   display
   fixHost
   fixDate
   saveInspection oldDoc
-}
--updateRecord :: D.Document -> IO ()
updateRecord oldDoc = runFix oldDoc $ do
   display
   fixHost
   fixDate
   saveInspection oldDoc


display :: Fix ()
display = do
   doc <- get
   let placeID = P.place_id . D.place $ doc
   let ut = posixSecondsToUTCTime . realToFrac . I.date . D.inspection $ doc
   liftIO $ do
      date <- formatTime defaultTimeLocale "%FT%T%z" <$> utcToLocalZonedTime ut
      printf "----------\nInspection %s %s\n" (T.unpack placeID) date


fixHost :: Fix ()
fixHost = do
   oldDoc <- get
   let oldUrl = I.detail . D.inspection $ oldDoc
   let newUrl = subRegex (mkRegex "(.*)wake\\.digitalhealthdepartment\\.com(.*)")
         oldUrl "\\1wake-nc.healthinspections.us\\2"

   when (oldUrl /= newUrl) $ do
      let newInsp = (D.inspection oldDoc) { I.detail = newUrl }
      let newDoc = oldDoc { D.inspection = newInsp }

      put newDoc
      liftIO $ putStrLn "  Fixed host"

   return ()


fixDate :: Fix ()
fixDate = do
   oldDoc <- get

   let ut = posixSecondsToUTCTime . realToFrac . I.date . D.inspection $ oldDoc
   zt <- liftIO $ utcToLocalZonedTime ut

   let oldHour = todHour . localTimeOfDay . zonedTimeToLocalTime $ zt

   when (oldHour /= 0) $ do
      let nextDay = addDays 1 . localDay . zonedTimeToLocalTime $ zt
      let newZT = ZonedTime (LocalTime nextDay midnight) (zonedTimeZone zt)
      let newUT = zonedTimeToUTC newZT
      let newEpoch = round . utcTimeToPOSIXSeconds $ newUT

      let newInsp = (D.inspection oldDoc) { I.date = newEpoch }
      let newDoc = oldDoc { D.inspection = newInsp }

      put newDoc
      date <- liftIO $ formatTime defaultTimeLocale "%FT%T%z" <$> utcToLocalZonedTime newUT
      liftIO $ printf "  Fixed date: %s\n" date

   return ()


saveInspection :: D.Document -> Fix ()
saveInspection oldDoc = do
   newDoc <- get
   when (oldDoc /= newDoc) $ do
      liftIO $ putStrLn "  Document changed, updating"
      -- write to mongo now


{-
   -- Get the stats for all regions in recent_inspections
   computedStats <- access pipe slaveOk (MC.database mc) $ aggregate
      "recent_inspections" [mkStatsQuery]

   -- Get the date right now
   now <- utcTimeToEpoch <$> getCurrentTime

   -- Construct the regional_stats documents
   newDocs <- mapM (mkRegionalStats now confDir) computedStats

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


{-
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


mkRegionalStats :: Int -> FilePath -> Document -> IO Document
mkRegionalStats now confDir stats = do
   let source = "_id" `at` stats
   sourceConfig <- loadConfig confDir source
   let (county : state : _) =
         fromJust
         . matchRegex (mkRegex "^(.+) County, (.+)$")
         . T.unpack
         $ displayName sourceConfig

   return $
      [ "source" =: source
      , "doctype" =: ("regional_stats" :: T.Text)
      , "date" =: now
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
-}


toExitCode :: Bool -> ExitCode
toExitCode True  = ExitSuccess
toExitCode False = ExitFailure 1
