-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

{-# LANGUAGE OverloadedStrings #-}

{- This is for constructing and inserting regional statistics data
   for KitchenSnitch into MongoDB
-}

import Control.Arrow ( (***) )
import Control.Monad.State
import qualified Data.Bson as BSON
import Data.Bson.Generic ( fromBSON, toBSON )
import Data.Maybe ( fromJust )
import qualified Data.Text as T
import Data.Time
import Data.Time.Clock.POSIX
import Data.Version ( showVersion )
import Database.MongoDB hiding ( options )
import KS.Data.BSON ( combineId, separateId )
import qualified KS.Data.Document as D
import qualified KS.Data.Inspection as I
import qualified KS.Data.Place as P
import Paths_ks_download ( version )
import System.Environment ( getArgs )
import System.Exit ( ExitCode (..), exitWith )
import System.IO
   ( BufferMode ( NoBuffering )
   , hSetBuffering, stdout, stderr
   )
import Text.Printf ( printf )
import Text.Regex ( mkRegex, subRegex )

import qualified KS.Database.Mongo.Config as MC


coll_inspections :: Collection
coll_inspections = "inspections_datefix"
--coll_inspections = "inspections"
--coll_inspections = "recent_inspections"


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


type Fix a = (StateT (BSON.Document, D.Document) IO) a

runFix :: (BSON.Document, D.Document) -> Fix (BSON.Document, D.Document)
   -> IO (BSON.Document, D.Document)
runFix st ev = execStateT ev st


updateRecords :: MC.MongoConfig -> Pipe -> IO Bool
updateRecords mc pipe = do
   rs <- access pipe slaveOk (MC.database mc) $ rest =<<
      find (select [] coll_inspections)
         { sort = [ "inspection.date" =: (1 :: Int) ]
         -- , limit = (50 :: Limit)
         }

   -- Take the _id apart and deserialize the rest into a KS Document
   let ts = map (\r -> (id *** (fromJust . fromBSON)) $ separateId r) rs

   _ <- forM ts $ (\t -> do
         (_id, newDoc) <- updateRecord t
         when (snd t /= newDoc) $ do
            liftIO $ putStrLn "  Document changed, updating"

            -- write to mongo now
            let newBSON = combineId (_id, toBSON newDoc)
            access pipe slaveOk (MC.database mc) $
               save coll_inspections newBSON
      )

   return True


updateRecord :: (BSON.Document, D.Document) -> IO (BSON.Document, D.Document)
updateRecord oldDocT = runFix oldDocT $ do
   display
   fixHost
   --fixDate
   upgradeDate
   return =<< get


updateSnd :: D.Document -> Fix ()
updateSnd d = do
   (i, _) <- get
   put (i, d)


display :: Fix ()
display = do
   doc <- gets snd
   let placeID = P.place_id . D.place $ doc
   let ut = posixSecondsToUTCTime . realToFrac . I.date . D.inspection $ doc
   let placeName = P.name . D.place $ doc
   let detail = I.detail . D.inspection $ doc
   liftIO $ do
      date <- formatTime defaultTimeLocale "%FT%T%z" <$> utcToLocalZonedTime ut
      printf "----------\nInspection %s %s %s\n  %s\n" (T.unpack placeID) date
         (T.unpack placeName) detail


fixHost :: Fix ()
fixHost = do
   oldDoc <- gets snd
   let oldUrl = I.detail . D.inspection $ oldDoc
   let newUrl = subRegex (mkRegex "(.*)wake\\.digitalhealthdepartment\\.com(.*)")
         oldUrl "\\1wake-nc.healthinspections.us\\2"

   when (oldUrl /= newUrl) $ do
      let newInsp = (D.inspection oldDoc) { I.detail = newUrl }
      let newDoc = oldDoc { D.inspection = newInsp }

      updateSnd newDoc
      liftIO $ putStrLn "  Fixed host"

   return ()


fixDate :: Fix ()
fixDate = do
   oldDoc <- gets snd

   let ut = posixSecondsToUTCTime . realToFrac . I.date . D.inspection $ oldDoc
   zt <- liftIO $ utcToLocalZonedTime ut

   let oldHour = todHour . localTimeOfDay . zonedTimeToLocalTime $ zt

   when (oldHour /= 0 && oldHour /= 1) $ do
      let nextDay = addDays 1 . localDay . zonedTimeToLocalTime $ zt
      let newZT = ZonedTime (LocalTime nextDay midnight) (zonedTimeZone zt)
      let newUT = zonedTimeToUTC newZT
      let newEpoch = round . utcTimeToPOSIXSeconds $ newUT

      let newInsp = (D.inspection oldDoc) { I.date = newEpoch }
      let newDoc = oldDoc { D.inspection = newInsp }

      updateSnd newDoc
      date <- liftIO $ formatTime defaultTimeLocale "%FT%T%z" <$> utcToLocalZonedTime newUT
      liftIO $ printf "  Fixed date: %s\n" date

   return ()


upgradeDate :: Fix ()
upgradeDate = do
   oldDoc <- gets snd

   let ut = posixSecondsToUTCTime . realToFrac . I.date . D.inspection $ oldDoc
   zt <- liftIO $ utcToLocalZonedTime ut
   let (y, m, d) = toGregorian . localDay . zonedTimeToLocalTime $ zt
   let newDate = read $ printf "%d%02d%02d" y m d

   let newInsp = (D.inspection oldDoc) { I.date = newDate }
   let newDoc = oldDoc { D.inspection = newInsp }

   updateSnd newDoc
   liftIO $ printf "  Upgraded date: %s\n" (show newDate)

   return ()


toExitCode :: Bool -> ExitCode
toExitCode True  = ExitSuccess
toExitCode False = ExitFailure 1
