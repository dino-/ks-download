-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

{-# LANGUAGE OverloadedStrings #-}

--import Data.Aeson.Encode.Pretty hiding ( Config )
import Data.Bson.Generic ( fromBSON )
--import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Maybe ( fromJust, mapMaybe )
import Data.Version ( showVersion )
import Database.MongoDB ( Host (..), Pipe, PortID (PortNumber),
   (=:), access, auth, connect, find, rest, select, slaveOk, sort )
import Paths_ks_download ( version )
import System.Environment ( getArgs )
import System.Exit ( exitFailure, exitSuccess )
--import System.FilePath
import System.IO
   ( BufferMode ( NoBuffering )
   , hSetBuffering, stdout, stderr
   )
--import System.IO.Error
import Text.Printf ( printf )

import KS.Closed.Opts
import KS.Data.Document ( Document (..) )
--import KS.Data.Inspection
import qualified KS.Database.Mongo.Config as MC
import KS.Database.Mongo.Util ( coll_inspections_recent )
import KS.Locate.Config ( Config (logPriority), loadConfig )
import KS.Locate.Locate
--import KS.Locate.Places.Geocoding ( forwardLookup )
--import KS.Locate.Places.Match ( Match, match )
--import KS.Locate.Places.Places ( coordsToPlaces )
--import qualified KS.SourceConfig as SC
import KS.Log


main :: IO ()
main = do
   -- No buffering, it messes with the order of output
   mapM_ (flip hSetBuffering NoBuffering) [ stdout, stderr ]

   (options, args) <- getArgs >>= parseOpts
   when (optHelp options) $ putStrLn usageText >> exitSuccess
   when (length args < 1) $ putStrLn usageText >> exitFailure
   let (confDir : _) = args

   -- Load the config file
   config <- loadConfig confDir

   initLogging $ logPriority config
   noticeM lname $
      printf "ks-closed version %s started" (showVersion version)
   logStartMsg lname

   mongoConf <- MC.loadMongoConfig confDir

   -- Get a connection to Mongo, they call it a 'pipe'
   pipe <- connect $ Host (MC.ip mongoConf)
      (PortNumber . fromIntegral . MC.port $ mongoConf)

   -- Authenticate with mongo, show the auth state on stdout
   (access pipe slaveOk (MC.database mongoConf)
      $ auth (MC.username mongoConf) (MC.password mongoConf)) >>=
      \tf -> putStrLn $ "Authenticated with Mongo: " ++ (show tf)

   -- FIXME fromJust
   oldEsts <- getOldEstablishments mongoConf pipe (fromJust . optBeforeDate $ options)
   mapM_ print $ take 10 oldEsts
   print $ length oldEsts

   --noticeM lname line

   logStopMsg lname


getOldEstablishments :: MC.MongoConfig -> Pipe -> Int -> IO [Document]
getOldEstablishments mongoConf pipe day =
   access pipe slaveOk (MC.database mongoConf) $
      mapMaybe fromBSON <$> (rest =<< find (select
         [ "inspection.date" =: [ "$lte" =: day ] ]
         coll_inspections_recent)
         { sort = [ "inspection.date" =: (1 :: Int) ] }
         )


{-
lookupInspection :: Config -> Options -> FilePath -> FilePath -> IO ()
lookupInspection config options confDir srcPath = do
   r <- runKSDL (Env config SC.nullSourceConfig nullInspection) $ do
      liftIO $ noticeM lname line

      insp <- loadInspection' srcPath
      sc <- liftIO $ SC.loadConfig confDir $ inspection_source insp
      local (\r -> r { getSourceConfig = sc, getInspection = insp }) $ do
         geo <- forwardLookup
         places <- coordsToPlaces geo
         match places

   either (handleFailure) (outputDoc options srcPath . mkDoc) r

   where
      handleFailure (ErrMsg prio msg) = do
         -- Copy to FAILDIR if we have one
         maybe (return ()) (\failDir ->
            copyFile srcPath $ failDir </> takeFileName srcPath)
            $ optFailDir options

         -- Delete the original if we've been instructed to do so
         when (optDelete options) $ removeFile srcPath

         -- Log what happened
         logM lname prio msg

      mkDoc :: Match -> Document
      mkDoc (inspection', place') =
         Document "inspection" inspection' place'
-}


{-
outputDoc :: Options -> FilePath -> Document -> IO ()
outputDoc options srcPath doc = do
   r <- tryIOError $ case (optSuccessDir options) of
      Just successDir -> saveDocument successDir doc
      Nothing -> do
         BL.putStrLn $ encodePretty doc
         return ""
   
   case r of
      Left ex -> print ex
      Right destPath -> do
         when (optDelete options) $ removeFile srcPath
         printf "%s -> %s\n" srcPath destPath
-}
