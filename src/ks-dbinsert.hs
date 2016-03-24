-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

{-# LANGUAGE OverloadedStrings #-}

{- This is for inserting inspections into MongoDB
-}

import Control.Monad ( when )
import Data.Bson.Generic
import Data.Either ( isLeft )
import Data.List ( isPrefixOf )
import Data.Version ( showVersion )
import Database.MongoDB hiding ( options )
import Paths_ks_download ( version )
import System.Directory ( doesFileExist, getDirectoryContents )
import System.Environment ( getArgs )
import System.Exit ( ExitCode (..), exitFailure, exitSuccess, exitWith )
import System.FilePath
import System.IO
   ( BufferMode ( NoBuffering )
   , hSetBuffering, stdout, stderr
   )
import System.IO.Error ( tryIOError )
import Text.Printf ( printf )

import qualified KS.Data.Document as D
import qualified KS.Data.Inspection as I
import qualified KS.Data.Place as P
import qualified KS.Database.Mongo.Config as MC
import KS.Database.Mongo.Util
   ( coll_inspections_all, coll_inspections_recent, parseLastError )
import KS.DBInsert.Opts


main :: IO ()
main = do
   -- No buffering, it messes with the order of output
   mapM_ (flip hSetBuffering NoBuffering) [ stdout, stderr ]

   (options, args) <- getArgs >>= parseOpts
   when (optHelp options) $ do putStrLn usageText >> exitSuccess
   when (length args < 2) $ do putStrLn usageText >> exitFailure
   let (confDir : srcDirsOrFiles) = args

   putStrLn $ "ks-dbinsert version " ++ (showVersion version) ++ " started"

   mongoConf <- MC.loadMongoConfig confDir

   -- Paths to all files we'll be processing
   files <- concat <$> (sequence $ map buildFileList srcDirsOrFiles)

   -- Get a connection to Mongo, they call it a 'pipe'
   pipe <- connect . host . MC.ip $ mongoConf

   -- Authenticate with mongo, show the auth state on stdout
   (access pipe UnconfirmedWrites (MC.database mongoConf)
      $ auth (MC.username mongoConf) (MC.password mongoConf)) >>=
      \tf -> putStrLn $ "Authenticated with Mongo: " ++ (show tf)

   exitCode <- do
      failures <- mapM (loadAndInsert mongoConf pipe) files
      if any (== True) failures
         then return $ ExitFailure 1
         else return ExitSuccess

   close pipe

   exitWith exitCode


loadAndInsert :: MC.MongoConfig -> Pipe -> FilePath -> IO Bool
loadAndInsert mongoConf pipe path = do
   edoc <- tryIOError $ D.loadDocument path

   result <- case edoc of
      Left ex   -> return . Left $ show ex
      Right doc ->
         access pipe UnconfirmedWrites (MC.database mongoConf) $ do
            -- Convert our inspection data structure to BSON
            let bson = toBSON doc

            -- Insert into the collection where all inspections are stored
            -- Avoiding duplicates based on the Places ID and inspection date
            upsert (select
               [ "place.place_id" =: (P.place_id . D.place $ doc)
               , "inspection.date" =: (I.date . D.inspection $ doc)
               ]
               coll_inspections_all) bson
            sr <- parseLastError `fmap` runCommand [ "getLastError" =: (1::Int) ]

            -- Insert or modify the one document in recent_inspections
            upsert (select ["place.place_id" =: (P.place_id . D.place $ doc)]
               coll_inspections_recent) bson
            ur <- parseLastError `fmap` runCommand [ "getLastError" =: (1::Int) ]

            -- Combine the results
            return $ sr >> ur

   printf "%s %s\n" path (either id id $ result)

   return . isLeft $ result


buildFileList :: FilePath -> IO [FilePath]
buildFileList srcDirOrFile = do
   isFile <- doesFileExist srcDirOrFile
   if isFile then return [srcDirOrFile]
      else
         ( map (srcDirOrFile </>)                  -- ..relative paths
         . filter (not . isPrefixOf ".") )         -- ..minus dotfiles
         `fmap` getDirectoryContents srcDirOrFile  -- All files
