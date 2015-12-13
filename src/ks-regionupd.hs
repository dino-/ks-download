-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

{-# LANGUAGE OverloadedStrings #-}

{- This is for inserting inspections into MongoDB
-}

import Control.Monad ( when )
--import Data.Either ( isLeft )
--import Data.List ( isPrefixOf )
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
import qualified KS.Database.Mongo.Config as C
import KS.Database.Mongo.Util ( parseLastError )
import KS.RegionUpd.Opts


main :: IO ()
main = do
   -- No buffering, it messes with the order of output
   mapM_ (flip hSetBuffering NoBuffering) [ stdout, stderr ]

   (options, srcDirsOrFiles) <- getArgs >>= parseOpts
   when (optHelp options) $ putStrLn usageText >> exitSuccess

   config <- C.loadConfig $ optConfDir options

   print config

   {-
   -- Paths to all files we'll be processing
   files <- concat <$> (sequence $ map buildFileList srcDirsOrFiles)

   -- Get a connection to Mongo, they call it a 'pipe'
   pipe <- connect . host . C.mongoServerIP $ config

   -- Authenticate with mongo, show the auth state on stdout
   (access pipe UnconfirmedWrites (C.mongoDatabase config)
      $ auth (C.mongoUsername config) (C.mongoPassword config)) >>=
      \tf -> putStrLn $ "Authenticated with Mongo: " ++ (show tf)

   exitCode <- do
      failures <- mapM (loadAndInsert config pipe) files
      if any (== True) failures
         then return $ ExitFailure 1
         else return ExitSuccess

   close pipe

   exitWith exitCode
   -}


{-
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


buildFileList :: FilePath -> IO [FilePath]
buildFileList srcDirOrFile = do
   isFile <- doesFileExist srcDirOrFile
   if isFile then return [srcDirOrFile]
      else
         ( map (srcDirOrFile </>)                  -- ..relative paths
         . filter (not . isPrefixOf ".") )         -- ..minus dotfiles
         `fmap` getDirectoryContents srcDirOrFile  -- All files
-}
