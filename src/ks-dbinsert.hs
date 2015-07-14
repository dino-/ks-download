-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

{-# LANGUAGE OverloadedStrings #-}

{- This is for inserting inspections into MongoDB
-}

import Control.Monad ( when )
import Data.Either ( isLeft )
import Data.List ( isPrefixOf )
import Database.MongoDB hiding ( options )
import System.Directory ( doesFileExist, getDirectoryContents )
import System.Environment ( getArgs )
import System.Exit ( ExitCode (..), exitSuccess, exitWith )
import System.FilePath
import System.IO
   ( BufferMode ( NoBuffering )
   , hSetBuffering, stdout, stderr
   )
import System.IO.Error ( tryIOError )
import Text.Printf ( printf )

import KS.Data.BSON ( docToBSON )
import qualified KS.Data.Document as D
import qualified KS.Database.Config as C
import KS.Database.Opts
import KS.Database.Mongo ( parseLastError )


main :: IO ()
main = do
   -- No buffering, it messes with the order of output
   mapM_ (flip hSetBuffering NoBuffering) [ stdout, stderr ]

   (options, srcDirsOrFiles) <- getArgs >>= parseOpts
   when ((optHelp options) || (null srcDirsOrFiles)) $ do
      putStrLn usageText
      exitSuccess

   config <- C.loadConfig options

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


loadAndInsert :: C.Config -> Pipe -> FilePath -> IO Bool
loadAndInsert config pipe path = do
   edoc <- tryIOError $ D.loadDoc path

   result <- case edoc of
      Left ex   -> return . Left $ show ex
      Right doc ->
         access pipe UnconfirmedWrites (C.mongoDatabase config) $ do
            save (C.mongoCollection config) $ docToBSON doc
            parseLastError `fmap` runCommand [ "getLastError" =: (1::Int) ]

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
