-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

import Data.Aeson
import qualified Data.ByteString as BS
import Data.List ( isPrefixOf )
import System.Directory ( copyFile, doesFileExist
   , getDirectoryContents, removeFile )
import System.Environment ( getArgs )
import System.Exit ( exitSuccess )
import System.FilePath
import System.IO
   ( BufferMode ( NoBuffering )
   , hSetBuffering, stdout, stderr
   )

import KS.Inspection
import KS.Locate.Config
import KS.Locate.Locate
import KS.Locate.Database.Inspection ( mkDoc, saveDoc )
import KS.Locate.Opts
import KS.Locate.Places.Geocoding ( forwardLookup )
import KS.Locate.Places.Match ( match )
import KS.Locate.Places.Place ( coordsToPlaces )
import KS.Log


main :: IO ()
main = do
   -- No buffering, it messes with the order of output
   mapM_ (flip hSetBuffering NoBuffering) [ stdout, stderr ]

   (options, srcDirsOrFiles) <- getArgs >>= parseOpts
   when ((optHelp options) || (null srcDirsOrFiles)) $ do
      putStrLn usageText
      exitSuccess

   -- Load the config file
   config <- loadConfig options

   initLogging $ logPriority config
   logStartMsg lname

   -- Paths to all files we'll be processing
   files <- concat `fmap`
      (sequence $ map buildFileList srcDirsOrFiles)

   -- Look up each inspection with Geocoding and Places
   mapM_ (lookupInspection config options) files

   noticeM lname line

   logStopMsg lname


buildFileList :: FilePath -> IO [FilePath]
buildFileList srcDirOrFile = do
   isFile <- doesFileExist srcDirOrFile
   if isFile then return [srcDirOrFile]
      else
         ( map (srcDirOrFile </>)                  -- ..relative paths
         . filter (not . isPrefixOf ".") )         -- ..minus dotfiles
         `fmap` getDirectoryContents srcDirOrFile  -- All files


lookupInspection :: Config -> Options -> FilePath -> IO ()
lookupInspection config options srcPath = do
   r <- runKSDL (Env config nullInspection) $ do
      liftIO $ noticeM lname line

      insp <- loadInspection srcPath
      local (\r -> r { getIdInspection = insp }) $ do
         geo <- forwardLookup
         places <- coordsToPlaces geo
         match places

   either (handleFailure) (saveDoc options srcPath . mkDoc) r

   where
      handleFailure msg = do
         -- Copy to FAILDIR if we have one
         maybe (return ()) (\failDir ->
            copyFile srcPath $ failDir </> takeFileName srcPath)
            $ optFailDir options

         -- Delete the original if we've been instructed to do so
         when (optDelete options) $ removeFile srcPath

         -- Log what happened
         errorM lname msg


loadInspection :: FilePath -> KSDL IdInspection
loadInspection path = do
   parseResult <- liftIO $ eitherDecodeStrict' `fmap` BS.readFile path
   either
      (\msg -> throwError $ "ERROR Inspection: " ++ path ++ "\n" ++ msg)
      (\insp -> (liftIO $ noticeM lname $ show insp) >> return insp)
      parseResult
