-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

import Data.Aeson.Encode.Pretty hiding ( Config )
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.List ( isPrefixOf )
import Data.Version ( showVersion )
import Paths_ks_download ( version )
import System.Directory ( copyFile, doesFileExist
   , getDirectoryContents, removeFile )
import System.Environment ( getArgs )
import System.Exit ( exitFailure, exitSuccess )
import System.FilePath
import System.IO
   ( BufferMode ( NoBuffering )
   , hSetBuffering, stdout, stderr
   )
import System.IO.Error
import Text.Printf ( printf )

import KS.Data.Document ( Document (..), saveDocument )
import KS.Data.Inspection
import KS.Locate.Config
import KS.Locate.Locate
import KS.Locate.Opts
import KS.Locate.Places.Geocoding ( forwardLookup )
import KS.Locate.Places.Match ( Match, match )
import KS.Locate.Places.Places ( coordsToPlaces )
import qualified KS.SourceConfig as SC
import KS.Log


main :: IO ()
main = do
   -- No buffering, it messes with the order of output
   mapM_ (flip hSetBuffering NoBuffering) [ stdout, stderr ]

   (options, args) <- getArgs >>= parseOpts
   when (optHelp options) $ putStrLn usageText >> exitSuccess
   when (length args < 2) $ putStrLn usageText >> exitFailure
   let (confDir : srcDirsOrFiles) = args

   -- Load the config file
   config <- loadConfig confDir

   initLogging $ logPriority config
   noticeM lname $
      printf "ks-locate version %s started" (showVersion version)
   logStartMsg lname

   -- Paths to all files we'll be processing
   files <- concat `fmap`
      (sequence $ map buildFileList srcDirsOrFiles)

   -- Look up each inspection with Geocoding and Places
   mapM_ (lookupInspection config options confDir) files

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


loadInspection' :: FilePath -> KSDL Inspection
loadInspection' path = do
   r <- liftIO $ tryIOError $ loadInspection path

   case r of
      Left ex -> throwError $ ErrMsg CRITICAL $ printf "Error loading %s:\n%s" path (show ex)
      Right insp -> do
         liftIO . noticeM lname . show $ insp
         return insp
