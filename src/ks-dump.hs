-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

import Control.Monad ( (>=>) )
import Data.Aeson
import qualified Data.ByteString as BS
import Data.List ( isPrefixOf )
import System.Directory ( doesFileExist
   , getDirectoryContents )
import qualified Data.Text as T
import System.Environment ( getArgs )
import System.FilePath
import System.IO
   ( BufferMode ( NoBuffering )
   , hSetBuffering, stdout, stderr
   )

import Ks.Inspection


main :: IO ()
main = do
   -- No buffering, it messes with the order of output
   mapM_ (flip hSetBuffering NoBuffering) [ stdout, stderr ]

   (srcDirOrFile : _) <- getArgs

   -- Paths to all files we'll be processing
   files <- buildFileList srcDirOrFile

   -- Look up each inspection with Geocoding and Places
   mapM_ (loadInspection >=> display) files


buildFileList :: FilePath -> IO [FilePath]
buildFileList srcDirOrFile = do
   isFile <- doesFileExist srcDirOrFile
   if isFile then return [srcDirOrFile]
      else
         ( map (srcDirOrFile </>)                  -- ..relative paths
         . filter (not . isPrefixOf ".") )         -- ..minus dotfiles
         `fmap` getDirectoryContents srcDirOrFile  -- All files


loadInspection :: FilePath -> IO IdInspection
loadInspection path = do
   parseResult <- eitherDecodeStrict' `fmap` BS.readFile path
   either
      (\msg -> error $ "ERROR Inspection: " ++ path ++ "\n" ++ msg)
      return parseResult


display :: IdInspection -> IO ()
display (IdInspection _id' insp) = putStrLn $
   (T.unpack . name $ insp) ++ " | " ++
   (T.unpack . addr $ insp) ++ " | " ++
   _id'
