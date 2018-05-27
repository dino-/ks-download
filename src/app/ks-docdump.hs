-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

{- This is a development tool for exploring the data in various ways
-}

import Control.Monad ( (>=>) )
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

import qualified KS.Inspection as I
import KS.Locate.Database.Inspection ( Document (..), loadDoc )


main :: IO ()
main = do
   -- No buffering, it messes with the order of output
   mapM_ (flip hSetBuffering NoBuffering) [ stdout, stderr ]

   (srcDirOrFile : _) <- getArgs

   -- Paths to all files we'll be processing
   files <- buildFileList srcDirOrFile

   -- Load and display each document
   mapM_ (loadDoc >=> display) files


buildFileList :: FilePath -> IO [FilePath]
buildFileList srcDirOrFile = do
   isFile <- doesFileExist srcDirOrFile
   if isFile then return [srcDirOrFile]
      else
         ( map (srcDirOrFile </>)                  -- ..relative paths
         . filter (not . isPrefixOf ".") )         -- ..minus dotfiles
         `fmap` getDirectoryContents srcDirOrFile  -- All files


display :: Either String Document -> IO ()
display (Left msg) = print msg
display (Right doc) = putStrLn
   $  (_id doc) ++ " | "
   ++ (T.unpack . I.name . inspection $ doc)
