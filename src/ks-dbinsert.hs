-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

{-# LANGUAGE OverloadedStrings #-}

{- This is for inserting inspections into MongoDB
-}

import Data.Either ( isLeft )
import Data.List ( isPrefixOf )
import Database.MongoDB
import System.Directory ( doesFileExist, getDirectoryContents )
import qualified Data.Text as T
import System.Environment ( getArgs )
import System.Exit ( exitFailure, exitSuccess )
import System.FilePath
import System.IO
   ( BufferMode ( NoBuffering )
   , hSetBuffering, stdout, stderr
   )
import Text.Printf ( printf )

import KS.Data.BSON ( docToBSON )
import qualified KS.Data.Document as D
import KS.Database.Mongo ( parseLastError )


{- Some of this goes into config
-}
mongoServerIP :: String
mongoServerIP = "tiddly.honuapps.com"

m_host :: Host
m_host = host mongoServerIP
--m_host = Host mongoServerIP notDefaultPort

m_db, m_collection, m_user, m_pass :: T.Text
m_db           = "ks"
--m_collection   = "insp_indiv"  -- with UUID
--m_collection   = "insp_objid"  -- without UUID
m_collection   = "insp_test"  -- development
m_user         = "mongoks"
m_pass         = "vaiDae8z"


main :: IO ()
main = do
   -- No buffering, it messes with the order of output
   mapM_ (flip hSetBuffering NoBuffering) [ stdout, stderr ]

   (srcDirOrFile : _) <- getArgs

   -- Paths to all files we'll be processing
   files <- buildFileList srcDirOrFile

   withDB (\p -> do
      failures <- mapM (loadAndInsert p) files
      if any (== True) failures
         then exitFailure
         else exitSuccess
      )


withDB :: (Pipe -> IO ()) -> IO ()
withDB action = do
      -- Get a connection to Mongo, they call it a 'pipe'
      pipe <- connect m_host

      -- Authenticate with mongo, show the auth state on stdout
      (access pipe UnconfirmedWrites m_db $ auth m_user m_pass) >>=
         \tf -> putStrLn $ "Authenticated with Mongo: " ++ (show tf)

      action pipe

      close pipe


loadAndInsert :: Pipe -> FilePath -> IO Bool
loadAndInsert pipe path = do
   edoc <- D.loadDoc path

   result <- case edoc of
      Left errMsg -> return . Left $ errMsg
      Right doc   -> access pipe UnconfirmedWrites m_db $ do
         save m_collection $ docToBSON doc
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
