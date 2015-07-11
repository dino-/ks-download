-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

{-# LANGUAGE OverloadedStrings #-}

{- This is for inserting inspections into MongoDB
-}

import Control.Monad ( (>=>) )
import Data.Geospatial ( GeoPoint (..) )
import Data.List ( isPrefixOf )
import Data.Time.Clock.POSIX ( utcTimeToPOSIXSeconds )
import Database.MongoDB
import System.Directory ( doesFileExist, getDirectoryContents )
import qualified Data.Text as T
import System.Environment ( getArgs )
import System.FilePath
import System.IO
   ( BufferMode ( NoBuffering )
   , hSetBuffering, stdout, stderr
   )

import qualified KS.Data.Document as D
import qualified KS.Data.Inspection as I
import qualified KS.Data.Place as P


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

   (command : srcDirOrFile : rest') <- getArgs

   -- Paths to all files we'll be processing
   files <- buildFileList srcDirOrFile

   case command of
      --"getone"  -> withDB getOne
      --"convertname" -> -- Just a name change using saveDoc
      {-
      "convert" -> do
         let (outDir : _) = rest'
         mapM_ (saveNewFormat outDir) files
      -}
      "display" -> mapM_ (D.loadDoc >=> print) files
      "insert"  -> withDB (\p -> mapM_ (loadAndInsert p) files)
      _         -> undefined


withDB :: (Pipe -> IO ()) -> IO ()
withDB action = do
      -- Get a connection to Mongo, they call it a 'pipe'
      pipe <- connect m_host

      -- Authenticate with mongo, show the auth state on stdout
      (access pipe UnconfirmedWrites m_db $ auth m_user m_pass) >>=
         \tf -> putStrLn $ "Authenticated with Mongo: " ++ (show tf)

      action pipe

      close pipe


{- FIXME Broken by abandonment of UUID
getOne :: Pipe -> IO ()
getOne pipe = do
   return ()
   d <- access pipe UnconfirmedWrites m_db $ do
      let muuid = UUID . toASCIIBytes . fromJust . fromString
            $ "ec53d9d7-c8e8-553b-9328-c10d6908a43b"
      rest =<< find (select ["_id" =: muuid] m_collection)
   print d
-}


getAll :: Pipe -> IO ()
getAll pipe = do
   is <- access pipe UnconfirmedWrites m_db $ do
      rest =<< find (select [] m_collection)
   mapM_ print is


-- This is our KS.Data.Document -> Data.BSON.Document
inspToBSON :: D.Document -> Document
inspToBSON doc = insertionDoc
   where
      insp = D.inspection doc
      pl = D.place doc
      (GeoPoint coords) = P.location pl
      insertionDoc =
         [ "doctype" =: D.doctype doc
         , "inspection" =:
            [ "inspection_source" =: I.inspection_source insp
            , "name" =: I.name insp
            , "addr" =: I.addr insp
            , "date" =: ((round . utcTimeToPOSIXSeconds . I.date
               $ insp) :: Integer)
            , "score" =: I.score insp
            , "violations" =: I.violations insp
            , "crit_violations" =: I.crit_violations insp
            , "reinspection" =: I.reinspection insp
            , "detail" =: I.detail insp
            ]
         , "place" =:
            [ "name" =: P.name pl
            , "vicinity" =: P.vicinity pl
            , "location" =:
               [ "type" =: ("Point" :: T.Text)
               , "coordinates" =: coords
               ]
            , "types" =: P.types pl
            , "place_id" =: P.place_id pl
            ]
         ]


loadAndInsert :: Pipe -> FilePath -> IO ()
loadAndInsert pipe path = do
   edoc <- D.loadDoc path
   putStrLn =<< case edoc of
      Left errMsg -> return errMsg
      Right mdoc   -> access pipe UnconfirmedWrites m_db $ do
         let insertionDoc = inspToBSON mdoc
         save m_collection insertionDoc
         show `fmap` runCommand [ "getLastError" =: (1::Int) ]


buildFileList :: FilePath -> IO [FilePath]
buildFileList srcDirOrFile = do
   isFile <- doesFileExist srcDirOrFile
   if isFile then return [srcDirOrFile]
      else
         ( map (srcDirOrFile </>)                  -- ..relative paths
         . filter (not . isPrefixOf ".") )         -- ..minus dotfiles
         `fmap` getDirectoryContents srcDirOrFile  -- All files
