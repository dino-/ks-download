-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

{-# LANGUAGE OverloadedStrings #-}

{- This is a development tool for inserting CouchBase data into MongoDB
-}

import Data.Aeson ( encode )
--import qualified Data.Bson as BSON
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Geospatial
import Data.List ( isPrefixOf )
import Data.Time ( defaultTimeLocale, formatTime )
import Data.Time.Clock.POSIX ( utcTimeToPOSIXSeconds )
import Database.MongoDB
import System.Directory ( doesFileExist, getDirectoryContents )
import qualified Data.Text as T
import System.Directory ( createDirectoryIfMissing )
import System.Environment ( getArgs )
import System.FilePath
import System.IO
   ( BufferMode ( NoBuffering )
   , hSetBuffering, stdout, stderr
   )
import Text.Printf ( printf )

--import KS.Database.Mongo  -- FIXME This module is leaving
--import qualified KS.Inspection as I
import qualified KS.Locate.Database.Inspection as D
--import KS.Locate.Database.Inspection ( Document (..), loadDoc )


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
m_collection   = "insp_objid"    -- without UUID
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


-- FIXME Wait, use KS.Database.Mongo.saveDoc!
saveNewFormat :: FilePath -> FilePath -> IO ()
saveNewFormat destDir inFile = do
   edoc <- D.loadDoc inFile
   putStrLn =<< case edoc of
      Left errMsg -> return errMsg
      Right doc   -> do
         let mdoc = toDocument doc
         createDirectoryIfMissing True destDir
         let outPath = destDir </> (mkFileName mdoc) <.> "json"
         exists <- doesFileExist outPath
         if exists
            then return $ printf "FILE EXISTS: %s <- %s" outPath inFile
            else do
               BL.writeFile outPath $ encode mdoc
               return $ printf "Wrote file: %s -> %s" inFile outPath


--mkFileName :: Document -> String
mkFileName doc = printf "ks_%s_%s" datePart placeID
   where
      datePart = (formatTime defaultTimeLocale "%Y-%m-%d")
         . date . inspection $ doc
      placeID = T.unpack . place_id . place $ doc


--inspToBSON :: Document -> BSON.Document
inspToBSON mdoc = insertionDoc
   where
      insp = inspection mdoc
      pl = place mdoc
      (GeoPoint coords) = location pl
      insertionDoc =
         [ "doctype" =: doctype mdoc
         , "inspection" =:
            [ "inspection_source" =: inspection_source insp
            , "name" =: iname insp
            , "addr" =: addr insp
            , "date" =: ((round . utcTimeToPOSIXSeconds . date
               $ insp) :: Integer)
            , "score" =: score insp
            , "violations" =: violations insp
            , "crit_violations" =: crit_violations insp
            , "reinspection" =: reinspection insp
            , "detail" =: detail insp
            ]
         , "place" =:
            [ "name" =: pname pl
            , "vicinity" =: vicinity pl
            , "location" =:
               [ "type" =: ("Point" :: T.Text)
               , "coordinates" =: coords
               ]
            , "types" =: types pl
            , "place_id" =: place_id pl
            ]
         ]


loadAndInsert :: Pipe -> FilePath -> IO ()
loadAndInsert pipe path = do
   edoc <- loadDoc path
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


{-
display :: Either String Document -> IO ()
display (Left msg) = print msg
display (Right doc) = putStrLn
   $  (_id doc) ++ " | "
   ++ (T.unpack . I.name . inspection $ doc)
-}
