-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-binds -fno-warn-orphans #-}

module KS.Data.Document
   ( Document (..), loadDoc, saveDoc )
   where

import Data.Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Text as T
import Data.Time ( defaultTimeLocale, formatTime )
import GHC.Generics ( Generic )
import System.Directory ( doesFileExist )
import System.FilePath
import Text.Printf

import qualified KS.Data.Inspection as I
import qualified KS.Data.Place as P


data Document = Document
   { doctype :: String
   , inspection :: I.Inspection
   , place :: P.Place
   }
   deriving (Generic, Show)

instance ToJSON Document
instance FromJSON Document


saveDoc :: FilePath -> Document -> IO (Either String ())
saveDoc dir doc = do
   let datePart = (formatTime defaultTimeLocale "%Y-%m-%d")
         . I.date . inspection $ doc
   let filename = printf "ks_%s_%s" datePart
         (T.unpack . I.scrubName . P.name . place $ doc)
   let path = dir </> filename <.> "json"
   exists <- doesFileExist path
   if exists
      then return . Left $ printf "FAILED File exists: %s" path
      else Right <$> (BL.writeFile path $ encode doc)


loadDoc :: FilePath -> IO (Either String Document)
loadDoc path = eitherDecodeStrict' `fmap` BS.readFile path
