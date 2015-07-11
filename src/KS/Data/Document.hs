-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-binds -fno-warn-orphans #-}

module KS.Data.Document
   ( Document (..), mkDoc, loadDoc, saveDoc )
   where

import Control.Monad ( when )
import Data.Aeson
import Data.Aeson.Encode.Pretty
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Text as T
import Data.Time ( defaultTimeLocale, formatTime )
import GHC.Generics ( Generic )
import System.Directory ( removeFile )
import System.FilePath
import Text.Printf

import qualified KS.Data.Inspection as I
import qualified KS.Data.Place as P
import KS.Locate.Opts
import KS.Locate.Places.Match


data Document = Document
   { doctype :: String
   , inspection :: I.Inspection
   , place :: P.Place
   }
   deriving Generic

instance ToJSON Document
instance FromJSON Document


mkDoc :: Match -> Document
mkDoc (inspection', place') =
   Document "inspection" inspection' place'


-- FIXME Need this to watch for file existance
-- Probably means putting it in ExceptT monad somehow
-- Or perhaps simply like loadDoc below
saveDoc :: Options -> FilePath -> Document -> IO ()
saveDoc options srcPath doc = do
   case (optSuccessDir options) of
      Just successDir -> BL.writeFile
         (successDir </> filename <.> "json")
         $ encode doc
      Nothing -> BL.putStrLn $ encodePretty doc

   when (optDelete options) $ removeFile srcPath

   where
      filename = printf "ks_%s_%s" datePart
         (T.unpack . I.scrubName . P.name . place $ doc)
      datePart = (formatTime defaultTimeLocale "%Y-%m-%d")
         . I.date . inspection $ doc


loadDoc :: FilePath -> IO (Either String Document)
loadDoc path = eitherDecodeStrict' `fmap` BS.readFile path
