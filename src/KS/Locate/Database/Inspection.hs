-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}

module KS.Locate.Database.Inspection
   ( mkDoc, saveDoc )
   where

import Control.Monad ( when )
import Data.Aeson
import Data.Aeson.Encode.Pretty
import qualified Data.ByteString.Lazy.Char8 as BL
import GHC.Generics ( Generic )
import System.Directory ( removeFile )
import System.FilePath

import qualified KS.Inspection as I
import KS.Locate.Opts
import KS.Locate.Places.Match
import qualified KS.Locate.Places.Place as P


data Document = Document
   { _id :: String
   , doctype :: String
   , inspection :: I.Inspection
   , place :: P.Place
   }
   deriving Generic

instance ToJSON Document


mkDoc :: Match -> Document
mkDoc ((I.IdInspection _id' inspection'), place') =
   Document _id' "inspection" inspection' place'


saveDoc :: Options -> FilePath -> Document -> IO ()
saveDoc options srcPath doc = do
   case (optSuccessDir options) of
      Just successDir -> BL.writeFile
         (successDir </> "ks_" ++ (_id doc) <.> "json")
         $ encode doc
      Nothing -> BL.putStrLn $ encodePretty doc

   when (optDelete options) $ removeFile srcPath
