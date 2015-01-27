-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}

module Ks.Locate.Database.Inspection
   ( mkDoc, saveDoc )
   where

import Control.Monad ( when )
import Data.Aeson
import Data.Aeson.Encode.Pretty
import qualified Data.ByteString.Lazy.Char8 as BL
import GHC.Generics ( Generic )
import System.Directory ( removeFile )
import System.FilePath

import qualified Ks.Inspection as I
import Ks.Locate.Opts
import Ks.Locate.Places.Match
import qualified Ks.Locate.Places.Place as P


data Document = Document
   { doctype :: String
   , inspection :: I.Inspection
   , place :: P.Place
   }
   deriving Generic

instance ToJSON Document where
   toJSON (Document ty insp pl) = object
      [ "_id" .= I._id insp
      , "type" .= ty
      , "place_id" .= P.place_id pl
      , "inspection_source" .= I.inspection_source insp
      , "inspection" .= object
         [ "name" .= I.name insp
         , "addr" .= I.addr insp
         , "date" .= (toJSON . I.date $ insp)
         , "score" .= I.score insp
         , "violations" .= I.violations insp
         , "crit_violations" .= I.crit_violations insp
         , "reinspection" .= I.reinspection insp
         , "detail" .= I.detail insp
         ]
      , "place" .= object
         [ "name" .= P.name pl
         , "vicinity" .= P.vicinity pl
         , "location" .= (toJSON . P.location $ pl)
         , "types" .= P.types pl
         ]
      ]


mkDoc :: Match -> Document
mkDoc (i, p) = Document "inspection" i p


saveDoc :: Options -> FilePath -> Document -> IO ()
saveDoc options srcPath doc = do
   case (optSuccessDir options) of
      Just successDir -> BL.writeFile
         (successDir </> "ks_" ++ (I._id . inspection $ doc) <.> "json")
         $ encode doc
      Nothing -> BL.putStrLn $ encodePretty doc

   when (optDelete options) $ removeFile srcPath
