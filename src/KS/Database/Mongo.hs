-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-binds -fno-warn-orphans #-}

module KS.Database.Mongo
   --( Document (..), mkDoc, loadDoc, saveDoc )
   where

--import Control.Monad ( when )
import Data.Aeson
--import Data.Aeson.Encode.Pretty
import qualified Data.ByteString as BS
--import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Geospatial
import Data.Text
import Data.Time
import GHC.Generics ( Generic )
--import System.Directory ( removeFile )
--import System.FilePath

import qualified KS.Inspection as I
import qualified KS.Locate.Database.Inspection as D
--import KS.Locate.Opts
--import KS.Locate.Places.Match
import qualified KS.Locate.Places.Place as P


data Document = Document
   { doctype :: String
   , inspection :: Inspection
   , place :: Place
   }
   deriving (Generic, Show)

instance ToJSON Document
instance FromJSON Document


toDocument :: D.Document -> Document
toDocument (D.Document _ ty din dpl) = Document
   ty
   newInsp
   (toPlace dpl)

   where
      newInsp = toInspection din


data Inspection = Inspection
   { inspection_source :: String
   , iname :: Text
   , addr :: Text
   , date :: UTCTime
   , score :: Double
   , violations :: Int
   , crit_violations :: Int
   , reinspection :: Bool
   , detail :: String
   }
   deriving (Generic, Show)

instance ToJSON Inspection
instance FromJSON Inspection


toInspection :: I.Inspection -> Inspection
toInspection (I.Inspection src na ad (y : m : d : _) sc vi cr re de) =
   Inspection src na ad ut sc vi cr re de

   where ut = UTCTime (fromGregorian (fromIntegral y) m d) 0
toInspection _ = undefined


data Place = Place
   { pname :: Text
   , vicinity :: Text
   , location :: GeoPoint
   , types :: [String]
   , place_id :: Text
   }
   deriving (Generic, Show)

instance ToJSON Place
instance FromJSON Place


toPlace :: P.Place -> Place
toPlace (P.Place n v (P.PlLatLng lat lng) ts p_id) = Place
   n v (GeoPoint [lng, lat]) ts p_id


{-
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
-}


loadDoc :: FilePath -> IO (Either String Document)
loadDoc path = eitherDecodeStrict' `fmap` BS.readFile path
