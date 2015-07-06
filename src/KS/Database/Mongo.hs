-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-binds -fno-warn-orphans #-}

module KS.Database.Mongo
   --( Document (..), mkDoc, loadDoc, saveDoc )
   where

import qualified Codec.Binary.UTF8.String as UTF8
--import Control.Monad ( when )
import Data.Aeson
import Data.Aeson.Encode.Pretty
import qualified Data.ByteString as BS
--import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Geospatial
import Data.Maybe ( fromJust )
import Data.Text
import Data.Time
import Data.UUID ( UUID, fromString, toString )
import Data.UUID.V5 ( generateNamed )
import GHC.Generics ( Generic )
--import System.Directory ( removeFile )
--import System.FilePath
import Text.Printf ( printf )

import qualified KS.Inspection as I
import qualified KS.Locate.Database.Inspection as D
import KS.Locate.Opts
--import KS.Locate.Places.Match
import qualified KS.Locate.Places.Place as P


data Document = Document
   { _id :: String   -- New UUID made from place_id
   , doctype :: String
   , inspection :: Inspection
   , place :: Place
   }
   deriving (Generic, Show)

instance ToJSON Document
instance FromJSON Document


toDocument :: D.Document -> Document
toDocument (D.Document _ ty din dpl) = Document
   (newUUID (P.place_id dpl) (date newInsp))
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


-- This was generated from "honuapps.com" with the nil namespace
-- FIXME This should be factored somewhere common
nsUUID :: UUID
nsUUID = fromJust . fromString $
   "e95d936e-3845-582e-a0c5-3f53b3949b97"


-- FIXME This should be factored somewhere common
--    genUUID :: Text -> UUID
-- or
--    genUUID :: String -> UUID
{-
newUUID :: Text -> UTCTime -> UUID
newUUID placeId ut = generateNamed nsUUID . UTF8.encode
   $ printf "%s|%s" (unpack placeId) (formatTime defaultTimeLocale "%s" ut)
-}
newUUID :: Text -> UTCTime -> String
newUUID placeId ut = toString . generateNamed nsUUID . UTF8.encode
   $ printf "%s|%s" (unpack placeId) (formatTime defaultTimeLocale "%s" ut)


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
