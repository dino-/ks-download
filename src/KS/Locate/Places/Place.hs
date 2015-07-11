-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

{-# LANGUAGE DeriveGeneric #-}

module KS.Locate.Places.Place
   ( PlLatLng (..), Place (..) )
   where

import Data.Aeson
import Data.Text
import GHC.Generics ( Generic )


data PlLatLng = PlLatLng
   { lat :: Double
   , lng :: Double
   }
   deriving (Generic, Show)

instance FromJSON PlLatLng
instance ToJSON PlLatLng


data Place = Place
   { name :: Text
   , vicinity :: Text
   , location :: PlLatLng
   , types :: [String]
   , place_id :: Text
   }
   deriving (Generic, Show)

instance FromJSON Place
instance ToJSON Place
