-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

{-# LANGUAGE DeriveGeneric #-}

module KS.Data.Place
   ( Place (..) )
   where

import Data.Aeson
import Data.Geospatial ( GeoPoint (..) )
import Data.Text
import GHC.Generics ( Generic )


data Place = Place
   { name :: Text
   , vicinity :: Text
   , location :: GeoPoint
   , types :: [String]
   , place_id :: Text
   }
   deriving (Generic, Show)

instance FromJSON Place
instance ToJSON Place
