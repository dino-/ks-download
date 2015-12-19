-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

{-# LANGUAGE DeriveGeneric #-}

module KS.RegionUpd.Region
   where

import Data.Aeson ( FromJSON )
import qualified Data.Text as T
import GHC.Generics ( Generic )


data Region = Region
   { _id :: T.Text
   , doctype :: T.Text
   , state :: T.Text
   , county :: T.Text
   }
   deriving (Generic)

instance FromJSON Region
