-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

module KS.Clean.Remove.Options
   ( RemoveOptions (..)
   )
   where

import Data.Text ( Text )

import KS.Clean.Types


data RemoveOptions = RemoveOptions
   { optConfDir :: ConfDir
   , optPlaceId :: PlaceId
   }
