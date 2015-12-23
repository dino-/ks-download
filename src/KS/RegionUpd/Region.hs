-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

{-# LANGUAGE OverloadedStrings #-}

module KS.RegionUpd.Region
   ( Regions, Region
   , regions
   -- Re-exported
   , M.lookup
   )
   where

import qualified Data.Map as M
import qualified Data.Text as T


type Regions = M.Map T.Text Region

type Region = (T.Text, T.Text)


-- FIXME This needs to go into a conf file!
regions :: Regions
regions = M.fromList [("nc_wake", ("NC", "Wake"))]
