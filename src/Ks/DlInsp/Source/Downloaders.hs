-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

module Ks.DlInsp.Source.Downloaders
   where

import qualified Data.Map as M

import qualified Ks.DlInsp.Source.NcWake as NcWake
import Ks.DlInsp.Types


downloaders :: M.Map String Downloader
downloaders = M.fromList
   [ ("nc_wake", NcWake.download)
   ]
