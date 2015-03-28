-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

module KS.DlInsp.Source.Downloaders
   where

import qualified Data.Map as M

import qualified KS.DlInsp.Source.NcWake as NcWake
import KS.DlInsp.Types


downloaders :: M.Map String Downloader
downloaders = M.fromList
   [ ("nc_wake", NcWake.download)
   ]
