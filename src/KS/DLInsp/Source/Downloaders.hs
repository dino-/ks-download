-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

module KS.DLInsp.Source.Downloaders
   where

import qualified Data.Map as M

import qualified KS.DLInsp.Source.NCWake as NCWake
import KS.DLInsp.Types


downloaders :: M.Map String Downloader
downloaders = M.fromList
   [ ("nc_wake", NCWake.download)
   ]
