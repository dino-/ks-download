-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

module KS.Clean.Closed.Options
   ( ClosedOptions (..)
   )
   where

import KS.Clean.Types


data ClosedOptions = ClosedOptions
   { optArchive :: ShouldArchive
   , optConfDir :: ConfDir
   }
