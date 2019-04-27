-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

module KS.Clean.Old.Options
   ( OldOptions (..)
   )
   where

import KS.Clean.Types


data OldOptions = OldOptions
   { optBeforeDate :: Maybe Date
   , optArchive :: ShouldArchive
   , optConfDir :: ConfDir
   }
