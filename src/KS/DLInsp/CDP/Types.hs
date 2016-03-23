-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

module KS.DLInsp.CDP.Types
   ( Options (..)
   , Downloader
   )
   where

import Data.Time.Calendar ( Day )
import System.FilePath ()


data Options = Options
   { optStartDate :: Maybe Day
   , optEndDate :: Maybe Day
   , optPageLimit :: Maybe Int
   , optHelp :: Bool
   }


type Downloader = Options -> FilePath -> IO ()
