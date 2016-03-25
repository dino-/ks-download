-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

module KS.DLInsp.CDP.Types
   ( Options (..)
   , Downloader
   , lookupCountyID
   )
   where

import Data.Time.Calendar ( Day )
import System.FilePath ()


data Options = Options
   { optStartDate :: Maybe Day
   , optEndDate :: Maybe Day
   , optName :: Maybe String
   , optEstNames :: Bool
   , optHelp :: Bool
   }


type Downloader = Options -> FilePath -> IO ()


lookupCountyID :: String -> Int
lookupCountyID "nc_chatham" = 19
lookupCountyID "nc_durham" = 32
lookupCountyID "nc_orange" = 68
lookupCountyID _ = undefined
