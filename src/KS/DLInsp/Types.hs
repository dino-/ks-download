-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

module KS.DLInsp.Types
   ( Options (..)
   , Downloader
   , DL, runDL

   -- re-exporting
   , asks, liftIO
   )
   where

import Control.Monad.Reader
import Data.Time.Calendar ( Day )
import System.FilePath ()


data Options = Options
   { optSource :: String
   , optDestDir :: FilePath
   , optStartDate :: Day
   , optEndDate :: Day
   , optPageLimit :: Maybe Int
   , optHelp :: Bool
   }


type Downloader = Options -> IO ()


type DL a = (ReaderT Options IO) a

runDL :: Options -> DL a -> IO a
runDL env ev = runReaderT ev env
