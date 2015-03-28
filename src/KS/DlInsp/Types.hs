-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

module KS.DlInsp.Types
   ( Options (..)
   , Downloader
   , Dl, runDl

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


type Dl a = (ReaderT Options IO) a

runDl :: Options -> Dl a -> IO a
runDl env ev = runReaderT ev env
