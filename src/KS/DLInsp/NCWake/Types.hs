-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

module KS.DLInsp.NCWake.Types
   ( Options (..)
   , Downloader
   , DL, runDL

   -- re-exporting
   , asks, liftIO
   )
   where

import Control.Monad.Reader ( ReaderT, asks, liftIO, runReaderT )
import Data.Time.Calendar ( Day )
import System.FilePath ()


data Options = Options
   { optStartDate :: Maybe Day
   , optEndDate :: Maybe Day
   , optPageLimit :: Maybe Int
   , optHelp :: Bool
   }


type Downloader = Options -> FilePath -> IO ()


type DL a = (ReaderT Options IO) a

runDL :: Options -> DL a -> IO a
runDL env ev = runReaderT ev env
