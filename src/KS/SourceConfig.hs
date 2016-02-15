-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module KS.SourceConfig
   ( SourceConfig (..)
   , loadConfig
   , nullSourceConfig
   )
   where

import qualified Data.Map as Map
import qualified Data.Text as T
import Data.Time.LocalTime ( TimeZone (..) )
import System.FilePath
import TCE.Data.ReadConf ( readConfig )


deriving instance Read TimeZone


data SourceConfig = SourceConfig
   { timeZone :: TimeZone
   , namewordsStopwords :: [T.Text]
   , namewordsSpecialCases :: Map.Map T.Text [T.Text]
   , placesTypes :: [String]
   }
   deriving (Read, Show)


nullSourceConfig :: SourceConfig
nullSourceConfig =
   SourceConfig (TimeZone (-300) False "EST") [] (Map.fromList []) []


loadConfig :: FilePath -> String -> IO SourceConfig
loadConfig confDir source = do
   let confPath = confDir </> ("ks-download-" ++ source) <.> "conf"
   (either error id . readConfig) `fmap` readFile confPath
