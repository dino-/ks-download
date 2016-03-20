-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

{-# LANGUAGE OverloadedStrings, StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module KS.SourceConfig
   ( SourceConfig (..)
   , loadConfig
   , nullSourceConfig
   )
   where

import qualified Data.Map as Map
import qualified Data.Text as T
import KS.Data.Place ( GeoPoint (..) )
import System.FilePath
import TCE.Data.ReadConf ( readConfig )


deriving instance Read GeoPoint


data SourceConfig = SourceConfig
   { timeZone :: String
   , centroid :: GeoPoint
   , displayName :: T.Text
   , namewordsStopwords :: [T.Text]
   , namewordsSpecialCases :: Map.Map T.Text [T.Text]
   , placesTypes :: [String]
   }
   deriving (Read, Show)


nullSourceConfig :: SourceConfig
nullSourceConfig =
   SourceConfig "America/New_York" (GeoPoint 0.0 0.0) "" [] (Map.fromList []) []


loadConfig :: FilePath -> String -> IO SourceConfig
loadConfig confDir source = do
   let confPath = confDir </> ("ks-download-" ++ source) <.> "conf"
   (either error id . readConfig) `fmap` readFile confPath
