-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

{-# LANGUAGE OverloadedStrings, StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module KS.SourceConfig
   ( MatchRule (..)
   , SourceConfig (..)
   , loadConfig
   , nullSourceConfig
   )
   where

import qualified Data.Text as T
import KS.Data.Place ( GeoPoint (..) )
import System.FilePath
import TCE.Data.ReadConf ( readConfig )


data MatchRule
   = KW        -- Keywords match
      T.Text   -- FULL inspection restaurant name, this will be an exact match
      [T.Text] -- List of keywords for the Places query string
   | RJ        -- Reject
      String   -- Regular expression to match against inspection restaurant name
   deriving (Read, Show)


deriving instance Read GeoPoint


data SourceConfig = SourceConfig
   { timeZone :: String
   , centroid :: GeoPoint
   , displayName :: T.Text
   , namewordsStopwords :: [T.Text]
   , matchRules :: [MatchRule]
   , placesTypes :: [String]
   }
   deriving (Read, Show)


nullSourceConfig :: SourceConfig
nullSourceConfig =
   SourceConfig "America/New_York" (GeoPoint 0.0 0.0) "" [] [] []


loadConfig :: FilePath -> String -> IO SourceConfig
loadConfig confDir source = do
   let confPath = confDir </> ("ks-download-" ++ source) <.> "conf"
   (either error id . readConfig) `fmap` readFile confPath
