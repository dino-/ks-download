-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

module KS.Locate.SourceConfig
   ( SourceConfig (..)
   , loadConfig
   , nullSourceConfig
   )
   where

import qualified Data.Map as Map
import qualified Data.Text as T
import System.FilePath
import TCE.Data.ReadConf ( readConfig )


data SourceConfig = SourceConfig
   { namewordsStopwords :: [T.Text]
   , namewordsSpecialCases :: Map.Map T.Text [T.Text]
   , placesTypes :: [String]
   }
   deriving (Read, Show)


nullSourceConfig :: SourceConfig
nullSourceConfig = SourceConfig [] (Map.fromList []) []


loadConfig :: FilePath -> String -> IO SourceConfig
loadConfig confDir source = do
   let confPath = confDir </> ("ks-download-" ++ source) <.> "conf"
   (either error id . readConfig) `fmap` readFile confPath
