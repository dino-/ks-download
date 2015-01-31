-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

module Ks.Locate.Config
   ( Config (..)
   , GoogleKey (..)
   , loadConfig )
   where

import Data.List ( isPrefixOf )
import qualified Data.Map as Map
import qualified Data.Text as T
import System.Log


newtype GoogleKey = GoogleKey String
   deriving (Read, Show)


data Config = Config
   { logPriority :: Priority
   , googleApiKey :: GoogleKey
   , geocodingApiDelay :: Int
   , namewordsStopwords :: [T.Text]
   , namewordsSpecialCases :: Map.Map T.Text [T.Text]
   , placesTypes :: [String]
   }
   deriving (Read, Show)


loadConfig :: FilePath -> IO Config
loadConfig path = do
   parseResult <- (reads . removeComments) `fmap` readFile path
   case parseResult of
      [(c, _)] -> return c
      _        -> error $ "ERROR parsing config file: " ++ path


{- Auto-derived Read instancing has no idea how to handle Haskell source
   code commenting. This will strip out very simple -- style comments.
-}
removeComments :: String -> String
removeComments = unlines . map removeComment . lines
   where
      removeComment =
         unwords . (takeWhile (not . isPrefixOf "--")) . words
