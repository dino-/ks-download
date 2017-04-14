-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

{-# LANGUAGE OverloadedStrings #-}

module KS.Locate.Places.NameWords
   ( toList
   )
   where

import qualified Data.List as L
import qualified Data.Map as Map
import Data.Text ( Text, filter, isPrefixOf, map, split, strip, toLower )
import Prelude hiding ( filter, map )

import KS.Data.Inspection ( Inspection (name) )
import KS.Locate.Locate ( KSDL, asks, getInspection, getSourceConfig )
import KS.SourceConfig
   ( SourceConfig (namewordsSpecialCases, namewordsStopwords) )


toList :: KSDL [Text]
toList = do
   specialCases <- asks (namewordsSpecialCases . getSourceConfig)
   list <- mkList

   iname <- strip <$> asks (name . getInspection)
   return $ Map.findWithDefault
      list           -- Or make a list for a normal name
      iname          -- Find this name..
      specialCases   -- ..in these special cases


mkList :: KSDL [Text]
mkList = do
   stopwords <- asks (namewordsStopwords . getSourceConfig)
   ( L.filter (not . isPrefixOf "#")
      . L.take 2
      . L.filter (\w -> not $ L.elem w stopwords)
      . split (== ' ')
      . remove '\''
      . remove '`'
      . remove ','
      . remove '.'
      . tr '/' ' '
      . tr '-' ' '
      . toLower
      ) `fmap` asks (name . getInspection)


-- Return a string with all of a certain character removed
remove :: Char -> Text -> Text
remove c = filter (not . (== c))


-- Transpose a character for another in a Text string
tr :: Char -> Char -> Text -> Text
tr oldCh newCh src = map tr' src where
   tr' c
      | c == oldCh = newCh
      | otherwise  = c
