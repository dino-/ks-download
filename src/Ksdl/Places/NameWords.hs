-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

{-# LANGUAGE OverloadedStrings #-}

module Ksdl.Places.NameWords
   ( toList
   )
   where

import qualified Data.List as L
import qualified Data.Map as Map
import Data.Text
import Prelude hiding ( filter, map )

import Ksdl
import Ksdl.Config
import Ksdl.Inspection


toList :: Ksdl [Text]
toList = do
   specialCases <- asks (namewordsSpecialCases . getConfig)
   list <- mkList

   iname <- asks (name . getInspection)
   return $ Map.findWithDefault
      list           -- Or make a list for a normal name
      iname          -- Find this name..
      specialCases   -- ..in these special cases


mkList :: Ksdl [Text]
mkList = do
   stopwords <- asks (namewordsStopwords . getConfig)
   (L.filter
        (not . isPrefixOf "#")
      . L.filter (\w -> not $ L.elem w stopwords)
      . L.take 2
      . split (== ' ')
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
