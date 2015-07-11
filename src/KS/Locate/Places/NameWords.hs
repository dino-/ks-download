-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

{-# LANGUAGE OverloadedStrings #-}

module KS.Locate.Places.NameWords
   ( toList
   )
   where

import qualified Data.List as L
import qualified Data.Map as Map
import Data.Text
import Prelude hiding ( filter, map )

import KS.Inspection
import KS.Locate.Config
import KS.Locate.Locate


toList :: KSDL [Text]
toList = do
   specialCases <- asks (namewordsSpecialCases . getConfig)
   list <- mkList

   iname <- asks (name . getInspection)
   return $ Map.findWithDefault
      list           -- Or make a list for a normal name
      iname          -- Find this name..
      specialCases   -- ..in these special cases


mkList :: KSDL [Text]
mkList = do
   stopwords <- asks (namewordsStopwords . getConfig)
   (headList
      . L.filter (not . isPrefixOf "#")
      . L.filter (\w -> not $ L.elem w stopwords)
      . L.take 2
      . split (== ' ')
      . remove '\''
      . remove '`'
      . remove ','
      . remove '.'
      . tr '/' ' '
      . tr '-' ' '
      . toLower
      ) `fmap` asks (name . getInspection)


headList :: [a] -> [a]
headList (h : _) = [h]
headList _       = []


-- Return a string with all of a certain character removed
remove :: Char -> Text -> Text
remove c = filter (not . (== c))


-- Transpose a character for another in a Text string
tr :: Char -> Char -> Text -> Text
tr oldCh newCh src = map tr' src where
   tr' c
      | c == oldCh = newCh
      | otherwise  = c
