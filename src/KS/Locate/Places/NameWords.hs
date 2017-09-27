-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

{-# LANGUAGE OverloadedStrings #-}

module KS.Locate.Places.NameWords
   ( matchRuleFromInsp
   )
   where

import qualified Data.List as L
import Data.String.Conv ( toS )
import Data.Text ( Text, filter, isPrefixOf, map, split, strip, toLower )
import Prelude hiding ( filter, map )
import Text.Regex.Posix ( (=~) )

import KS.Data.Inspection ( Inspection (name) )
import KS.Locate.Locate ( KSDL, asks, getInspection, getSourceConfig )
import KS.SourceConfig
   ( MatchRule (KW, RJ), SourceConfig (matchRules, namewordsStopwords) )


matchRuleFromInsp :: KSDL MatchRule
matchRuleFromInsp = do
   matchRules' <- asks (matchRules . getSourceConfig)
   iname <- strip <$> asks (name . getInspection)
   checkRule iname matchRules'

   where
      checkRule iname' []                          = KW iname' <$> mkList
      checkRule iname' (mr@(KW name' _) : rules)
         | iname' == name'                         = return mr
         | otherwise                               = checkRule iname' rules
      checkRule iname' (mr@(RJ regexp)  : rules)
         | (toS iname' :: String) =~ regexp        = return mr
         | otherwise                               = checkRule iname' rules


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
