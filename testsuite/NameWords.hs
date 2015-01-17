-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

{-# LANGUAGE OverloadedStrings #-}

module NameWords
   ( tests )
   where

import Control.Applicative
import Data.Text hiding ( map )
import Test.HUnit
import Text.Printf ( printf )

import Ksdl
import Ksdl.Config
import Ksdl.Inspection
import Ksdl.Places.NameWords ( toList )


tests :: Test
tests = TestList $ map testNameWords testData


testNameWords :: (Text, [Text]) -> Test
testNameWords (input, output) = TestCase $ do
   env <- Env <$> loadConfig "ksdl.conf" <*> fakeInspection input
   actual <- runKsdl env $ toList
   let label = printf "name words for \"%s\"" (unpack input)
   assertEqual label (Right output) actual


testData :: [(Text, [Text])]
testData =
   [ ("Belle at The Jones House", ["belle"])
   , ("BOJANGLES #15", ["bojangles"])
   , ("Flights Restaurant-Raleigh Renaissance", ["flights"])
   , ("K&W CAFETERIA", ["k&w"])
   , ("Kadhai-The Indian Wok", ["kadhai"])
   , ("Piccola Italia", ["piccola"])
   , ("R.J.`S PLACE", ["rj`s","place"])
   , ("Tonys Bourbon Street Oyster Bar", ["tonys"])
   ]


fakeInspection :: Text -> IO Inspection
fakeInspection name' =
   return $ Inspection "" "" name' "" [] 0.0 False ""
