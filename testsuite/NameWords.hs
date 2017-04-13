-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

{-# LANGUAGE OverloadedStrings #-}

module NameWords
   ( tests )
   where

import Data.Text hiding ( foldl, map )
import Test.Hspec
import Text.Printf ( printf )

import KS.Data.Inspection
import KS.Locate.Config
import KS.Locate.Locate
import KS.Locate.Places.NameWords ( toList )
import qualified KS.SourceConfig as SC


tests :: SpecWith ()
tests = describe "NameWords" $ foldl (>>) (return ()) . map testNameWords $ testData


testNameWords :: (Text, [Text]) -> SpecWith ()
testNameWords (input, output) = do
   let confDir = "resources"

   -- Loading the default config template file, inspection and
   -- config for this inspection source
   conf <- runIO $ loadConfig confDir
   let insp = fakeInspection input
   sourceConf <- runIO $ SC.loadConfig confDir $ inspection_source insp

   actual <- runIO $ runKSDL (Env conf sourceConf insp) toList

   it (printf "words extracted for \"%s\"" (unpack input)) $
      (Right output) `shouldBe` actual


testData :: [(Text, [Text])]
testData =
   [ ("Belle at The Jones House", ["belle"])
   , ("BOJANGLES #15", ["bojangles"])
   , ("Cafe Tiramisu/North Ridge Pub", ["tiramisu"])
   , ("Flights Restaurant-Raleigh Renaissance", ["flights"])
   , ("INTER-FAITH FOOD SHUTTLE @ INTERACT", ["interact"])
   , ("K&W CAFETERIA", ["k&w"])
   , ("Kadhai-The Indian Wok", ["kadhai"])
   , ("NC Farm Bureau Cafeteria", ["farm"])
   , ("New Wangs Kitchen", ["wangs"])
   , ("Piccola Italia", ["piccola"])
   , ("Quiznos Sub", ["quiznos"])
   , ("R.J.`S PLACE", ["rjs"])
   , ("SAMI'S SUBS, PIZZA & MORE", ["samis"])
   , ("Tonys Bourbon Street Oyster Bar", ["tonys"])
   ]


fakeInspection :: Text -> Inspection
fakeInspection name' = nullInspection
   { inspection_source = "nc_wake"
   , name = name'
   }
