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
      actual `shouldBe` (Right output)


testData :: [(Text, [Text])]
testData =
   [ ("Belle at The Jones House", ["belle", "jones"])
   , ("BOJANGLES #15", ["bojangles"])
   , ("Cafe Tiramisu/North Ridge Pub", ["cafe", "tiramisu"])
   , ("Flights Restaurant-Raleigh Renaissance", ["flights", "restaurant"])
   , ("INTER-FAITH FOOD SHUTTLE @ INTERACT", ["interact"])
   , ("K&W CAFETERIA", ["k&w", "cafeteria"])
   , ("Kadhai-The Indian Wok", ["kadhai", "indian"])
   , ("NC Farm Bureau Cafeteria", ["nc", "farm"])
   , ("Piccola Italia", ["piccola"])
   , ("Quiznos Sub", ["quiznos", "sub"])
   , ("R.J.`S PLACE", ["rjs", "place"])
   , ("SAMI'S SUBS, PIZZA & MORE", ["samis", "subs"])
   , ("Tonys Bourbon Street Oyster Bar", ["tonys", "bourbon"])
   ]


fakeInspection :: Text -> Inspection
fakeInspection name' = nullInspection
   { inspection_source = "nc_wake"
   , name = name'
   }
