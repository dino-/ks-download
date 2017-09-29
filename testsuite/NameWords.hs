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
import KS.Locate.Places.NameWords ( matchRuleFromInsp )
import qualified KS.SourceConfig as SC


tests :: SpecWith ()
tests = describe "NameWords" $ foldl (>>) (return ()) . map testNameWords $ testData


testNameWords :: SC.MatchRule -> SpecWith ()

testNameWords expected@(SC.KW name' _) = do
   let confDir = "resources"

   -- Loading the default config template file, inspection and
   -- config for this inspection source
   conf <- runIO $ loadConfig confDir
   let insp = fakeInspection name'
   sourceConf <- runIO $ SC.loadConfig confDir $ inspection_source insp

   actual <- runIO $ runKSDL (Env conf sourceConf insp) matchRuleFromInsp

   it (printf "words extracted for \"%s\"" (unpack name')) $
      actual `shouldBe` (Right expected)

testNameWords _ = return undefined


testData :: [SC.MatchRule]
testData =
   [ SC.KW "Belle at The Jones House" ["belle", "jones"]
   , SC.KW "BOJANGLES #15" ["bojangles"]
   , SC.KW "Cafe Tiramisu/North Ridge Pub" ["cafe", "tiramisu"]
   , SC.KW "Flights Restaurant-Raleigh Renaissance" ["flights", "restaurant"]
   , SC.KW "INTER-FAITH FOOD SHUTTLE @ INTERACT" ["interact"]
   , SC.KW "K&W CAFETERIA" ["k&w", "cafeteria"]
   , SC.KW "Kadhai-The Indian Wok" ["kadhai", "indian"]
   , SC.KW "NC Farm Bureau Cafeteria" ["nc", "farm"]
   , SC.KW "Piccola Italia" ["piccola"]
   , SC.KW "Quiznos Sub" ["quiznos", "sub"]
   , SC.KW "R.J.`S PLACE" ["rjs", "place"]
   , SC.KW "SAMI'S SUBS, PIZZA & MORE" ["samis", "subs"]
   , SC.KW "Tonys Bourbon Street Oyster Bar" ["tonys", "bourbon"]
   ]


fakeInspection :: Text -> Inspection
fakeInspection name' = nullInspection
   { inspection_source = "nc_wake"
   , name = name'
   }
