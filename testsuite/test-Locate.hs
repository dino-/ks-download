-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

import Test.Hspec

import qualified NameWords
import Places ( test_computeDistance )
import KS.Locate.Places.Match ( test_cleanAddress )


main :: IO ()
main = hspec $ do
   NameWords.tests
   test_cleanAddress
   test_computeDistance
