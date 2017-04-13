-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

import Test.Hspec

import qualified NameWords


main :: IO ()
main = hspec $ do
   NameWords.tests
