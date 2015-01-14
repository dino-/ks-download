-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

{-# LANGUAGE DeriveGeneric #-}

module Ksdl.Inspection
   where

import qualified Codec.Binary.UTF8.String as UTF8
import Data.Aeson ( FromJSON, ToJSON, encode )
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Maybe ( fromJust )
import Data.Text hiding ( init, map, unlines )
import Data.UUID ( UUID, fromString, toString )
import Data.UUID.V5 ( generateNamed )
import GHC.Generics ( Generic )
import System.FilePath
import Text.Printf ( printf )
import Text.Regex ( matchRegex, mkRegex )


data Inspection = Inspection
   { _id :: String
   , name :: Text
   , score :: Double
   , location :: Text
   , inspection_date :: [Int]
   }
   deriving Generic

instance Show Inspection
   where show = displayInspection

instance FromJSON Inspection
instance ToJSON Inspection


parseDate :: String -> [Int]
parseDate dateStr =
   let mbParsed = (map read) `fmap` matchRegex re dateStr
       re = mkRegex "([0-9]{2})/([0-9]{2})/([0-9]{4})"
   in maybe [] (\(m : d : y : []) -> [y, m, d]) mbParsed


-- This was generated from "honuapps.com" with the nil namespace
nsUUID :: UUID
nsUUID = fromJust . fromString $
   "e95d936e-3845-582e-a0c5-3f53b3949b97"


setId :: Inspection -> Inspection
setId f = f { _id = toString newId }
   where
      newId = generateNamed nsUUID $ UTF8.encode $ printf "%s|%s|%s|%f"
         (unpack . name $ f) (unpack . location $ f)
         (show . inspection_date $ f) (score f)


saveInspection :: FilePath -> Inspection -> IO ()
saveInspection dir insp = BL.writeFile (dir </> (_id insp)) $ encode insp


displayInspection :: Inspection -> String
displayInspection insp = printf mask (_id insp) (unpack $ name insp)
   (inspection_date insp !! 0) (inspection_date insp !! 1)
   (inspection_date insp !! 2) (score insp)
   (unpack $ location insp)

   where
      mask = init . unlines $
         [ "Inspection %s"
         , "   %s | %4d-%02d-%02d %f"
         , "   %s"
         ]