-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}

module Ks.Inspection
   ( Inspection (..)
   , nullInspection
   , parseDate
   , setId
   , saveInspection
   )
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
   , inspection_source :: String
   , name :: Text
   , addr :: Text
   , date :: [Int]
   , score :: Double
   , violations :: Int
   , crit_violations :: Int
   , reinspection :: Bool
   , detail :: String
   }
   deriving Generic

instance Show Inspection
   where show = displayInspection

instance FromJSON Inspection
instance ToJSON Inspection


nullInspection :: Inspection
nullInspection = Inspection "" "" "" "" [] 0.0 0 0 False ""


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
setId i = i { _id = toString newId }
   where
      newId = generateNamed nsUUID $ UTF8.encode $ printf "%s|%s|%s|%f"
         (unpack . name $ i) (unpack . addr $ i)
         (show . date $ i) (score i)


saveInspection :: FilePath -> Inspection -> IO ()
saveInspection dir insp =
   BL.writeFile (dir </> ("insp_" ++ _id insp)) $ encode insp


displayInspection :: Inspection -> String
displayInspection (Inspection i src n a [y, m, d] sc viol crit r _) =
   printf mask i (unpack n) y m d sc viol crit (showRe r) src (unpack a)

   where
      mask = init . unlines $
         [ "Inspection %s"
         , "   %s | %4d-%02d-%02d %f %d/%d %s | %s"
         , "   %s"
         ]

      showRe :: Bool -> String
      showRe True  = "Re"
      showRe False = "Ins"

displayInspection _ = undefined
