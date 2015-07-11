-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}

module KS.Inspection
   ( Inspection (..)
   , nullInspection
   , parseDate
   , saveInspection
   , loadInspection
   , scrubName
   )
   where

import Data.Aeson ( FromJSON, ToJSON, eitherDecodeStrict', encode )
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Char ( isAlphaNum )
import Data.Maybe ( fromJust )
import qualified Data.Text as T
import Data.Time ( UTCTime (..), defaultTimeLocale, formatTime
   , parseTimeM )
import Data.Time.Clock.POSIX ( posixSecondsToUTCTime )
import GHC.Generics ( Generic )
import System.FilePath
import Text.Printf ( printf )


data Inspection = Inspection
   { inspection_source :: String
   , name :: T.Text
   , addr :: T.Text
   , date :: UTCTime
   , score :: Double
   , violations :: Int
   , crit_violations :: Int
   , reinspection :: Bool
   , detail :: String
   }
   deriving Generic

instance Show Inspection
   where show = formatForDisplay

instance FromJSON Inspection
instance ToJSON Inspection


nullInspection :: Inspection
nullInspection = Inspection "" "" "" (posixSecondsToUTCTime 0) 0.0 0 0 False ""


parseDate :: String -> UTCTime
parseDate dateStr = fromJust $  -- FIXME Very dangerous
   parseTimeM True defaultTimeLocale "%m/%d/%0Y" dateStr


-- FIXME Should this be in a common module?
scrubName :: T.Text -> T.Text
scrubName = T.filter isAlphaNum


mkFileName :: Inspection -> String
mkFileName insp = printf "insp_%s_%s" (formatDay . date $ insp)
   $ T.unpack . scrubName . name $ insp


formatDay :: UTCTime -> String
formatDay = formatTime defaultTimeLocale "%Y-%m-%d"


saveInspection :: FilePath -> Inspection -> IO ()
saveInspection dir insp = BL.writeFile
   (dir </> (mkFileName insp) <.> "json") $ encode insp


loadInspection :: FilePath -> IO (Either String Inspection)
loadInspection path = eitherDecodeStrict' `fmap` BS.readFile path


formatForDisplay :: Inspection -> String
formatForDisplay
   (Inspection src n a ut sc viol crit r _) =

   printf mask (T.unpack n) (formatDay ut) sc viol crit (showRe r) src (T.unpack a)

   where
      mask = init . unlines $
         [ "Inspection"
         , "   %s | %s %f %d/%d %s | %s"
         , "   %s"
         ]

      showRe :: Bool -> String
      showRe True  = "Re"
      showRe False = "Ins"
