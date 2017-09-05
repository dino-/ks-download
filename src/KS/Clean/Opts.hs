-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

module KS.Clean.Opts
   ( Options (..)
   , parseOpts
   )
   where

import Data.Version ( showVersion )
import Options.Applicative ( Parser, (<>), (<**>), argument, auto,
   execParser, footerDoc, fullDesc, header, help, helper, info, long,
   metavar, option, optional, short, str, switch )
import Options.Applicative.Builder ( InfoMod )
import Paths_ks_download ( version )
import Text.PrettyPrint.ANSI.Leijen ( string )


data Options = Options
   { optBeforeDate :: Maybe Int
   , optArchive :: Bool
   , optConfDir :: FilePath
   }
   deriving Show


oldParser :: Parser Options
oldParser = Options <$> parseBeforeDate <*> parseArchive <*> parseConfDir


parseBeforeDate :: Parser (Maybe Int)
parseBeforeDate = optional . option auto $
   short 'b' <> long "before-date" <> metavar "YYYYMMDD" <>
   help "Check places inspected on or before this date. Default: 9 months ago"


parseArchive :: Parser Bool
parseArchive = switch $
   long "archive" <>
   help "Archive closed places. See ARCHIVING below."


parseConfDir :: Parser FilePath
parseConfDir = argument str $
   metavar "CONFDIR" <>
   help "Directory containing ks-download.conf file"


parseOpts = execParser $ info (oldParser <**> helper) $
      header "ks-clean - Tool for cleaning the Kitchen Snitch data"
   <> fullDesc
   <> usageText


usageText :: InfoMod a
usageText = footerDoc . Just . string . init . unlines $
   [ "Looks up places in inspections_recent that are older the date above. Reports on whether or not Google is reporting that the place is permanently closed."
   , ""
   , "Logging is written to stdout."
   , ""
   , ""
   , "ARCHIVING"
   , ""
   , "Archiving means, for each Places ID of a closed establishment, this will be done:"
   , ""
   , "  - All inspections for that Places ID will be inserted into the inspections_archived collection"
   , "  - All inspections for that Places ID will be deleted from the inspections_all collection"
   , "  - The one inspection for that Places ID will be deleted from the inspections_recent collection"
   , ""
   , "A place is determined to be closed by looking it up via the Google Places API."
   , ""
   , "Version " ++ (showVersion version) ++ "  Dino Morelli <dino@ui3.info>"
   ]
