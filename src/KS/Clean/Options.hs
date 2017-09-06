-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

module KS.Clean.Options
   ( Options (..)
   , parseOpts
   )
   where

import Data.Version ( showVersion )
import Options.Applicative ( Parser, ParserInfo, (<>), argument, auto,
   command, execParser, footerDoc, fullDesc, help, helper, info, long,
   metavar, option, optional, progDesc, short, str, subparser, switch )
import Options.Applicative.Builder ( InfoMod )
import Paths_ks_download ( version )
import Text.PrettyPrint.ANSI.Leijen ( string )

import KS.Clean.Closed.Options ( ClosedOptions (ClosedOptions) )
import KS.Clean.Old.Options ( OldOptions (OldOptions) )
import KS.Clean.Types ( ConfDir, Date, ShouldArchive )


data Options
   = Closed ClosedOptions
   | Old OldOptions


parseBeforeDate :: Parser (Maybe Date)
parseBeforeDate = optional . option auto $
   short 'b' <> long "before-date" <> metavar "YYYYMMDD" <>
   help "Check places inspected on or before this date. Default: 9 months ago"


parseArchive :: Parser ShouldArchive
parseArchive = switch $
   long "archive" <>
   help "Archive closed places. See ARCHIVING below."


parseConfDir :: Parser ConfDir
parseConfDir = argument str $
   metavar "CONFDIR" <>
   help "Directory containing ks-download.conf file"


parseClosed :: Parser Options
parseClosed = Closed <$> (ClosedOptions <$> parseArchive <*> parseConfDir)


parseOld :: Parser Options
parseOld = Old <$> (OldOptions <$> parseBeforeDate <*> parseArchive <*> parseConfDir)


parseOpts :: IO Options
parseOpts = execParser $ parseCommand `withInfo`
   "Tool for cleaning the Kitchen Snitch data" $ footerString commonFooter

   where
      parseCommand = subparser $
         command "closed" (parseClosed `withInfo`
            "Process new feedback on closed places" $ footerString commonFooter) <>
         command "old" (parseOld `withInfo`
            "Batch check old inspections against Places" $ footerString usageOld)


-- Convenience function to add --help support to anything
withInfo :: Parser a -> String -> InfoMod a -> ParserInfo a
withInfo opts desc usage = info (helper <*> opts) $ progDesc desc <> fullDesc <> usage


-- Helper function to construct usage strings
footerString :: [String] -> InfoMod a
footerString = footerDoc . Just . string . init . unlines


usageOld :: [String]
usageOld =
   [ "Looks up records in inspections_recent that are older the date above in Google Places. Reports the permanently closed status and optionally archives the closed places."
   , ""
   ] ++ usageArchiving ++ commonFooter


usageArchiving :: [String]
usageArchiving =
   [ "ARCHIVING"
   , ""
   , "Archiving means, for each Places ID of a closed establishment, this will be done:"
   , ""
   , "  - All inspections for that Places ID will be inserted into the inspections_archived collection"
   , "  - All inspections for that Places ID will be deleted from the inspections_all collection"
   , "  - The one inspection for that Places ID will be deleted from the inspections_recent collection"
   , ""
   ]


commonFooter :: [String]
commonFooter =
   [ "Version " ++ (showVersion version) ++ "  Dino Morelli <dino@ui3.info>"
   ]
