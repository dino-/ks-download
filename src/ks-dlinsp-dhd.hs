-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

import Control.Monad ( when )
import Data.Maybe ( fromJust )
import Data.Version ( showVersion )
import Paths_ks_download ( version )
import System.Environment ( getArgs, setEnv )
import System.Exit ( exitFailure, exitSuccess )
import System.IO
   ( BufferMode ( NoBuffering )
   , hSetBuffering, stdout, stderr
   )
import Text.Printf ( printf )

import KS.DLInsp.NCWake.Opts ( Options (optEndDate, optHelp, optStartDate)
   , parseOpts, usageText )
import KS.DLInsp.NCWake.Downloader ( download )
import KS.DLInsp.Util ( setDates )
import KS.SourceConfig ( SourceConfig (timeZone), loadConfig )


main :: IO ()
main = do
   -- No buffering, it messes with the order of output
   mapM_ (flip hSetBuffering NoBuffering) [ stdout, stderr ]

   (options, args) <- getArgs >>= parseOpts
   when (optHelp options) $ putStrLn usageText >> exitSuccess
   when (length args < 3) $ putStrLn usageText >> exitFailure
   let (confDir : source : destDir : _) = args

   putStrLn $ "ks-dlinsp-dhd version " ++ (showVersion version) ++ " started"

   -- We need to get the source config to see its time zone to
   -- supply proper values for optStartDate and optEndDate
   sourceConfig <- loadConfig confDir source
   setEnv "TZ" $ timeZone sourceConfig
   (newStartDate, newEndDate) <- setDates (confDir, source)
      (optStartDate options) (optEndDate options)
   let fixedOptions = options { optStartDate = newStartDate, optEndDate = newEndDate }

   printf "Downloading inspections between dates %s and %s\n"
      (show . fromJust . optStartDate $ fixedOptions)
      (show . fromJust . optEndDate $ fixedOptions)

   download fixedOptions destDir
