-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

import Control.Monad ( when )
import qualified Data.Map as M
import Data.Maybe ( fromJust )
import Data.Version ( showVersion )
import Paths_ks_download ( version )
import System.Environment ( getArgs )
import System.Exit ( exitFailure, exitSuccess )
import System.IO
   ( BufferMode ( NoBuffering )
   , hSetBuffering, stdout, stderr
   )
import Text.Printf ( printf )

import KS.DLInsp.Opts
import KS.DLInsp.Source.Downloaders
import KS.SourceConfig ( SourceConfig (timeZone), loadConfig )
import KS.Util ( setDates )


main :: IO ()
main = do
   -- No buffering, it messes with the order of output
   mapM_ (flip hSetBuffering NoBuffering) [ stdout, stderr ]

   (options, args) <- getArgs >>= parseOpts
   when (optHelp options) $ putStrLn usageText >> exitSuccess
   when (length args < 3) $ putStrLn usageText >> exitFailure
   let (confDir : source : destDir : _) = args

   putStrLn $ "ks-dlinsp version " ++ (showVersion version) ++ " started"

   -- We need to get the source config to see its time zone to
   -- supply proper values for optStartDate and optEndDate
   sourceConfig <- loadConfig confDir source
   fixedOptions <- setDates (timeZone sourceConfig) options

   printf "Downloading inspections between dates %s and %s\n"
      (show . fromJust . optStartDate $ fixedOptions)
      (show . fromJust . optEndDate $ fixedOptions)

   let mbDownloader = M.lookup source downloaders
   maybe (putStrLn usageText >> exitFailure)
      (\dl -> dl fixedOptions destDir) mbDownloader
