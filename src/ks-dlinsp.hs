-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

import Control.Monad ( when )
import qualified Data.Map as M
import Data.Version ( showVersion )
import Paths_ks_download ( version )
import System.Environment ( getArgs )
import System.Exit ( exitFailure, exitSuccess )
import System.IO
   ( BufferMode ( NoBuffering )
   , hSetBuffering, stdout, stderr
   )

import KS.DLInsp.Opts
import KS.DLInsp.Source.Downloaders


main :: IO ()
main = do
   -- No buffering, it messes with the order of output
   mapM_ (flip hSetBuffering NoBuffering) [ stdout, stderr ]

   (options, args) <- getArgs >>= parseOpts
   when (optHelp options) $ putStrLn usageText >> exitSuccess
   when (length args < 3) $ putStrLn usageText >> exitFailure

   let (confDir : source : destDir : _) = args

   putStrLn $ "ks-dlinsp version " ++ (showVersion version) ++ " started"

   let mbDownloader = M.lookup source downloaders
   maybe (putStrLn usageText >> exitFailure)
      (\dl -> dl options destDir) mbDownloader
