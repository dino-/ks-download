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

   (options, _) <- getArgs >>= parseOpts

   when (optHelp options) $ do
      putStrLn usageText
      exitSuccess

   when (optDestDir options == "") $ do
      putStrLn usageText
      exitFailure

   putStrLn $ "ks-dlinsp version " ++ (showVersion version) ++ " started"

   let mbDownloader = M.lookup (optSource options) downloaders
   maybe (putStrLn usageText >> exitFailure)
      (\dl -> dl options) mbDownloader
