-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

import qualified Data.Map as M
import Data.List ( intercalate )
import Data.Maybe ( listToMaybe )
import System.Environment ( getArgs, getProgName )
import System.Exit ( exitFailure )
import System.IO
   ( BufferMode ( NoBuffering )
   , hSetBuffering, stdout, stderr
   )

import Ks.DlInsp.Source.Common
import qualified Ks.DlInsp.Source.NcWake as NcWake


main :: IO ()
main = do
   -- No buffering, it messes with the order of output
   mapM_ (flip hSetBuffering NoBuffering) [ stdout, stderr ]

   (downloader', dir, pageLimit) <- getArgs >>= parseArgs

   downloader' dir pageLimit


downloaders :: M.Map String Downloader
downloaders = M.fromList
   [ ("nc_wake", NcWake.download)
   ]


parseArgs :: [String] -> IO (Downloader, FilePath, Maybe Int)
parseArgs (src : dir : limitL) = do
   dl <- maybe usage return $ M.lookup src downloaders
   return (dl, dir, read `fmap` (listToMaybe limitL))
parseArgs _                    = usage


usage :: IO a
usage = do
   appName <- getProgName
   putStrLn $ unlines
      [ "Acquire inspection date from a source"
      , ""
      , "Usage: " ++ appName ++ " SOURCE DESTDIR [PAGE_LIMIT]"
      , "       " ++ appName ++ " [OPTIONS]"
      , ""
      , "Options:"
      , "  -h, --help  This usage information"
      , ""
      , "SOURCE is one of: " ++ (intercalate ", " $ M.keys downloaders)
      , ""
      , "Writes successful downloads to DESTDIR"
      , "Logging is written to stdout."
      , ""
      , "Dino Morelli <dino@ui3.info>"
      ]
   exitFailure
