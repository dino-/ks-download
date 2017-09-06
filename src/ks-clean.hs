-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

import System.IO
   ( BufferMode ( NoBuffering )
   , hSetBuffering, stdout, stderr
   )

import qualified KS.Clean.Old.Command as Old
import KS.Clean.Options ( Options (Old), parseOpts )


main :: IO ()
main = do
   -- No buffering, it messes with the order of output
   mapM_ (flip hSetBuffering NoBuffering) [ stdout, stderr ]

   command <- parseOpts
   case command of
      --Closed closedOpts -> Closed.run closedOpts
      Old oldOpts -> Old.run oldOpts
      _ -> undefined
