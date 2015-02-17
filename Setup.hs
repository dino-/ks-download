#! /usr/bin/env runhaskell

-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

import Distribution.Simple ( defaultMainWithHooks, postCopy,
   simpleUserHooks )
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.Setup ( CopyFlags (..), Flag (..) )
import System.Directory ( copyFile )
import System.FilePath


copyBinDestDir :: CopyFlags -> LocalBuildInfo -> FilePath
copyBinDestDir copyFlags localBuildInfo = destDir
   where
      copyPrefix = case copyDest copyFlags of
         Flag NoCopyDest  -> ""
         Flag (CopyTo cp) -> cp ++ "/"
         {- Can't use </> here because instPrefix is often absolute
            and </> returns the second path if ALONE if it's absolute.
            Safer to just have the extra / if that's how it goes.
         -}

      instPrefix = fromPathTemplate . prefix
         . installDirTemplates $ localBuildInfo

      destDir = copyPrefix ++ instPrefix </> "bin"


copyCustomBinFiles :: FilePath -> IO ()
copyCustomBinFiles destDir = do
   -- src path info
   let srcFile = "ks-dl-nightly.sh"
   let srcDir = "util"

   putStrLn $ "Installing additional binaries in " ++ destDir

   copyFile (srcDir </> srcFile) (destDir </> srcFile)


main = defaultMainWithHooks (simpleUserHooks
   { postCopy = customPostCopy
   } )

   where
      customPostCopy _ copyFlags _ localBuildInfo =
         copyCustomBinFiles (copyBinDestDir copyFlags localBuildInfo)
