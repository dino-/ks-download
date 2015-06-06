-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

module KS.Log
   ( initLogging, lname
   , logStartMsg, logStopMsg
   , line

   -- Re-exported from System.Log
   , Priority (..), debugM, infoM, noticeM, warningM, errorM
   , criticalM, alertM, emergencyM
   )
   where

import Data.Time ( defaultTimeLocale, formatTime, getCurrentTime
   , utcToLocalZonedTime )
import System.IO ( stdout )
import System.Log.Handler.Simple ( streamHandler )
import System.Log.Logger


lname :: String
lname = "normal-output"


{- Set up logging
-}
initLogging :: Priority -> IO ()
initLogging logPriority = do
   -- Remove the root logger's default handler that writes every
   -- message to stderr!
   updateGlobalLogger rootLoggerName removeHandler

   -- Set up our logger
   h <- streamHandler stdout DEBUG
   updateGlobalLogger lname $ setHandlers [h]
   updateGlobalLogger lname $ setLevel logPriority


logStartMsg :: String -> IO ()
logStartMsg name = do
   l <- getLogger name
   let displayLevel = maybe "NO LEVEL SET" show $ getLevel l
   timeString <- timeStamp
   noticeM name $ timeString ++ "  Logging started, Priority: "
      ++ displayLevel


logStopMsg :: String -> IO ()
logStopMsg name = do
   timeString <- timeStamp
   noticeM name $ timeString ++ "  Logging stopped"


timeStamp :: IO String
timeStamp = do
   local <- utcToLocalZonedTime =<< getCurrentTime 
   return $ formatTime defaultTimeLocale "%F %T %Z" local


line :: String
line = replicate 70 '-'
