-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module KS.DLInsp.DHD.Downloader
  ( download )
  where

import Control.Concurrent ( threadDelay )
import Control.Lens ( (^.) )
import Control.Monad ( (>=>) )
import Data.ByteString.Lazy ( ByteString )
import Data.Either ( partitionEithers )
import Data.List ( isInfixOf, isPrefixOf )
import Data.Maybe ( fromJust, fromMaybe )
import Data.Monoid ( (<>) )
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import Data.Time.Calendar ( toGregorian )
-- import Debug.Trace ( trace )
import qualified KS.Data.Inspection as I
import Network.Wreq ( FormParam ((:=)), Response, get, post, responseBody )
import Text.HTML.TagSoup ( Tag (TagText), (~==), (~/=), fromAttrib,
  fromTagText, innerText, isTagClose, parseTags, partitions, sections )
import Text.Printf ( printf )
import Text.Regex ( subRegex, mkRegex )

import KS.DLInsp.DHD.Types ( DL, Downloader, Options ( optEndDate,
  optEstType, optPageLimit, optStartDate ), asks, liftIO, runDL )
import KS.Util ( Seconds (..), TryCount (Remaining), withRetry )


urlPrefix :: String
urlPrefix = "https://wake-nc.healthinspections.us/"


inspectionSrc :: String
inspectionSrc = "nc_wake"


download :: Downloader
download options destDir = runDL options $ do
  allPageUrls <- getPageUrls
  let pageCount = length allPageUrls
  pageLimit <- asks optPageLimit
  liftIO $ printf "Downloading %d of %d pages\n\n"
    (fromMaybe pageCount pageLimit) pageCount

  let pageUrls = maybe allPageUrls (\n -> take n allPageUrls) pageLimit

  let getters = map getFacilities pageUrls  -- [IO [Inspection]]
  liftIO $ mapM_ (\ml -> ml >>=
    mapM_ (I.saveInspection destDir >=> printf "Saved inspection file %s\n")) getters


reqToTags :: IO (Response ByteString) -> IO [Tag String]
reqToTags = fmap (parseTags . TL.unpack . TL.decodeUtf8 . (^. responseBody))


-- Get all (4) facilities from a page at the supplied URL
getFacilities :: String -> IO [I.Inspection]
getFacilities url = do
  -- 3 seconds
  threadDelay 3000000

  printf "Retrieving %s\n" url

  tags <- either error return =<< withRetry (Remaining 5) (Seconds 2)
    (reqToTags (get $ urlPrefix <> url)) putStrLn

  let itags = isolateInspTags tags
  putStrLn $ "Facilities on this page (should never be 0): " ++ (show . length $ itags)  -- FIXME
  (failures, successes) <- partitionEithers <$> mapM extractInsp itags
  mapM_ putStrLn failures
  return successes


-- Extract the block of tags containing each separate facility
isolateInspTags :: [Tag String] -> [[Tag String]]
isolateInspTags = partitions isFacAnchor where
  isFacAnchor e =
    (e ~== ("<a href>" :: String)) &&
    (isPrefixOf "facilities" $ fromAttrib "href" e) &&
    (not . elem '&' $ fromAttrib "href" e)


-- Extract the Inspection data from a facility's tags
extractInsp :: [Tag String] -> IO (Either String I.Inspection)
extractInsp tags = return $ makeInspection <$> I.parseDate date where
  makeInspection dateParsed = I.Inspection
    inspectionSrc
    (T.pack name)
    (T.pack . trim $ addr)
    dateParsed
    (read . trim $ score)
    violCount
    critCount
    (reinspToBool reinspection)
    (urlPrefix ++ detail)

  name = innerText . (takeWhile (not . isTagClose)) $ tags
  TagText addr = (dropWhile (~/= ("Location:" :: String)) tags) !! 2
  TagText date = (dropWhile (~/= ("Inspection Date:" :: String)) tags) !! 2
  TagText score = (dropWhile (~/= ("Score:" :: String)) tags) !! 2
  detailTag = head . (dropWhile (~/= ("<input type=button>" :: String))) $ tags
  reinspection = fromAttrib "value" detailTag
  detail = extractDetailUrl . fromAttrib "onclick" $ detailTag

  trim = unwords . words

  reinspToBool "Inspection" = False
  reinspToBool _            = True

  extractDetailUrl =
    takeWhile (/= '\'') . tail . dropWhile (/= '\'')


  -- This code is for extracting the violations data. Tricky!

  -- Table containing the violations
  vtable = (dropWhile (~/= ("<table cellpadding=2>" :: String))) $ tags

  -- Each violations table row. Each of these has 3 tdS
  allRowsInViolsTable = partitions (~== ("<tr>" :: String)) vtable

  -- Discard the first row, it's display labels
  allRowsMinusHeader = tail $ allRowsInViolsTable

  -- Discard the rows after the last violation
  vrows = takeWhile isViol allRowsMinusHeader
  isViol tags' = length (filter (~== ("General Comments" :: String)) tags') == 0

  -- Extract the violations and critical-ness from the remaining tags
  vs = map extractViolation vrows

  -- Count them up
  critCount = length . filter fst $ vs
  violCount = length vs


{- This code is pulling the violation full text which we are
   discarding at this time. But if we want it in the future, it's
   already being extracted here.
-}
extractViolation :: [Tag String] -> (Bool, String)
extractViolation tags = (crit, text)
  where
    crit = isInfixOf "red" $ fromAttrib "style" $ tags !! 6
    text = fromTagText $ tags !! 7


-- Get the URLs of all search result pages
getPageUrls :: DL [String]
getPageUrls = do
  liftIO $ putStrLn "\nRetrieving all page URLs"
  formParams <- searchParams
  etags <- liftIO $ withRetry (Remaining 5) (Seconds 2)
    (reqToTags (post (urlPrefix ++ "reports.cfm") formParams))
    putStrLn
  tags <- either error return etags
  return $ map (\i -> subRegex (mkRegex " ") i "%20") . map (fromAttrib "href" . head) .
    sections (~== ("<a class=teaser>" :: String)) $ tags


-- Used (in the distant past) for debugging to store the search results page
-- locally for inspection
{- FIXME
savePage :: IO ()
savePage = do
  src <- openURL mkPost
  writeFile "temp.html" src
-}


{- These things are for the first form post to get all of the page URLs
-}

fvAny, fvEmpty :: T.Text
fvAny = "Any"
fvEmpty = ""


searchParams :: DL [FormParam]
searchParams = do
  (sy, sm, sd) <- toGregorian . fromJust <$> asks optStartDate
  (ey, em, ed) <- toGregorian . fromJust <$> asks optEndDate

  estType <- asks optEstType
  liftIO $ printf "Retrieving inspections for establishment type: %s\n" estType

  return $
    [ "f" := ("search" :: T.Text)
    , "strSearch1" := fvEmpty
    , "relevance1" := ("fName" :: T.Text)
    , "strSearch2" := fvEmpty
    , "relevance2" := ("fName" :: T.Text)
    , "strSearch3" := fvEmpty
    , "relevance3" := ("fName" :: T.Text)
    , "lscore" := fvEmpty
    , "hscore" := fvEmpty
    , "ftype" := T.pack estType
    , "fzipcode" := fvAny
    , "rcritical" := fvAny
    , "sMonth" := sm
    , "sDay" := sd
    , "sYear" := sy
    , "eMonth" := em
    , "eDay" := ed
    , "eYear" := ey
    , "func" := ("Search" :: T.Text)
    ]
