-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

module Ks.DlInsp.Source.NcWake
   where

import Data.List ( intercalate, isInfixOf, isPrefixOf )
import Data.Maybe ( fromMaybe )
import qualified Data.Text as T
--import Debug.Trace ( trace )
import Network.HTTP
import Text.HTML.TagSoup
import Text.Printf ( printf )

import qualified Ks.Inspection as I
import Ks.DlInsp.Source.Common


urlPrefix :: String
urlPrefix = "http://wake.digitalhealthdepartment.com/"


inspectionSrc :: String
inspectionSrc = "nc_wake"


download :: Downloader
download dir pageLimit = do
   allPageUrls <- getPageUrls
   let pageCount = length allPageUrls
   printf "Downloading %d of %d pages\n\n"
      (fromMaybe pageCount pageLimit) pageCount

   let pageUrls = maybe allPageUrls (\n -> take n allPageUrls) pageLimit

   let getters = map getFacilities pageUrls  -- [IO [Inspection]]
   mapM_ (\ml -> ml >>= mapM_ (I.saveInspection dir)) getters


-- Get all (4) facilities from a page at the supplied URL
getFacilities :: String -> IO [I.Inspection]
getFacilities url = do
   printf "Retrieving %s\n" url

   tags <- parseTags `fmap` (openURL . getRequest $ urlPrefix ++ url)

   let itags = isolateInspTags tags
   let insps = map extractInsp itags
   return $ map I.setId insps


-- Extract the block of tags containing each separate facility
isolateInspTags :: [Tag String] -> [[Tag String]]
isolateInspTags= partitions isFacAnchor
   where isFacAnchor e =
            (e ~== "<a href>") &&
            (isPrefixOf "facilities" $ fromAttrib "href" e) &&
            (not . elem '&' $ fromAttrib "href" e)


-- Extract the Inspection data from a facility's tags
extractInsp :: [Tag String] -> I.Inspection
extractInsp tags = I.Inspection
   ""  -- Will fill in _id later, once this record is complete
   inspectionSrc
   (T.pack name)
   (T.pack . trim $ addr)
   (I.parseDate date)
   (read . trim $ score)
   violCount
   critCount
   (reinspToBool reinspection)
   (urlPrefix ++ detail)

   where
      name = innerText . (takeWhile (not . isTagClose)) $ tags
      TagText addr = (dropWhile (~/= "Location:") tags) !! 2
      TagText date = (dropWhile (~/= "Inspection Date:") tags) !! 2
      TagText score = (dropWhile (~/= "Score:") tags) !! 2
      detailTag = head . (dropWhile (~/= "<input type=button>")) $ tags
      reinspection = fromAttrib "value" detailTag
      detail = extractDetailUrl . fromAttrib "onclick" $ detailTag

      trim = unwords . words

      reinspToBool "Inspection" = False
      reinspToBool _            = True

      extractDetailUrl =
         takeWhile (/= '\'') . tail . dropWhile (/= '\'')


      -- This code is for extracting the violations data. Tricky!

      -- Table containing the violations
      vtable = (dropWhile (~/= "<table cellpadding=2>")) $ tags

      -- Each violations table row. Each of these has 3 tdS
      allRowsInViolsTable = partitions (~== "<tr>") vtable

      -- Discard the first row, it's display labels
      allRowsMinusHeader = tail $ allRowsInViolsTable

      -- Discard the rows after the last violation
      vrows = takeWhile isViol allRowsMinusHeader
      isViol tags' = length (filter (~== "General Comments") tags') == 0

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
getPageUrls :: IO [String]
getPageUrls = do
   tags <- parseTags `fmap` openURL mkPost
   return $ map (fromAttrib "href" . head) .
      sections (~== "<a class=teaser>") $ tags


-- Used for debugging to store the search results page locally
-- for inspection
savePage :: IO ()
savePage = do
   src <- openURL mkPost
   writeFile "temp.html" src


-- Retrieve a page
openURL :: HStream b => Request b -> IO b
openURL p = getResponseBody =<< simpleHTTP p


{- These things are for the first form post to get all of the page URLs
-}

mkPost :: Request_String
mkPost = postRequestWithBody
   (urlPrefix ++ "reports.cfm")
   "application/x-www-form-urlencoded"
   $ intercalate "&" searchParams


searchParams :: [String]
searchParams =
   [ "f=search"
   , "strSearch1="
   , "relevance1=fName"
   , "strSearch2="
   , "relevance2=fName"
   , "strSearch3="
   , "relevance3=fName"
   , "lscore="
   , "hscore="
   , "ftype=Restaurant"
   , "fzipcode=Any"
   , "rcritical=Any"
   , "sMonth=8"
   , "sDay=20"
   , "sYear=2014"
   , "eMonth=1"
   , "eDay=19"
   , "eYear=2015"
   , "func=Search"
   ]