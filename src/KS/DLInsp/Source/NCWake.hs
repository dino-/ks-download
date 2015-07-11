-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

module KS.DLInsp.Source.NCWake
   where

import Data.List ( intercalate, isInfixOf, isPrefixOf )
import Data.Maybe ( fromMaybe )
import qualified Data.Text as T
import Data.Time.Calendar ( toGregorian )
--import Debug.Trace ( trace )
import Network.HTTP
import Text.HTML.TagSoup
import Text.Printf ( printf )

import KS.DLInsp.Types
import qualified KS.Inspection as I


urlPrefix :: String
urlPrefix = "http://wake-nc.healthinspections.us/"


inspectionSrc :: String
inspectionSrc = "nc_wake"


download :: Downloader
download options = runDL options $ do
   allPageUrls <- getPageUrls
   let pageCount = length allPageUrls
   pageLimit <- asks optPageLimit
   liftIO $ printf "Downloading %d of %d pages\n\n"
      (fromMaybe pageCount pageLimit) pageCount

   let pageUrls = maybe allPageUrls (\n -> take n allPageUrls) pageLimit

   let getters = map getFacilities pageUrls  -- [IO [Inspection]]
   dir <- asks optDestDir
   liftIO $ mapM_ (\ml -> ml >>= mapM_ (I.saveInspection dir)) getters


-- Get all (4) facilities from a page at the supplied URL
getFacilities :: String -> IO [I.Inspection]
getFacilities url = do
   printf "Retrieving %s\n" url

   tags <- parseTags `fmap` (openURL . getRequest $ urlPrefix ++ url)

   let itags = isolateInspTags tags
   return $ map extractInsp itags


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
getPageUrls :: DL [String]
getPageUrls = do
   post <- mkPost
   tags <- liftIO $ parseTags `fmap` openURL post
   return $ map (fromAttrib "href" . head) .
      sections (~== "<a class=teaser>") $ tags


-- Used for debugging to store the search results page locally
-- for inspection
{- FIXME
savePage :: IO ()
savePage = do
   src <- openURL mkPost
   writeFile "temp.html" src
-}


-- Retrieve a page
openURL :: HStream b => Request b -> IO b
openURL p = getResponseBody =<< simpleHTTP p


{- These things are for the first form post to get all of the page URLs
-}

mkPost :: DL Request_String
mkPost = do
   ps <- searchParams
   return $ postRequestWithBody
      (urlPrefix ++ "reports.cfm")
      "application/x-www-form-urlencoded"
      $ intercalate "&" ps


searchParams :: DL [String]
searchParams = do
   (sy, sm, sd) <- toGregorian `fmap` asks optStartDate
   (ey, em, ed) <- toGregorian `fmap` asks optEndDate

   return $
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
      , "sMonth=" ++ (show sm)
      , "sDay=" ++ (show sd)
      , "sYear=" ++ (show sy)
      , "eMonth=" ++ (show em)
      , "eDay=" ++ (show ed)
      , "eYear=" ++ (show ey)
      , "func=Search"
      ]
