-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

module KS.DLInsp.DHD.Downloader
   ( download )
   where

import           Data.Either ( partitionEithers )
import           Data.List ( intercalate, isInfixOf, isPrefixOf )
import           Data.Maybe ( fromJust, fromMaybe )
import qualified Data.Text as T
import           Data.Time.Calendar ( toGregorian )
--import           Debug.Trace ( trace )
import qualified KS.Data.Inspection as I
import           Network.HTTP ( HStream, Request, Request_String,
                  getResponseBody, getRequest, postRequestWithBody,
                  simpleHTTP )
import           Text.HTML.TagSoup ( Tag (TagText), (~==), (~/=), fromAttrib,
                  fromTagText, innerText, isTagClose, parseTags, partitions,
                  sections )
import           Text.Printf ( printf )

import           KS.DLInsp.DHD.Types ( DL, Downloader,
                  Options ( optEndDate, optPageLimit, optStartDate ),
                  asks, liftIO, runDL )
import           KS.Util ( withRetry )


urlPrefix :: String
urlPrefix = "http://wake-nc.healthinspections.us/"


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
   liftIO $ mapM_ (\ml -> ml >>= mapM_ (I.saveInspection destDir)) getters


-- Get all (4) facilities from a page at the supplied URL
getFacilities :: String -> IO [I.Inspection]
getFacilities url = do
   printf "Retrieving %s\n" url

   tags <- either error return =<< withRetry 3 1
      (parseTags `fmap` (openURL . getRequest $ urlPrefix ++ url)) putStrLn

   let itags = isolateInspTags tags
   (failures, successes) <- partitionEithers <$> mapM extractInsp itags
   mapM_ putStrLn failures
   return successes


-- Extract the block of tags containing each separate facility
isolateInspTags :: [Tag String] -> [[Tag String]]
isolateInspTags= partitions isFacAnchor
   where isFacAnchor e =
            (e ~== "<a href>") &&
            (isPrefixOf "facilities" $ fromAttrib "href" e) &&
            (not . elem '&' $ fromAttrib "href" e)


-- Extract the Inspection data from a facility's tags
extractInsp :: [Tag String] -> IO (Either String I.Inspection)
extractInsp tags = return $ makeInspection <$> I.parseDate date

   where
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
   liftIO $ putStrLn "Retrieving all page URLs"
   post <- mkPost
   dlResult <- liftIO $ withRetry 3 1 (parseTags `fmap` openURL post) putStrLn
   tags <- either error return dlResult
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
   (sy, sm, sd) <- toGregorian . fromJust <$> asks optStartDate
   (ey, em, ed) <- toGregorian . fromJust <$> asks optEndDate

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
