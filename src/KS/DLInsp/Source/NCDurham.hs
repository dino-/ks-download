-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

{-# LANGUAGE OverloadedStrings #-}

module KS.DLInsp.Source.NCDurham
   where

-- New wreq stuff
import           Control.Lens
--import           Data.Aeson.Lens ( _String, key )
import qualified Data.ByteString.Lazy.Char8 as BL
import           Data.List ( isPrefixOf, tails )
import qualified Data.Text as T
import           Data.Time.Calendar ( Day, toGregorian )
import           Debug.Trace ( trace )
import           Network.Wreq hiding ( options )  -- FIXME explicit list!
import qualified Network.Wreq.Session as S
import           Text.HTML.TagSoup
import           Text.Printf ( printf )

--import qualified Data.ByteString as BS
--import qualified Data.ByteString.Char8 as C8
--import qualified Data.ByteString.Lazy as BL
--import           Data.Either ( partitionEithers )
--import           Data.List ( intercalate, isInfixOf, isPrefixOf )
--import           Data.Maybe ( fromJust, fromMaybe )
--import qualified Data.Text as T
--import           Data.Time.Calendar ( toGregorian )
--import           Debug.Trace ( trace )
--import qualified KS.Data.Inspection as I
{-import           Network.HTTP ( HStream, Request, Request_String,
                  getResponseBody, getRequest, postRequestWithBody,
                  simpleHTTP )
-}
{-import           Text.HTML.TagSoup ( Tag (TagText), (~==), (~/=), fromAttrib,
                  fromTagText, innerText, isTagClose, parseTags, partitions,
                  sections )
-}
--import           Text.Printf ( printf )

import           KS.DLInsp.Types ( DL, Downloader,
                  Options ( optEndDate, optPageLimit, optStartDate ),
                  asks, liftIO, runDL )
--import           KS.Util ( withRetry )


{-
urlPrefix :: String
urlPrefix = "https://public.cdpehs.com/NCENVPBL/ESTABLISHMENT/ShowESTABLISHMENTTablePage.aspx?ESTTST_CTY=32"
-}


inspectionSrc :: String
inspectionSrc = "nc_durham"


countyID :: Int
countyID = 32  -- Durham county, NC


url :: String
url = printf "https://public.cdpehs.com/NCENVPBL/ESTABLISHMENT/ShowESTABLISHMENTTablePage.aspx?ESTTST_CTY=%d" countyID


type EstablishmentType = Int

et01Restaurant, et02FoodStands, et03MobileFood, et04PushCarts :: EstablishmentType

et01Restaurant = 1
et02FoodStands = 2
et03MobileFood = 3
et04PushCarts  = 4


fvAny, fvEmpty :: T.Text
fvAny = "--ANY--"
fvEmpty = ""


userAgentMoz :: Network.Wreq.Options -> Network.Wreq.Options
userAgentMoz = header "User-Agent" .~ ["Mozilla/5.0 (X11; Linux x86_64; rv:44.0) Gecko/20100101 Firefox/44.0"]


{-
   - For each establishment type (first crack, just est type 1)
      - Get the form results for the days we want
      - Get the inspection details urls from all pages
      - For each establishment
         - Visit its details page
         - Scrape the most recent inspection
         - Save out as JSON using the Inspection type
-}

download :: Downloader
download options destDir = runDL options $ do
   -- We need these for building the search params further down
   sday <- asks optStartDate
   eday <- asks optEndDate

   liftIO $ S.withSession $ \sess -> do
      putStrLn "Start page GET"
      let firstOpts = defaults
            -- & proxy ?~ httpProxy "127.0.0.1" 8888
            & userAgentMoz
      startPage <- S.getWith firstOpts sess url

      --print $ startPage ^. responseBody

      -- We get this first to set the cookie business and get the
      -- __VIEWSTATE .Net values. Oh, look there's that view state now:
      {- let (viewStateGenerator, viewState) =
            extractViewState $ startPage ^. responseBody -}
            --extractViewState . BL.unpack $ startPage ^. responseBody
      -- We'll use these along with the dates above to build the search params

      -- FWIW, log this stuff
      --printf "viewStateGenerator: %s\n" viewStateGenerator
      --printf "viewState: %s...\n" (take 60 viewState)

      -- Set some headers
      let opts = defaults
            -- & proxy ?~ httpProxy "127.0.0.1" 8888
            & userAgentMoz
            & header "Origin" .~ ["https://public.cdpehs.com"]
            & header "Referer" .~ ["https://public.cdpehs.com/NCENVPBL/ESTABLISHMENT/ShowESTABLISHMENTTablePage.aspx?ESTTST_CTY=32"]
            & header "X-Requested-With" .~ ["XMLHttpRequest"]
            & header "X-MicrosoftAjax" .~ ["Delta=true"]

      let vs = extractViewState startPage

      putStrLn "Changing establishment type POST"
      r1 <- S.postWith opts sess url
         $ establishmentParams vs

      putStrLn "Changing date range POST"
      r2 <- S.postWith opts sess url
         -- $ dateRangeParams sday eday vs
         -- $ dateRangeParams sday eday $ extractViewState r1
         $ dateRangeParams sday eday $ (fst vs, viewStateFromBars r1)
      --print $ r2 ^. responseBody
      --print $ extractViewState r1

      putStrLn "Extracting view state from the bars now"
      let r2VS = viewStateFromBars r2
      --writeFile "viewstate" r2VS
      putStrLn "Finished extracting view state"
      print $ take 20 $ r2VS

      putStrLn "Establishment view button POST"
      r3 <- S.postWith opts sess url
         $ estRowParams sday eday (fst vs, r2VS)
         -- $ estRowParams vs
      print $ r3 ^. responseBody

      return ()

   return ()


viewStateFromBars :: Response BL.ByteString -> String
viewStateFromBars resp =
   takeWhile (/= '|')
   . tail
   . dropWhile (/= '|')
   . head
   . dropWhile (not . isPrefixOf "__VIEWSTATE")
   . tails
   . BL.unpack
   $ resp ^. responseBody


estRowParams :: Maybe Day -> Maybe Day -> (String, String) -> [FormParam]
estRowParams startDay endDay (viewStateGenerator, viewState) =
   [ "ctl00$scriptManager1" := ("ctl00$PageContent$UpdatePanel1|ctl00$PageContent$ESTABLISHMENTTableControlRepeater$ctl01$Button$_Button" :: T.Text)
   --[ "ctl00$scriptManager1" := ("ctl00$PageContent$UpdatePanel1|ctl00$PageContent$INSPECTIONFilterButton$_Button" :: T.Text)
   , "__EVENTTARGET" := ("ctl00$PageContent$ESTABLISHMENTTableControlRepeater$ctl01$Button$_Button" :: T.Text)
   , "__EVENTARGUMENT" := fvEmpty
   , "__LASTFOCUS" := ("ctl00_PageContent_ESTABLISHMENTTableControlRepeater_ctl01_Button__Button" :: T.Text)
   , "ctl00$pageLeftCoordinate" := fvEmpty
   , "ctl00$pageTopCoordinate" := fvEmpty
   , "ctl00$PageContent$_clientSideIsPostBack" := ("Y" :: T.Text)
   , "ctl00$PageContent$ESTABLISHMENTSearch" := fvEmpty
   , "ctl00$PageContent$PREMISE_CITYFilter1" := fvEmpty
   , "ctl00$PageContent$PREMISE_NAMEFilter" := fvAny
   , "ctl00$PageContent$PREMISE_CITYFilter" := fvAny
   , "ctl00$PageContent$PREMISE_ZIPFilter" := fvEmpty
   , "ctl00$PageContent$EST_TYPE_IDFilter" := et01Restaurant
   , "ctl00$PageContent$INSPECTION_DATEFromFilter" := formatDay startDay
   , "ctl00$PageContent$INSPECTION_DATEToFilter" := formatDay endDay
   , "ctl00$PageContent$FINAL_SCOREFromFilter" := fvAny
   , "ctl00$PageContent$COUNTY_IDFilter" := countyID
   , "ctl00$PageContent$ESTABLISHMENTPagination$_CurrentPage" := (1 :: Int)
   , "ctl00$PageContent$ESTABLISHMENTPagination$_PageSize" := (10 :: Int)
   , "hiddenInputToUpdateATBuffer_CommonToolkitScripts" := (1 :: Int)
   , "__ASYNCPOST" := ("true" :: T.Text)
   , "__VIEWSTATEGENERATOR" := T.pack viewStateGenerator
   , "__VIEWSTATE" := T.pack viewState
   ]


establishmentParams :: (String, String) -> [FormParam]
establishmentParams (viewStateGenerator, viewState) =
   [ "ctl00$scriptManager1" := ("ctl00$PageContent$UpdatePanel1|ctl00$PageContent$INSPECTIONFilterButton$_Button" :: T.Text)
   , "__EVENTTARGET" := ("ctl00$PageContent$EST_TYPE_IDFilter" :: T.Text)
   , "__EVENTARGUMENT" := fvEmpty
   , "__LASTFOCUS" := ("ctl00$PageContent$EST_TYPE_IDFilter" :: T.Text)
   , "ctl00$pageLeftCoordinate" := fvEmpty
   , "ctl00$pageTopCoordinate" := fvEmpty
   , "ctl00$PageContent$_clientSideIsPostBack" := ("Y" :: T.Text)
   , "ctl00$PageContent$ESTABLISHMENTSearch" := fvEmpty
   , "ctl00$PageContent$PREMISE_CITYFilter1" := fvEmpty
   , "ctl00$PageContent$PREMISE_NAMEFilter" := fvAny
   , "ctl00$PageContent$PREMISE_CITYFilter" := fvAny
   , "ctl00$PageContent$PREMISE_ZIPFilter" := fvEmpty
   , "ctl00$PageContent$EST_TYPE_IDFilter" := et01Restaurant
   , "ctl00$PageContent$INSPECTION_DATEFromFilter" := fvEmpty
   , "ctl00$PageContent$INSPECTION_DATEToFilter" := fvEmpty
   , "ctl00$PageContent$FINAL_SCOREFromFilter" := fvAny
   , "ctl00$PageContent$COUNTY_IDFilter" := countyID
   , "ctl00$PageContent$ESTABLISHMENTPagination$_CurrentPage" := (1 :: Int)
   , "ctl00$PageContent$ESTABLISHMENTPagination$_PageSize" := (10 :: Int)
   , "hiddenInputToUpdateATBuffer_CommonToolkitScripts" := (1 :: Int)
   , "__ASYNCPOST" := ("true" :: T.Text)
   , "__VIEWSTATEGENERATOR" := T.pack viewStateGenerator
   , "__VIEWSTATE" := T.pack viewState
   ]


dateRangeParams :: Maybe Day -> Maybe Day -> (String, String) -> [FormParam]
dateRangeParams startDay endDay (viewStateGenerator, viewState) =
   [ "ctl00$scriptManager1" := ("ctl00$PageContent$UpdatePanel1|ctl00$PageContent$INSPECTIONFilterButton$_Button" :: T.Text)
   , "__EVENTTARGET" := ("ctl00_PageContent_INSPECTIONFilterButton__Button" :: T.Text)
   , "__EVENTARGUMENT" := fvEmpty
   , "__LASTFOCUS" := ("ctl00_PageContent_INSPECTIONFilterButton__Button" :: T.Text)
   , "ctl00$pageLeftCoordinate" := fvEmpty
   , "ctl00$pageTopCoordinate" := fvEmpty
   , "ctl00$PageContent$_clientSideIsPostBack" := ("Y" :: T.Text)
   , "ctl00$PageContent$ESTABLISHMENTSearch" := fvEmpty
   , "ctl00$PageContent$PREMISE_CITYFilter1" := fvEmpty
   , "ctl00$PageContent$PREMISE_NAMEFilter" := fvAny
   , "ctl00$PageContent$PREMISE_CITYFilter" := fvAny
   , "ctl00$PageContent$PREMISE_ZIPFilter" := fvEmpty
   , "ctl00$PageContent$EST_TYPE_IDFilter" := et01Restaurant
   , "ctl00$PageContent$INSPECTION_DATEFromFilter" := formatDay startDay
   , "ctl00$PageContent$INSPECTION_DATEToFilter" := formatDay endDay
   , "ctl00$PageContent$FINAL_SCOREFromFilter" := fvAny
   , "ctl00$PageContent$COUNTY_IDFilter" := countyID
   , "ctl00$PageContent$ESTABLISHMENTPagination$_CurrentPage" := (1 :: Int)
   , "ctl00$PageContent$ESTABLISHMENTPagination$_PageSize" := (10 :: Int)
   , "hiddenInputToUpdateATBuffer_CommonToolkitScripts" := (1 :: Int)
   , "__ASYNCPOST" := ("true" :: T.Text)
   , "__VIEWSTATEGENERATOR" := T.pack viewStateGenerator
   , "__VIEWSTATE" := T.pack viewState
   ]


formatDay :: Maybe Day -> T.Text
formatDay (Just day) = T.pack $ printf "%d/%d/%d" m d y
   where (y, m, d) = toGregorian day
formatDay Nothing    = fvEmpty



extractViewState :: Response BL.ByteString -> (String, String)
extractViewState response = (viewStateGenerator tags, viewState tags)
--extractViewState response = (viewStateGenerator (trace (show . take 5 $ tags) tags), viewState tags)
   where
      content = response ^. responseBody

      viewState = getValueForID "<input id=__VIEWSTATE"
      viewStateGenerator = getValueForID "<input id=__VIEWSTATEGENERATOR"

      tags = parseTags . BL.unpack $ content

      getValueForID :: String -> [Tag String] -> String
      getValueForID pat = fromAttrib "value" . head . dropWhile (~/= pat)
{-
extractViewState :: BL.ByteString -> (String, String)
extractViewState content = (viewStateGenerator tags, viewState tags)
   where
      viewState = getValueForID "<input id=__VIEWSTATE"
      viewStateGenerator = getValueForID "<input id=__VIEWSTATEGENERATOR"

      tags = parseTags . BL.unpack $ content

      getValueForID :: String -> [Tag String] -> String
      getValueForID pat = fromAttrib "value" . head . dropWhile (~/= pat)
-}
{-
extractViewState :: String -> (String, String)
extractViewState content = (viewStateGenerator tags, viewState tags)
   where
      viewState = getValueForID "<input id=__VIEWSTATE"
      viewStateGenerator = getValueForID "<input id=__VIEWSTATEGENERATOR"

      tags = parseTags content

      getValueForID :: String -> [Tag String] -> String
      getValueForID pat = fromAttrib "value" . head . dropWhile (~/= pat)
-}


{-

This downloader is from the (failed) old code using http-conduit

download :: Downloader
download options destDir = runDL options $ do
   params <- searchParams
   liftIO $ do
      --print params

      req <- urlEncodedBody params <$> (setHeaders =<< parseUrl urlPrefix)
      print req
      res <- httpLbs req =<< newManager tlsManagerSettings

      print res
      --responseBody res >>= BL.putStr
      --writeFile "temp.html" . show . responseBody $ res
      --writeFile "temp.html" $ responseBody res

   return ()

   where
      setHeaders req = return $ req
         { requestHeaders =
            [ ("X-Requested-With", "XMLHttpRequest")
            , ("X-MicrosoftAjax", "Delta=true")
            , ("Content-Type", "application/x-www-form-urlencoded; charset=utf-8")
            ]
         }
-}

{-

This downloader is from the old-OLD Wake county stuff

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
-}


-- Get all (4) facilities from a page at the supplied URL
{-
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
extractInsp tags = do
   parsed <- I.parseDate date
   return $ makeInspection <$> parsed

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
-}


{- This code is pulling the violation full text which we are
   discarding at this time. But if we want it in the future, it's
   already being extracted here.
-}
{-
extractViolation :: [Tag String] -> (Bool, String)
extractViolation tags = (crit, text)
   where
      crit = isInfixOf "red" $ fromAttrib "style" $ tags !! 6
      text = fromTagText $ tags !! 7
-}


-- Get the URLs of all search result pages
{-
getPageUrls :: DL [String]
getPageUrls = do
   liftIO $ putStrLn "Retrieving all page URLs"
   post <- mkPost
   dlResult <- liftIO $ withRetry 3 1 (parseTags `fmap` openURL post) putStrLn
   tags <- either error return dlResult
   return $ map (fromAttrib "href" . head) .
      sections (~== "<a class=teaser>") $ tags
-}


-- Used for debugging to store the search results page locally
-- for inspection
{-
savePage :: IO ()
savePage = do
   src <- openURL mkPost
   writeFile "temp.html" src
-}


{-
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
-}


{-
searchParams :: DL [(BS.ByteString, BS.ByteString)]
searchParams = do
   (sy, sm, sd) <- toGregorian . fromJust <$> asks optStartDate
   (ey, em, ed) <- toGregorian . fromJust <$> asks optEndDate
   let sdate = C8.pack $ printf "%d/%d/%d" sm sd sy
   let edate = C8.pack $ printf "%d/%d/%d" em ed ey

   return $
      [ ("ctl00$scriptManager1", "\"ctl00$PageContent$UpdatePanel1|ctl00$PageContent$INSPECTIONFilterButton$_Button\"")
      , ("__EVENTTARGET", "\"ctl00$PageContent$INSPECTIONFilterButton$_Button\"")
      , ("__EVENTARGUMENT", "")
      , ("__LASTFOCUS", "\"ctl00_PageContent_INSPECTIONFilterButton__Button\"")
      , ("ctl00$pageLeftCoordinate", "")
      , ("ctl00$pageTopCoordinate", "")
      --, ("ctl00$PageContent$_clientSideIsPostBack", "Y")
      , ("ctl00$PageContent$_clientSideIsPostBack", "N")
      , ("ctl00$PageContent$ESTABLISHMENTSearch", "")
      , ("ctl00$PageContent$PREMISE_CITYFilter1", "")
      , ("ctl00$PageContent$PREMISE_NAMEFilter1", "--ANY--")
      , ("ctl00$PageContent$PREMISE_CITYFilter", "--ANY--")
      , ("ctl00$PageContent$PREMISE_ZIPFilter1", "")
      --, ("ctl00$PageContent$EST_TYPE_IDFilter", "--ANY--")
      , ("ctl00$PageContent$EST_TYPE_IDFilter", "1")
      , ("ctl00$PageContent$INSPECTION_DATEFromFilter", sdate)
      , ("ctl00$PageContent$INSPECTION_DATEToFilter", edate)
      , ("ctl00$PageContent$FINAL_SCOREFromFilter", "--ANY--")
      , ("ctl00$PageContent$COUNTY_IDFilter", "32")
      , ("ctl00$PageContent$ESTABLISHMENTPagination$_CurrentPage", "1")
      , ("ctl00$PageContent$ESTABLISHMENTPagination$_PageSize", "10")
      , ("hiddenInputToUpdateATBuffer_CommonToolkitScripts", "1")
      , ("__ASYNCPOST", "true")
      --, ("__VIEWSTATEGENERATOR", "324C8509")
      ]
-}
