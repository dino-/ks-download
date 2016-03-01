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
--import           Debug.Trace ( trace )
import           Network.HTTP ( urlDecode )
import           Network.Wreq hiding ( options )  -- FIXME explicit list!
import qualified Network.Wreq.Session as S
import           Text.HTML.TagSoup
import           Text.Printf ( printf )

--import qualified KS.Data.Inspection as I
import           KS.DLInsp.Types ( DL, Downloader,
                  Options ( optEndDate, optPageLimit, optStartDate ),
                  asks, liftIO, runDL )
--import           KS.Util ( withRetry )


inspectionSrc :: String
inspectionSrc = "nc_durham"


countyID :: Int
countyID = 32  -- Durham county, NC


urlPrefix :: String
urlPrefix = "https://public.cdpehs.com"


url :: String
url = printf "%s/NCENVPBL/ESTABLISHMENT/ShowESTABLISHMENTTablePage.aspx?ESTTST_CTY=%d" urlPrefix countyID


type EstablishmentType = Int

et01Restaurant, et02FoodStands, et03MobileFood, et04PushCarts :: EstablishmentType

et01Restaurant = 1
et02FoodStands = 2
et03MobileFood = 3
et04PushCarts  = 4


fvAny, fvEmpty :: T.Text
fvAny = "--ANY--"
fvEmpty = ""


-- This is for debugging
charlesProxy :: Network.Wreq.Options -> Network.Wreq.Options
charlesProxy = proxy ?~ httpProxy "127.0.0.1" 8888


-- Set some headers
opts :: Network.Wreq.Options
opts = defaults
   & header "User-Agent" .~ ["Mozilla/5.0 (X11; Linux x86_64; rv:44.0) Gecko/20100101 Firefox/44.0"]



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
      putStrLn "GET start page"
      rStart <- S.getWith opts sess url

      let (viewStateGenerator, vsStart) = viewStateFromResponse rStart

      putStrLn "POST submitting establishment type and date range"
      rSearch <- S.postWith opts sess url
         $ dateRangeParams sday eday viewStateGenerator vsStart

      {- FIXME Need code here to fish out each of the button IDs
      -}

      putStrLn "POST to retrieve one establishment"
      rEstRedir <- S.postWith opts sess url
         $ estRowParams sday eday viewStateGenerator (viewStateFromBars rSearch)
      --print $ rEstRedir ^. responseBody
      --print $ urlFromBars rEstRedir
      let estUrl = printf "%s/%s" urlPrefix $ urlFromBars rEstRedir
      --putStrLn estUrl

      rEst <- S.getWith opts sess estUrl
      print $ rEst ^. responseBody

      {-
      putStrLn "POST to retrieve one inspection"
      rInsp <- S.postWith opts sess url
         $ estInspParams sday eday viewStateGenerator (viewStateFromBars rEst)
      print $ rInsp ^. responseBody
      -}

      return ()

   return ()


urlFromBars :: Response BL.ByteString -> String
urlFromBars resp =
   urlDecode
   . reverse
   . takeWhile (/= '|')
   . tail
   . reverse
   . BL.unpack
   $ resp ^. responseBody


viewStateFromResponse :: Response BL.ByteString -> (String, String)
viewStateFromResponse response = (viewStateGenerator tags, viewState tags)
   where
      content = response ^. responseBody

      viewState = getValueForID "<input id=__VIEWSTATE"
      viewStateGenerator = getValueForID "<input id=__VIEWSTATEGENERATOR"

      tags = parseTags . BL.unpack $ content

      getValueForID :: String -> [Tag String] -> String
      getValueForID pat = fromAttrib "value" . head . dropWhile (~/= pat)


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


dateRangeParams :: Maybe Day -> Maybe Day -> String -> String -> [FormParam]
dateRangeParams startDay endDay viewStateGenerator viewState =
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


estRowParams :: Maybe Day -> Maybe Day -> String -> String -> [FormParam]
estRowParams startDay endDay viewStateGenerator viewState =
   [ "ctl00$scriptManager1" := ("ctl00$PageContent$UpdatePanel1|ctl00$PageContent$ESTABLISHMENTTableControlRepeater$ctl03$Button$_Button" :: T.Text)
   , "__EVENTTARGET" := ("ctl00$PageContent$ESTABLISHMENTTableControlRepeater$ctl03$Button$_Button" :: T.Text)
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
