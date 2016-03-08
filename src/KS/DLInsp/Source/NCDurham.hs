-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

{-# LANGUAGE OverloadedStrings #-}

module KS.DLInsp.Source.NCDurham
   where

import           Control.Lens ( (^.), (?~), (.~) , (&) )
import qualified Data.ByteString.Lazy.Char8 as BL
import           Data.Either ( partitionEithers )
import           Data.List ( isPrefixOf, tails )
import qualified Data.Text as T
import           Data.Time.Calendar ( Day, toGregorian )
--import           Debug.Trace ( trace )
import           Network.HTTP ( urlDecode )
import           Network.Wreq
                  ( FormParam ((:=)), Response
                  , defaults, header, httpProxy, proxy, responseBody
                  )
import qualified Network.Wreq as Wreq
import qualified Network.Wreq.Session as S
import           Text.HTML.TagSoup
import           Text.Printf ( printf )

import qualified KS.Data.Inspection as I
import           KS.DLInsp.Types ( DL, Downloader,
                  Options ( optEndDate, optPageLimit, optStartDate ),
                  asks, liftIO, runDL )
--import           KS.Util ( withRetry )


{- development notes

   General notes

   To examine the body of a response, use this:

      print $ someResponse ^. responseBody

   Often it's helpful to print out what Tagsoup's parseTags function is really generating:

      mapM_ print $ parseTags someUglyPage


   Older notes, clean these up

   - For each establishment type (first crack, just est type 1)
      - Get the form results for the days we want
      - Get the inspection details urls from all pages
      - For each establishment
         - Visit its details page
         - Scrape the most recent inspection
         - Save out as JSON using the Inspection type
-}



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
charlesProxy :: Wreq.Options -> Wreq.Options
charlesProxy = proxy ?~ httpProxy "127.0.0.1" 8888


-- Set some headers
opts :: Wreq.Options
opts = defaults
   & header "User-Agent" .~ ["Mozilla/5.0 (X11; Linux x86_64; rv:44.0) Gecko/20100101 Firefox/44.0"]


{-
download :: Downloader
download options destDir = runDL options $ do
   -- We need these for building the search params further down
   sday <- asks optStartDate
   eday <- asks optEndDate

   liftIO $ S.withSession $ \sess -> do
      putStrLn "GET start page"
      rStart <- S.getWith opts sess url

      let (vsGenStart, vsStart) = viewStateFromResponse rStart

      putStrLn "POST submitting establishment type and date range"
      rSearch <- S.postWith opts sess url $ searchParams
         "ctl00_PageContent_INSPECTIONFilterButton__Button"
         sday eday vsGenStart vsStart

      let vsSearch = viewStateFromBars rSearch
      let eets = extractEstEventTargets rSearch
      printf "Number of establishments found: %d\n" (length eets)
      eis <- concat <$> mapM (retrieveEstablishment sess vsGenStart vsSearch) eets

      let (failures, successes) = partitionEithers eis

      mapM_ (\emsg -> putStrLn $ "ERROR parsing inspection: " ++ emsg) failures
      mapM_ (I.saveInspection destDir) successes

      return ()

   return ()
-}


download :: Downloader
download options destDir = runDL options $ do
   -- We need these for building the search params further down
   sday <- asks optStartDate
   eday <- asks optEndDate

   liftIO $ S.withSession $ \sess -> do
      putStrLn "GET start page"
      rStart <- S.getWith opts sess url

      processEstablishmentPage 
         "ctl00_PageContent_INSPECTIONFilterButton__Button"
         sday eday sess rStart destDir

   return ()


processEstablishmentPage :: String -> Maybe Day -> Maybe Day -> S.Session
   -> Response BL.ByteString -> FilePath -> IO ()
processEstablishmentPage eventTarget sday eday sess rPrior destDir = do
      let (vsGenPrior, vsPrior) = viewStateFromResponse rPrior

      putStrLn "POST to get page of establishments"
      rCurrent <- S.postWith opts sess url $ searchParams
         eventTarget sday eday vsGenPrior vsPrior

      let vsCurrent = viewStateFromBars rCurrent
      let eets = extractEstEventTargets rCurrent
      printf "Number of establishments found on current page: %d\n" (length eets)
      eis <- concat <$> mapM (retrieveEstablishment sess vsGenPrior vsCurrent) eets

      let (failures, successes) = partitionEithers eis

      mapM_ (\emsg -> putStrLn $ "ERROR parsing inspection: " ++ emsg) failures
      mapM_ (I.saveInspection destDir) successes

      -- FIXME Check the status of the next-page control and recurse if necessary
      -- processEstablishmentPage eventTarget? sday eday sess rCurrent destDir
      return ()


retrieveEstablishment
   :: S.Session
   -> String -> String
   -> String
   -> IO [Either String I.Inspection]
retrieveEstablishment
   sess
   vsGenStart vsSearch
   eventTarget = do

   putStrLn "POST to retrieve one establishment"
   rEstRedir <- S.postWith opts sess url
      $ searchParams eventTarget Nothing Nothing vsGenStart vsSearch
   let estUrl = printf "%s%s" urlPrefix $ urlFromBars rEstRedir

   rEst <- S.getWith opts sess estUrl
   let (vsGenEst, vsEst) = viewStateFromResponse rEst

   {- FIXME Without the `take 1` this is ALL of the inspections. Most
      often we want just the first one, BUT needs to be
      switchable(?) for gathering the initial pile. Is it worth
      designing this switchability into the code?
   -}
   let iets = take 1 . extractInspEventTargets $ rEst

   mapM (retrieveInspection sess estUrl vsGenEst vsEst) iets


retrieveInspection :: S.Session -> String -> String -> String -> String
   -> IO (Either String I.Inspection)
retrieveInspection sess url' vsGenEst vsEst eventTarget = do
   putStrLn "POST to retrieve one inspection"
   rInspRedir <- S.postWith opts sess url'
      $ inspRowParams vsGenEst vsEst eventTarget
   let inspUrl = printf "%s%s" urlPrefix $ urlFromBars rInspRedir

   -- Get the inspection page and extract data from it
   rInsp <- S.getWith opts sess inspUrl
   extractInspection inspUrl $ parseTags . BL.unpack $ rInsp ^. responseBody


extractInspEventTargets :: Response BL.ByteString -> [String]
extractInspEventTargets resp =
   map extractWithinSQuotes
   . map (fromAttrib "href")
   . map head
   . sections (~== patAVio)
   . parseTags
   . BL.unpack
   $ resp ^. responseBody

   where
      patAVio :: String
      patAVio = "<a title=\"Show Violations\">"


extractEstEventTargets :: Response BL.ByteString -> [String]
extractEstEventTargets resp =
   map extractWithinSQuotes
   . map (fromAttrib "href")
   . map head
   . sections (~== patAInsp)
   . parseTags
   . BL.unpack
   $ resp ^. responseBody

   where
      patAInsp :: String
      patAInsp = "<a title=\"Inspection for this establishment\">"


extractWithinSQuotes :: String -> String
extractWithinSQuotes = takeWhile (/= '\'') . tail . dropWhile (/= '\'')


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


searchParams :: String -> Maybe Day -> Maybe Day -> String -> String -> [FormParam]
searchParams eventTarget startDay endDay viewStateGenerator viewState =
   [ "ctl00$scriptManager1" := ("ctl00$PageContent$UpdatePanel1|ctl00$PageContent$INSPECTIONFilterButton$_Button" :: T.Text)
   , "__EVENTTARGET" := T.pack eventTarget
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

   where
      formatDay :: Maybe Day -> T.Text
      formatDay (Just day) = T.pack $ printf "%d/%d/%d" m d y
         where (y, m, d) = toGregorian day
      formatDay Nothing    = fvEmpty


inspRowParams :: String -> String -> String -> [FormParam]
inspRowParams viewStateGenerator viewState eventTarget =
   [ "ctl00$scriptManager1" := ("ctl00$PageContent$UpdatePanel1|ctl00$PageContent$ESTABLISHMENTTableControlRepeater$ctl03$Button$_Button" :: T.Text)
   , "__EVENTTARGET" := T.pack eventTarget
   , "__EVENTARGUMENT" := fvEmpty
   , "ctl00$pageLeftCoordinate" := fvEmpty
   , "ctl00$pageTopCoordinate" := fvEmpty
   , "ctl00$PageContent$_clientSideIsPostBack" := ("N" :: T.Text)
   , "ctl00$PageContent$ESTABLISHMENTPagination$_CurrentPage" := (1 :: Int)
   , "ctl00$PageContent$ESTABLISHMENTPagination$_PageSize" := (10 :: Int)
   , "__ASYNCPOST" := ("true" :: T.Text)
   , "__VIEWSTATEGENERATOR" := T.pack viewStateGenerator
   , "__VIEWSTATE" := T.pack viewState
   ]


extractInspection :: String -> [Tag String] -> IO (Either String I.Inspection)
extractInspection detailUrl tags = do
   putStrLn $ "Parsing inspection data for " ++ name
   parsed <- I.parseDate dateStr
   return $ makeInspection <$> parsed

   where
      makeInspection dateParsed = I.Inspection
         inspectionSrc
         (T.pack name)
         (T.pack . trim $ ((trim addr) ++ ", " ++ csz))
         dateParsed
         (read . trim $ score)
         (length violations)
         (length . filter (== True) $ violations)
         False  -- We can't determine reinspections from this system
         detailUrl

      name = innerText . take 1 . drop 3
         . dropWhile (~/= TagText ("Name" :: String)) $ tags
      addr = innerText . take 1 . drop 3
         . dropWhile (~/= TagText ("Address" :: String)) $ tags
      csz = innerText . takeWhile (~/= ("</tr>" :: String)) . drop 8
         . dropWhile (~/= TagText ("City/State/ZIP" :: String)) $ tags
      dateStr = innerText . take 1 . drop 5
         . dropWhile (~/= TagText ("Inspection Date" :: String)) $ tags
      score = innerText . take 1 . drop 9
         . dropWhile (~/= TagText ("Final Score @ Grade" :: String)) $ tags

      violations =
         map isCrit
         . map head
         . map (drop 2)
         . tail
         . sections (~== ("<tr>" :: String))
         . takeWhile (~/= ("</table>" :: String))
         . dropWhile (~/= ("<td class=tre>" :: String))
         $ tags

      isCrit = isPrefixOf ";0000FF" . reverse . fromAttrib "style"

      trim = unwords . words
