-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

{-# LANGUAGE OverloadedStrings #-}

module KS.DLInsp.CDP.NCDurham
   where

import           Control.Lens ( (^.), (?~), (.~) , (&) )
import Control.Monad.Reader
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
import           KS.DLInsp.CDP.Types ( DL, Downloader,
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


data ScrapeST = ScrapeST
   { destDir :: FilePath
   , startDay :: Maybe Day
   , endDay :: Maybe Day
   , estType :: EstablishmentType
   , session :: S.Session
   }

type Scrape a = (ReaderT ScrapeST IO) a

runScrape :: ScrapeST -> Scrape () -> IO ()
runScrape env ev = runReaderT ev env


download :: Downloader
download options destDir' = runDL options $ do
   -- We need these for building the search params further down
   startDay' <- asks optStartDate
   endDay' <- asks optEndDate

   liftIO $ S.withSession $ \sess -> do
      let initialState = ScrapeST destDir' startDay' endDay' et01Restaurant sess
      runScrape initialState processEstType

   return ()


processEstType :: Scrape ()
processEstType = do
   session' <- asks session

   rStart <- liftIO $ do
      putStrLn "GET start page"
      S.getWith opts session' url

   processEstablishmentPage =<< (searchParams "ctl00_PageContent_INSPECTIONFilterButton__Button" $ viewStateFromResponse rStart)

   return ()


processEstablishmentPage :: [FormParam] -> Scrape ()
processEstablishmentPage params = do
      destDir' <- asks destDir
      session' <- asks session

      liftIO $ putStrLn "POST to get page of establishments"
      rCurrent <- liftIO $ S.postWith opts session' url $ params

      let vsCurrent = viewStateFromBars rCurrent
      let eets = extractEstEventTargets rCurrent
      liftIO $ printf "Number of establishments found on current page: %d\n" (length eets)
      eis <- concat <$> mapM (retrieveEstablishment Latest vsCurrent) eets

      let (failures, successes) = partitionEithers eis

      liftIO $ mapM_ (\emsg -> putStrLn $ "ERROR parsing inspection: " ++ emsg) failures
      liftIO $ mapM_ (I.saveInspection destDir') successes

      if (hasNextPage rCurrent)
         then pagingParams vsCurrent >>= processEstablishmentPage
         else return ()


hasNextPage :: Response BL.ByteString -> Bool
hasNextPage resp =
   not . isDisabled
   . head
   . dropWhile (~/= ("<input title=\"Next page\">" :: String))
   . parseTags
   . BL.unpack
   $ resp ^. responseBody

   where
      isDisabled tag = case (fromAttrib "disabled" tag) of
         "" -> False
         _  -> True


data Scope = Latest | FirstPage

retrieveEstablishment :: Scope -> String -> String
   -> Scrape [Either String I.Inspection]
retrieveEstablishment Latest = retrieveEstablishment' (take 1)
retrieveEstablishment FirstPage = retrieveEstablishment' id


retrieveEstablishment' :: ([String] -> [String]) -> String -> String
   -> Scrape [Either String I.Inspection]
retrieveEstablishment' limitF vsSearch eventTarget = do
   session' <- asks session
   liftIO $ putStrLn "POST to retrieve one establishment"
   searchFormFields <- searchParams eventTarget vsSearch
   rEstRedir <- liftIO $ S.postWith opts session' url searchFormFields
   let estUrl = printf "%s%s" urlPrefix $ urlFromBars rEstRedir

   rEst <- liftIO $ S.getWith opts session' estUrl
   let vsEst = viewStateFromResponse rEst

   let iets = limitF . extractInspEventTargets $ rEst

   mapM (retrieveInspection estUrl vsEst) iets


retrieveInspection :: String -> String -> String
   -> Scrape (Either String I.Inspection)
retrieveInspection url' vsEst eventTarget = do
   session' <- asks session
   liftIO $ putStrLn "POST to retrieve one inspection"
   rInspRedir <- liftIO $ S.postWith opts session' url'
      $ inspRowParams vsEst eventTarget
   let inspUrl = printf "%s%s" urlPrefix $ urlFromBars rInspRedir

   -- Get the inspection page and extract data from it
   rInsp <- liftIO $ S.getWith opts session' inspUrl
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


viewStateFromResponse :: Response BL.ByteString -> String
viewStateFromResponse response =
   getValueForID "<input id=__VIEWSTATE"
   . parseTags
   . BL.unpack
   $ response ^. responseBody

   where
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


searchParams :: String -> String -> Scrape [FormParam]
searchParams eventTarget viewState = do
   startDay' <- asks startDay
   endDay' <- asks endDay
   estType' <- asks estType
   return
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
      , "ctl00$PageContent$EST_TYPE_IDFilter" := estType'
      , "ctl00$PageContent$INSPECTION_DATEFromFilter" := formatDay startDay'
      , "ctl00$PageContent$INSPECTION_DATEToFilter" := formatDay endDay'
      , "ctl00$PageContent$FINAL_SCOREFromFilter" := fvAny
      , "ctl00$PageContent$COUNTY_IDFilter" := countyID
      , "ctl00$PageContent$ESTABLISHMENTPagination$_CurrentPage" := (1 :: Int)
      , "ctl00$PageContent$ESTABLISHMENTPagination$_PageSize" := (10 :: Int)
      , "hiddenInputToUpdateATBuffer_CommonToolkitScripts" := (1 :: Int)
      , "__ASYNCPOST" := ("true" :: T.Text)
      , "__VIEWSTATEGENERATOR" := fvEmpty
      , "__VIEWSTATE" := T.pack viewState
      ]


pagingParams :: String -> Scrape [FormParam]
pagingParams viewState = do
   startDay' <- asks startDay
   endDay' <- asks endDay
   estType' <- asks estType

   return
      [ "ctl00$scriptManager1" := ("ctl00$PageContent$UpdatePanel1|ctl00$PageContent$ESTABLISHMENTPagination$_NextPage" :: T.Text)
      , "__EVENTTARGET" := fvEmpty
      , "__EVENTARGUMENT" := fvEmpty
      , "__LASTFOCUS" := ("ctl00_PageContent_ESTABLISHMENTPagination__NextPage" :: T.Text)
      , "ctl00$pageLeftCoordinate" := fvEmpty
      , "ctl00$pageTopCoordinate" := fvEmpty
      , "ctl00$PageContent$_clientSideIsPostBack" := ("Y" :: T.Text)
      , "ctl00$PageContent$ESTABLISHMENTSearch" := fvEmpty
      , "ctl00$PageContent$PREMISE_CITYFilter1" := fvEmpty
      , "ctl00$PageContent$PREMISE_NAMEFilter" := fvAny
      , "ctl00$PageContent$PREMISE_CITYFilter" := fvAny
      , "ctl00$PageContent$PREMISE_ZIPFilter" := fvEmpty
      , "ctl00$PageContent$EST_TYPE_IDFilter" := estType'
      , "ctl00$PageContent$INSPECTION_DATEFromFilter" := formatDay startDay'
      , "ctl00$PageContent$INSPECTION_DATEToFilter" := formatDay endDay'
      , "ctl00$PageContent$FINAL_SCOREFromFilter" := fvAny
      , "ctl00$PageContent$COUNTY_IDFilter" := countyID
      , "ctl00$PageContent$ESTABLISHMENTPagination$_CurrentPage" := (1 :: Int)
      , "ctl00$PageContent$ESTABLISHMENTPagination$_PageSize" := (10 :: Int)
      , "hiddenInputToUpdateATBuffer_CommonToolkitScripts" := (1 :: Int)
      , "__ASYNCPOST" := ("true" :: T.Text)
      , "__VIEWSTATEGENERATOR" := fvEmpty
      , "__VIEWSTATE" := T.pack viewState
      , "ctl00$PageContent$ESTABLISHMENTPagination$_NextPage.x" := (7 :: Int)
      , "ctl00$PageContent$ESTABLISHMENTPagination$_NextPage.y" := (12 :: Int)
      ]


formatDay :: Maybe Day -> T.Text
formatDay (Just day) = T.pack $ printf "%d/%d/%d" m d y
   where (y, m, d) = toGregorian day
formatDay Nothing    = fvEmpty


inspRowParams :: String -> String -> [FormParam]
inspRowParams viewState eventTarget =
   [ "ctl00$scriptManager1" := ("ctl00$PageContent$UpdatePanel1|ctl00$PageContent$ESTABLISHMENTTableControlRepeater$ctl03$Button$_Button" :: T.Text)
   , "__EVENTTARGET" := T.pack eventTarget
   , "__EVENTARGUMENT" := fvEmpty
   , "ctl00$pageLeftCoordinate" := fvEmpty
   , "ctl00$pageTopCoordinate" := fvEmpty
   , "ctl00$PageContent$_clientSideIsPostBack" := ("N" :: T.Text)
   , "ctl00$PageContent$ESTABLISHMENTPagination$_CurrentPage" := (1 :: Int)
   , "ctl00$PageContent$ESTABLISHMENTPagination$_PageSize" := (10 :: Int)
   , "__ASYNCPOST" := ("true" :: T.Text)
   , "__VIEWSTATEGENERATOR" := fvEmpty
   , "__VIEWSTATE" := T.pack viewState
   ]


extractInspection :: String -> [Tag String] -> Scrape (Either String I.Inspection)
extractInspection detailUrl tags = do
   liftIO $ putStrLn $ "Parsing inspection data for " ++ name
   parsed <- liftIO $ I.parseDate dateStr
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
