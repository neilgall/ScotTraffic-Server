{-# LANGUAGE OverloadedStrings #-}

module Scraper (scrape) where

import           Control.Applicative
import           Control.Error.Util
import           Control.Monad
import           Data.Maybe
import qualified Data.Text                     as T
import           Data.Text.Read
import           Text.XML.HXT.Core

import           Geodetics.LatLon
import           Types.SafetyCamera
import           Utils.RoadName
import           Utils.WebScraping

-- Base URL for the website
baseURL :: Text
baseURL = "http://www.safetycameras.gov.scot"


-- Scrape the Scottish Safety Camera Partnership website
-- Returns a list of SafetyCamera in IO context
scrape :: IO [SafetyCamera]
scrape = do
    areaLinks <- scrapeIndex "/cameras/safety-camera-locations/"
    councilLinks <- scrapeLinks areaLinks
    cameraLinks <- scrapeLinks councilLinks
    cameras <- sequence (map cameraFromPage cameraLinks)
    return $! catMaybes cameras

-- Given a sub-index link, scrape the page and extract the next level of links
scrapeLinks :: [Text] -> IO [Text]
scrapeLinks links = fmap concat (sequence $ map scrapeIndex links)

-- Load an HTML DOM from an absolute or relative URL. If the URL is relative,
-- it is prefixed with the base URL.
absoluteURL :: Text -> Text
absoluteURL path =
    if baseURL `T.isPrefixOf` path
    then path
    else baseURL `T.append` path


-- From an index page get the list of links to further pages
scrapeIndex :: Text -> IO [Text]
scrapeIndex path =
    runX $ loadHTML (absoluteURL path) //> indexLinks


-- HXT filter to find the links on the top-level index page
indexLinks :: ArrowXml a => a XmlTree Text
indexLinks =
    hasName "div" //> hasAttrValue "class" (== "active")
    //> hasName "a" >>> getAttrl /> getTextAsText

cameraFromPage :: Text -> IO (Maybe SafetyCamera)
cameraFromPage link = do
    fields <- dataFromPage link
    return $ cameraFromData link fields

dataFromPage :: Text -> IO [Text]
dataFromPage path =
    runX $ loadHTML (absoluteURL path) //> itemsFromPage

itemsFromPage :: ArrowXml a => a XmlTree Text
itemsFromPage =
    (itemFromPage "site" /> getBoldText)
    <+> (itemFromPage "siteStartLatitude" /> getTextAsText)
    <+> (itemFromPage "siteStartLongitude" /> getTextAsText)
    <+> (itemFromPage "siteEndLatitude" /> getTextAsText)
    <+> (itemFromPage "siteEndLongitude" /> getTextAsText)
    <+> speedLimitFromPage

itemFromPage :: ArrowXml a => Text -> a XmlTree XmlTree
itemFromPage name =
    deep (elementsWithAttributeValue "span" "id" (T.unpack name))

speedLimitFromPage :: ArrowXml a => a XmlTree Text
speedLimitFromPage =
    divsWithID "speedLimit" /> getBoldText

getBoldText :: ArrowXml a => a XmlTree Text
getBoldText = deep (hasName "strong" /> getTextAsText)

cameraFromData :: Text -> [Text] -> Maybe SafetyCamera
cameraFromData link fields =
    case fields of
        (title:slat:slon:elat:elon:speed:[]) ->
            let (road, title') = splitRoadNameFromTitle title
            in SpeedCamera link
                <$> (coordinate slat slon >>= validateCoordinate)
                <*> (coordinate elat elon >>= validateCoordinate)
                <*> title'
                <*> road
                <*> pure (speedLimit speed)
                <*> pure link
                <*> pure []
        _ -> Nothing

coordinate :: Text -> Text -> Maybe LatLon
coordinate lat lon =
    let parse c = fmap fst $ hush $ double c
    in LatLon <$> parse lat <*> parse lon <*> Just WGS84

validateCoordinate :: LatLon -> Maybe LatLon
validateCoordinate c@(LatLon lat lon _) =
  if validateLat lat && validateLon lon
  then (Just c)
  else Nothing
  where
    validateLat lat = 54.5 < lat && lat < 60.8
    validateLon lon = -7.6 < lon && lon < -0.8


-- Extract a SpeedLimit from the page's speed information string
speedLimit :: Text -> SpeedLimit
speedLimit text
    | find "20 mph" = SpeedLimit20
    | find "30 mph" = SpeedLimit30
    | find "40 mph" = SpeedLimit40
    | find "50 mph" = SpeedLimit50
    | find "60 mph" = SpeedLimit60
    | find "70 mph" = SpeedLimit70
    | find "National" = NationalSpeedLimit
    | otherwise = SpeedLimitUnknown
    where
        find x = x `T.isInfixOf` text
