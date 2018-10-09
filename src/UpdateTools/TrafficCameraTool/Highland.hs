{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module Highland (getCameraList) where

import           Control.Applicative
import           Data.List.Split
import           Data.Maybe                     (catMaybes)
import qualified Data.Text              as T
import           Data.Time.Format
import           Text.Read                      (readMaybe)
import           Text.XML.HXT.Core

import           Geodetics.LatLon
import           Types.TrafficCamera
import           Utils.AtomicFile
import           Utils.RoadName
import           Utils.WebScraping

baseURL = "http://www.travelhighland.info" :: T.Text

getCameraList :: IO [TrafficCamera]
getCameraList = do
    let tableURL = T.append baseURL "/public/cctv_tab.htm"
    index <- runX $ loadHTML tableURL >>> parseCCTVTab
    return $ catMaybes $ map cameraFromIndexEntry (chunksOf 3 index)


parseCCTVTab :: ArrowXml a => a XmlTree T.Text
parseCCTVTab = divsWithClass "yui-dt"
                >>> elementsWithAttributeValue "tbody" "id" "cctvtable"
                //> hasName "tr"
                //> hasName "td"
                //> removeAllWhiteSpace
                >>> (getLinkTargets <+> getTextAsText)
                >>. map T.strip
                
cameraFromIndexEntry :: [T.Text] -> Maybe TrafficCamera
cameraFromIndexEntry (link:name:source:_) =
    if source == "Traffic Scotland" then
        -- we get these elsewhere
        Nothing
    else
        StaticURLCamera <$> filename <*> locationName <*> roadName <*> direction <*> image <*> latlon <*> timestamp
    where
        parameters = parametersFromLink link
        identifier = lookup "id" parameters
        filename = T.append <$> identifier <*> Just ".jpg"
        locationName = snd . splitRoadNameFromTitle $ name
        roadName = fst . splitRoadNameFromTitle $ name
        direction = Just None
        image = T.append baseURL <$> (T.append "/public/cctv.htm?i=" <$> identifier)
        latlon = LatLon <$> lat <*> lon <*> Just WGS84
        lat = lookup "lat" parameters >>= readMaybe . T.unpack
        lon = lookup "lng" parameters >>= readMaybe . T.unpack
        timestamp = parseTimeM True defaultTimeLocale "%s" "0"

parametersFromLink :: T.Text -> [(T.Text, T.Text)]
parametersFromLink link = map fromArray parameterPairs
    where
        parameterPairs = map (T.split (== '=')) parameters
        parameters = T.split (== '&') query
        query = (T.split (== '?') link) !! 1
        fromArray (k:v:_) = (k,v)

