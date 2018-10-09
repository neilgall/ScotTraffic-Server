{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module RSSParser (parseAlerts, parseRoadworks) where

import           Control.Applicative
import           Control.Monad              (liftM)
import qualified Data.ByteString.Lazy as BS
import           Data.Time.Clock            (UTCTime)
import           Data.Time.Format           (parseTimeM, defaultTimeLocale)
import           Data.Maybe                 (catMaybes, listToMaybe)
import           Data.String.Conversions    (convertString)
import qualified Data.Text            as T
import           Network.URI                (parseURI)
import           Text.Feed.Import           (parseFeedString)
import           Text.Feed.Types
import           Text.Read                  (readMaybe)
import qualified Text.RSS.Syntax      as RSS
import           Text.XML.Light.Types       (QName(..), Element(..))
import           Text.XML.Light.Output      (showContent)

import           Geodetics.LatLon
import           Types.Incident
import           Utils.RoadName

georssPoint = QName "point" (Just "http://www.georss.org/georss") (Just "georss")
rssDateFormat = "%a, %d %b %Y %H:%M:%S %Z"

parseAlerts = parseRSS alertFromItem
parseRoadworks = parseRSS roadworkFromItem 

type FromItem = RSS.RSSItem -> Maybe Incident        

parseRSS :: FromItem -> T.Text -> [Incident]
parseRSS fromItem str =
    case parseFeedString . convertString $ str of
        Just feed -> incidentsFromFeed fromItem feed
        _ -> []


incidentsFromFeed :: FromItem -> Feed -> [Incident]
incidentsFromFeed fromItem (RSSFeed rss) = catMaybes . map fromItem . RSS.rssItems . RSS.rssChannel $ rss
incidentsFromFeed _ _ = []

    
alertFromItem :: RSS.RSSItem -> Maybe Alert
alertFromItem RSS.RSSItem{..} = do
    (roadName, description) <- fmap (splitRoadNameFromTitle . T.pack) rssItemDescription
    Incident <$> fmap T.pack rssItemTitle
             <*> roadName
             <*> description
             <*> (contentForQName georssPoint rssItemOther >>= parseCoord)
             <*> (rssItemPubDate >>= parseDate)
             <*> (rssItemLink >>= parseURI)

roadworkFromItem :: RSS.RSSItem -> Maybe Roadwork
roadworkFromItem RSS.RSSItem{..} = do
    (roadName, title) <- fmap (splitRoadNameFromTitle . T.pack) rssItemTitle
    Incident <$> title
             <*> roadName
             <*> fmap (unHTML . T.pack) rssItemDescription
             <*> (contentForQName georssPoint rssItemOther >>= parseCoord)
             <*> (rssItemPubDate >>= parseDate)
             <*> (rssItemLink >>= parseURI)


contentForQName :: QName -> [Element] -> Maybe String
contentForQName qname elements =
    let matchesQName e = (elName e) == qname
        elementContent = concat . map showContent . elContent
        concatContent = concat . map elementContent
     in
        case filter matchesQName elements of
            [] -> Nothing
            es -> Just $ concatContent es


parseCoord :: String -> Maybe LatLon
parseCoord str = let (lat, lon) = break (== ' ') str
                  in LatLon <$> readMaybe lat <*> readMaybe lon <*> Just WGS84


parseDate :: String -> Maybe UTCTime
parseDate = parseTimeM True defaultTimeLocale rssDateFormat


unHTML :: T.Text -> T.Text
unHTML = T.replace "<br />" "\n"
