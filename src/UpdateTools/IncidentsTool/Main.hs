{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module Main where

import           Control.Monad               (liftM)
import           Control.Monad.IO.Class      (liftIO)
import qualified Data.ByteString.Char8 as BS
import           Data.Maybe                  (fromJust)
import           Data.String.Conversions     (convertString)
import qualified Data.Text             as T
import           Network.URI
    
import qualified Database.Incidents
import           Database.ScotTraffic
import           Types.Incident
import           Utils.HTTP                  (maybeOpenURI)

import           RSSParser
    
alertsURI    = fromJust . parseURI $ "http://trafficscotland.org/rss/feeds/currentincidents.aspx"
roadworksURI = fromJust . parseURI $ "http://trafficscotland.org/rss/feeds/roadworks.aspx"

roadworkTransform :: Incident -> Incident
roadworkTransform incident =
    incident { incidentDescription = fixedDescription }
  where
    fixedDescription = replaceZeros . replaceDelayInfo . incidentDescription $ incident
    replaceZeros = T.replace " - 00:00" ""
    replaceDelayInfo = T.replace "Delay Information: " ""


stringFromURI :: URI -> IO T.Text
stringFromURI uri = do
     content <- maybeOpenURI uri
     return $ case content of
         Just content -> convertString content
         _ -> ""

updateAlerts :: ScotTraffic ()
updateAlerts =
    liftIO fetchAlerts >>= Database.Incidents.storeAlerts
  where
    fetchAlerts = liftM parseAlerts . stringFromURI $ alertsURI
    
updateRoadworks :: ScotTraffic ()
updateRoadworks =
    liftIO fetchRoadworks >>= Database.Incidents.storeRoadworks
  where
    fetchRoadworks = liftM ((map roadworkTransform) . parseRoadworks) . stringFromURI $ roadworksURI

main = withScotTraffic "localhost" $ do
    updateAlerts
    updateRoadworks

