{-# LANGUAGE OverloadedStrings #-}

module Bridges.Forth (getBridgeStatus) where

import qualified Codec.Binary.UTF8.String   as UTF8
import           Control.Applicative
import           Control.Monad                        (join, liftM)
import           Data.Aeson
import           Data.Either.Combinators              (rightToMaybe)
import qualified Data.ByteString.Lazy       as BS
import           Data.Maybe
import qualified Data.Text                  as T
import           Text.XML.HXT.Core

import           Geodetics.LatLon
import           Types.BridgeStatus
import           Utils.HTTP
import           Utils.WebScraping
import qualified Debug.Trace                as DBG

bridgeStatusURL = "http://www.forthroadbridge.org"
windSpeedURL = "http://forthroadbridge-dev.blueprintwebtech.com/Umbraco/Api/Windspeed/GetWindspeed"
coordinate = LatLon 56.000925 (-3.404131) WGS84

getBridgeStatus :: IO (Maybe BridgeStatus)
getBridgeStatus = do
    response <- maybeOpenURL bridgeStatusURL
    case response of
        Nothing ->
            return Nothing
            
        Just page -> do
            status <- runX $ readString [withValidate  no,
                                         withWarnings  no,
                                         withParseHTML yes] (stringFromByteString page)
                             >>> bridgeStatus
            windSpeed <- getWindSpeed
            let message = listToMaybe status
            return $ BridgeStatus "forthRoadBridge" "Forth Road Bridge" "A90" coordinate <$> message <*> Just windSpeed

stringFromByteString = UTF8.decode . BS.unpack

bridgeStatus = asidesWithClass "panel-bridge windspeed"
               //> divsWithClass "row-1"
               //> hasName "p"
               //> getText
               >>. map T.pack

instance FromJSON WindSpeed where
    parseJSON (Object json) = WindSpeed
                                <$> fmap read (json .: "Maximum")
                                <*> fmap read (json .: "Average")
                                <*> fmap T.pack (json .: "Direction")
                                <*> fmap T.pack (json .: "LastUpdated")

getWindSpeed :: IO (Maybe WindSpeed)
getWindSpeed = do
    response <- maybeOpenURL windSpeedURL
    return $ response >>= decode
