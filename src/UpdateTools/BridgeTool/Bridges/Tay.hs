{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module Bridges.Tay (getBridgeStatus) where

import Control.Monad
import Data.Aeson
import Data.Aeson.Parser
import Data.Either.Combinators            (rightToMaybe)
import Data.HashMap.Strict       as H
import Data.Maybe
import Data.Text                 as T
import Data.Time.Clock
import Data.Time.Format

import Geodetics.LatLon
import Types.BridgeStatus
import Utils.HTTP

-- Richard kindly made this available to me. See http://mealybar.co.uk
tayBridgeTweetsURL = "http://sandbox.mealybar.co.uk/taybridgetweets/api.php?key=<REDACTED>"
tayBridgeLatLon = LatLon 56.453 (-2.948) WGS84

data TayBridge = TayBridge {
    pMessage :: T.Text,
    pTimestamp :: UTCTime
} deriving(Show)

instance FromJSON TayBridge where
    parseJSON (Object json) = TayBridge
        <$> liftM message (json .: "payload")
        <*> liftM parseTimestamp (json .: "modified")
      where
        closedTo = H.filter (== String "closed")
        noneClosed = H.null . closedTo
        allClosed m = (closedTo m) == m
        closedToNames = H.keys . closedTo
        message payload
            | noneClosed payload = "No restrictions on bridge"
            | allClosed payload  = "Closed to all vehicles"
            | otherwise  = "Closed to " `T.append` (T.intercalate ", " $ closedToNames payload)

        parseTimestamp = parseTimeOrError True defaultTimeLocale "%a, %d %b %Y %H:%M:%S %z"

getTayBridgeStatus :: IO (Maybe TayBridge)
getTayBridgeStatus = do
    response <- maybeOpenURL $ tayBridgeTweetsURL
    return $ response >>= decode
    
getBridgeStatus :: IO (Maybe BridgeStatus)
getBridgeStatus = do
    tay <- getTayBridgeStatus
    return $ fmap bridgeStatusFromTayBridge tay

bridgeStatusFromTayBridge TayBridge{..} =
    BridgeStatus "tayRoadBridge" "Tay Bridge" "A92" tayBridgeLatLon pMessage Nothing
