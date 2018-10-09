{-# LANGUAGE OverloadedStrings #-}

module Types.TrafficCamera where

import Data.Maybe
import Data.Text
import Data.Time.Clock
import Geodetics.LatLon

data Direction = North | South | East | West | None deriving (Eq, Show, Read)

data TrafficCamera =
    TrafficScotlandCamera {
        tcName      :: Text,
        tcRoadName  :: Text,
        tcDirection :: Direction,
        tcFilename  :: Text,
        tcLatLon    :: LatLon,
        tcTimestamp :: UTCTime
    }
    | StaticURLCamera {
        tcFilename  :: Text,
        tcName      :: Text,
        tcRoadName  :: Text,
        tcDirection :: Direction,
        tcImage     :: Text,
        tcLatLon    :: LatLon,
        tcTimestamp :: UTCTime
    }

fromText :: Text -> Direction
fromText "North" = North
fromText "South" = South
fromText "East" = East
fromText "West" = West
fromText _ = None


type TrafficCameraWithImageDigest = (TrafficCamera, Text)

