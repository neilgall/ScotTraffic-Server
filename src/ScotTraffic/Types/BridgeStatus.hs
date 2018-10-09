module Types.BridgeStatus where

import Data.Text
import Geodetics.LatLon

data WindSpeed = WindSpeed {
    wsMaximum     :: Int,
    wsAverage     :: Int,
    wsDirection   :: Text,
    wsLastUpdated :: Text
} deriving (Eq, Show)

data BridgeStatus = BridgeStatus {
    bsIdentifier   :: Text,
    bsDisplayName  :: Text,
    bsRoad         :: Text,
    bsCoordinate   :: LatLon,
    bsMessage      :: Text,
    bsWindSpeed    :: Maybe WindSpeed
} deriving (Eq, Show)
