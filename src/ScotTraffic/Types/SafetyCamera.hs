{-# LANGUAGE DeriveDataTypeable, StandaloneDeriving #-}

module Types.SafetyCamera where

import Data.Data (Data, Typeable)
import Data.Text
import Geodetics.LatLon

data SpeedLimit = SpeedLimit20
                | SpeedLimit30
                | SpeedLimit40
                | SpeedLimit50
                | SpeedLimit60
                | SpeedLimit70
                | NationalSpeedLimit
                | SpeedLimitUnknown
                deriving (Eq, Read, Show, Data, Typeable)

data SafetyCameraImage = SafetyCameraImage {
    sciFileIndex :: Int,
    sciURL       :: Text
} deriving (Eq, Read, Show, Data, Typeable)

data SafetyCamera = SpeedCamera {
    scIdentifier      :: Text,
    scStartCoordinate :: LatLon,
    scEndCoordinate   :: LatLon,
    scName            :: Text,
    scRoad            :: Text,
    scSpeedLimit      :: SpeedLimit,
    scOriginalURL     :: Text,
    scImages          :: [SafetyCameraImage]
} deriving (Read, Show, Data, Typeable)

instance Eq SafetyCamera where
    c == d = (scIdentifier c) == (scIdentifier d)

safetyCameraImageFromURL = SafetyCameraImage (-1)

deriving instance Data Datum
deriving instance Typeable Datum
deriving instance Data LatLon
deriving instance Typeable LatLon

