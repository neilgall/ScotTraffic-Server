{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module Bridges (generate) where

import           Data.Aeson
import qualified Data.ByteString.Lazy  as BS
import           Data.Time.Clock
import           Data.Time.Format
import qualified Data.Text             as T
import           System.FilePath.Posix

import           Geodetics.LatLon
import           Types.BridgeStatus
import           Utils.AtomicFile

instance ToJSON WindSpeed where
    toJSON WindSpeed{..} = object [
        "windSpeedMax"     .= wsMaximum,
        "windSpeedAverage" .= wsAverage,
        "windDirection"    .= wsDirection,
        "lastUpdated"      .= wsLastUpdated ]

instance ToJSON BridgeStatus where
    toJSON BridgeStatus{..} = object [
        "identifier"       .= bsIdentifier,
        "name"             .= bsDisplayName,
        "road"             .= bsRoad,
        "latitude"         .= latitude bsCoordinate,
        "longitude"        .= longitude bsCoordinate,
        "message"          .= bsMessage,
        "windSpeed"        .= bsWindSpeed ]

generate :: [BridgeStatus] -> FilePath -> IO ()
generate bridges webDirectory = atomicFile filePath (flip BS.writeFile json)
     where
         filePath = combine webDirectory "bridges.json"
         json = encode bridges
