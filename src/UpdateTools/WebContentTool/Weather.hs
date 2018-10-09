{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module Weather (generate) where

import qualified Data.ByteString.Lazy  as BS
import           Data.Aeson
import           System.FilePath.Posix

import           Geodetics.LatLon
import           Types.Weather
import           Utils.AtomicFile

instance ToJSON Weather where
    toJSON Weather{..} = object [
        "identifier" .= wIdentifier,
        "name"       .= wName,
        "latitude"   .= latitude wLatLon,
        "longitude"  .= longitude wLatLon,
        "type"       .= wWeatherType,
        "temp"       .= wTemperature,
        "windSpeed"  .= wWindSpeed,
        "windDir"    .= wWindDirection ]
        
generate :: [Weather] -> FilePath -> IO ()
generate weather webDirectory = atomicFile filePath (flip BS.writeFile json)
     where
         filePath = combine webDirectory "weather.json"
         json = encode weather
