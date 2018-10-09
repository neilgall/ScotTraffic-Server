
module Types.Weather (Weather(..), closestWeatherLocation) where

import Data.Foldable (minimumBy)
import Data.Ord
import Data.Text
import Geodetics.LatLon

data Weather = Weather {
    wIdentifier    :: Int,
    wName          :: Text,
    wLatLon        :: LatLon,
    wWeatherType   :: Text,
    wTemperature   :: Double,
    wWindSpeed     :: Double,
    wWindDirection :: Text
} deriving (Show)

closestWeatherLocation :: LatLon -> [Weather] -> Weather
closestWeatherLocation latLon weather = minimumBy distanceToWeatherLocation weather
    where
        distanceTo w = distance latLon (wLatLon w)
        distanceToWeatherLocation w1 w2 = compare (distanceTo w1) (distanceTo w2)
