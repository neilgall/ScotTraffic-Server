{-# LANGUAGE OverloadedStrings, RecordWildCards #-} 

module MetOffice.DataPoint (getWeather) where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Trans
import           Control.Monad.Trans.Maybe
import           Data.Aeson
import           Data.Aeson.Types (Parser)
import qualified Data.ByteString.Lazy as B
import           Data.Maybe
import qualified Data.Text as T
import           Debug.Trace
import           Text.Read

import           Geodetics.LatLon
import           Types.Weather
import           Utils.HTTP

-- Get your API key at https://www.metoffice.gov.uk/datapoint/api
apiKey = "<REDACTED>" :: String
weatherURL = "http://datapoint.metoffice.gov.uk/public/data/val/wxobs/all/json/all?res=hourly&key=" ++ apiKey

type Text = T.Text

-- JSON structure for UK Observations Feed
-- http://www.metoffice.gov.uk/datapoint/product/uk-hourly-site-specific-observations/detailed-documentation

newtype Content = Content {
    getLocations :: [Location]
} deriving (Show)

data Location = Location {
    locIdentifier :: Maybe Int,
    locLatitude   :: Maybe Double,
    locLongitude  :: Maybe Double,
    locName       :: Maybe Text,
    locPeriods    :: [Period]
} deriving(Show)

newtype Period = Period {
    getRep :: [Rep] 
} deriving (Show)
 
data Rep = Rep {
    repWeatherType   :: Maybe Int,
    repTemperature   :: Maybe Double,
    repWindSpeed     :: Maybe Double,
    repWindDirection :: Maybe Text
} deriving (Show)

instance FromJSON Content where
    parseJSON (Object v) =
        Content <$> ((v .: "SiteRep") >>= (.: "DV") >>= (.: "Location"))

instance FromJSON Location where    
    parseJSON (Object v) =
        Location <$> readv "i"
                 <*> readv "lat"
                 <*> readv "lon"
                 <*> (v .: "name")
                 <*> (v .: "Period" >>= arrayOrSingleton)
        where
            readv f = liftM readMaybe (v .: f)
    
instance FromJSON Period where
    parseJSON (Object v) =
        Period <$> (v .:? "Rep" >>= arrayOrSingleton)

instance FromJSON Rep where
    parseJSON (Object v) =
        Rep <$> readv "W" <*> readv "T" <*> readv "S" <*> v .:? "D"
        where
            readv f = liftM (join . fmap readMaybe) (v .:? f)
    
    parseJSON v = return $ Rep Nothing Nothing Nothing Nothing

arrayOrSingleton :: FromJSON t => Maybe Value -> Parser [t]
arrayOrSingleton Nothing = return []
arrayOrSingleton (Just v) =
    case v of
        (Array _) -> parseJSON v
        _         -> parseJSON v >>= return . pure
        
        
getWeather :: IO [Weather]
getWeather = do
    locations <- getLocationsFeed
    let all = catMaybes $ map weatherFromLocation locations
    return $ filter isInScotland all

getLocationsFeed :: IO [Location]
getLocationsFeed = do
    response <- openURL weatherURL
    case response of
        Left _ ->
            return []
            
        Right content ->
            case eitherDecode content of
                Left error ->
                    trace error $ return []
                    
                Right content ->
                    return $ getLocations content
        
weatherFromLocation :: Location -> Maybe Weather
weatherFromLocation Location{..} =
    Weather <$> locIdentifier
            <*> locName
            <*> latlon
            <*> weatherType
            <*> (lastRep >>= repTemperature)
            <*> (lastRep >>= repWindSpeed)
            <*> (lastRep >>= repWindDirection)
    where
        latlon = LatLon <$> locLatitude <*> locLongitude <*> (Just WGS84)
        lastPeriod = last locPeriods
        weatherType = fmap weatherTypeName (lastRep >>= repWeatherType)
        lastRep = let rep = getRep lastPeriod in
            if null rep then Nothing else Just (last rep)

isInScotland :: Weather -> Bool
isInScotland w = (latitude . wLatLon $ w) > 54.5

weatherTypeName :: Int -> Text
weatherTypeName w = case w of
    0 -> "clear"
    1 -> "clear"
    2 -> "part-cloudy"
    3 -> "part-cloudy"
    5 -> "mist"
    6 -> "fog"
    7 -> "cloudy"
    8 -> "overcast"
    9 -> "light-rain-shower"
    10 -> "light-rain-shower"
    11 -> "drizzle"
    12 -> "light-rain"
    13 -> "heavy-rain-shower"
    14 -> "heavy-rain-shower"
    15 -> "heavy-rain"
    16 -> "sleet-shower"
    17 -> "sleet-shower"
    18 -> "sleet"
    19 -> "hail-shower"
    20 -> "hail-shower"
    21 -> "hail"
    22 -> "light-snow-shower"
    23 -> "light-snow-shower"
    24 -> "light-snow"
    25 -> "heavy-snow-shower"
    26 -> "heavy-snow-shower"
    27 -> "heavy-snow"
    28 -> "thunder-shower"
    29 -> "thunder-shower"
    30 -> "thunder"
    otherwise -> "unknown"
