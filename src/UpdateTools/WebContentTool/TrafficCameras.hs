{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module TrafficCameras (generate) where

import           Control.Monad.Reader
import qualified Data.ByteString.Lazy  as BS
import           Data.Aeson
import qualified Data.Map.Strict       as Map
import           Data.Time.Clock
import           Data.Time.Format
import qualified Data.Text             as T
import           System.Directory
import           System.FilePath.Posix

import           Geodetics.LatLon
import           Types.TrafficCamera
import           Types.Weather
import           Utils.AtomicFile

type Text = T.Text

-- Reader context for building TrafficCameras data

data Context = Context {
    weather           :: [Weather],
    minImageTimestamp :: UTCTime,
    imageDirectory    :: FilePath
}

-- Traffic Cameras JSON data structure

data Location = Location {
    locName     :: Text,
    locRoadName :: Text,
    locLatLon   :: LatLon,
    locWeather  :: Int,
    locCameras  :: [Camera]
}

data Camera = Camera {
    camFilename  :: FilePath,
    camDirection :: Direction,
    camAvailable :: Bool
}

instance ToJSON Location where
    toJSON Location{..} = object [
        "name"      .= locName,
        "road"      .= locRoadName,
        "latitude"  .= latitude locLatLon,
        "longitude" .= longitude locLatLon,
        "weather"   .= locWeather,
        "cameras"   .= locCameras ]
    
instance ToJSON Camera where
    toJSON Camera{..} =
        case camDirection of
            None -> object fields
            _    -> object (directionField:fields)
        where
            fields = [
                "image"     .= camFilename,
                "available" .= camAvailable ]
            directionField = "direction" .= directionChar camDirection
    
directionChar :: Direction -> Text
directionChar North = "N"
directionChar South = "S"
directionChar East = "E"
directionChar West = "W"
    
generate :: [TrafficCamera] -> [Weather] -> NominalDiffTime -> FilePath -> FilePath -> IO ()
generate cameras weather maxAge webDirectory imageDirectory = do
    createDirectoryIfMissing True webDirectory
    createDirectoryIfMissing True imageDirectory
    now <- getCurrentTime
    let filePath = combine webDirectory "trafficcameras.json"
        minimumTimestamp = addUTCTime (-maxAge) now
        locations = runReader (locationsFromCameras cameras) (Context weather minimumTimestamp imageDirectory)
        json = encode locations
    atomicFile filePath (flip BS.writeFile json)
 
locationsFromCameras :: [TrafficCamera] -> Reader Context [Location]
locationsFromCameras cameras = fmap Map.elems $ foldM insertCamera Map.empty cameras

-- assume unique names, so just using the name as the map key
type LocationMap = Map.Map Text Location

insertCamera :: LocationMap -> TrafficCamera -> Reader Context LocationMap
insertCamera locations camera = do
    context <- ask
    let latlon = tcLatLon camera
        name = tcName camera
        closestWeather = closestWeatherLocation latlon (weather context)
        available = isAvailable (tcTimestamp camera) $ minImageTimestamp context
        localFilepath = combine (imageDirectory context) (localFilename camera)
        outputCamera = Camera localFilepath (tcDirection camera) available
        location = Location name (tcRoadName camera) latlon (wIdentifier closestWeather) [outputCamera]
    return $ Map.insertWith mergeLocations name location locations
        
localFilename :: TrafficCamera -> FilePath
localFilename TrafficScotlandCamera{..} = T.unpack tcFilename
localFilename StaticURLCamera{..} = T.unpack tcFilename
        
isAvailable :: UTCTime -> UTCTime -> Bool
isAvailable timestamp minTimestamp = (diffUTCTime timestamp minTimestamp) > 0
    
mergeLocations :: Location -> Location -> Location
mergeLocations old new = new { locCameras = (locCameras old) ++ (locCameras new) }
