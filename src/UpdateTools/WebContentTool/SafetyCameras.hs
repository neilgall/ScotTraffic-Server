{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module SafetyCameras (generate, downloadImages, pruneImages) where

import           Control.Applicative
import qualified Data.ByteString.Lazy  as BS
import           Data.Aeson
import           Data.Maybe
import           Data.Time.Clock
import           Data.Time.Format
import qualified Data.Text             as T
import           System.Directory
import           System.FilePath.Posix
import           Text.Read (readMaybe)

import           Geodetics.LatLon
import           Types.SafetyCamera
import           Types.Weather
import           Utils.AtomicFile
import           Utils.HTTP (downloadURL)

data OutputCamera = OutputCamera {
    latLon          :: LatLon,
    name            :: T.Text,
    road            :: T.Text,
    url             :: T.Text,
    speedLimit      :: SpeedLimit,
    weatherLocation :: Int,
    imageURLs       :: [T.Text]
}

instance ToJSON OutputCamera where
    toJSON OutputCamera{..} = object [
        "latitude"   .= latitude latLon,
        "longitude"  .= longitude latLon,
        "name"       .= name,
        "road"       .= road,
        "url"        .= url,
        "speedLimit" .= showSpeedLimit speedLimit,
        "weather"    .= weatherLocation,
        "images"     .= imageURLs ]
        
showSpeedLimit :: SpeedLimit -> String
showSpeedLimit SpeedLimit20 = "20"
showSpeedLimit SpeedLimit30 = "30"
showSpeedLimit SpeedLimit40 = "40"
showSpeedLimit SpeedLimit50 = "50"
showSpeedLimit SpeedLimit60 = "60"
showSpeedLimit SpeedLimit70 = "70"
showSpeedLimit NationalSpeedLimit = "nsl"
showSpeedLimit SpeedLimitUnknown = ""

data OutputContext = OutputContext {
    ocImageDir :: FilePath,
    ocWeather :: [Weather]
}

imageSubdir root = combine root "safetycamera"

-- generate the safetycameras.json file in the web directory
--
generate :: [SafetyCamera] -> [Weather] -> FilePath -> IO ()
generate cameras weather webDirectory = do
    atomicFile filePath (flip BS.writeFile json)
    return ()
    where
        outputCameras = map (outputCamera $ OutputContext imageDir weather) cameras
        json = encode outputCameras
        filePath = combine webDirectory "safetycameras.json"
        imageDir = imageSubdir ""


-- Download the safety camera images from the various SSCP websites to the web directory
--
downloadImages :: [SafetyCamera] -> FilePath -> IO [()]
downloadImages cameras webDirectory = do
    let imageDirectory = imageSubdir webDirectory
    createDirectoryIfMissing True imageDirectory
    sequence $ map (downloadImage imageDirectory) $ imagesForSafetyCameras cameras

-- Prune images from the download directory which are not referenced by the database
--
pruneImages :: [SafetyCamera] -> FilePath -> IO [()]
pruneImages cameras webDirectory = do
    filenames <- getDirectoryContents $ imageSubdir webDirectory
    let indicesFromFilenames = catMaybes $ map indexForFilename filenames
    let indicesFromDatabase = map sciFileIndex $ imagesForSafetyCameras cameras
    let unreferencedIndices = filter (not . flip elem indicesFromDatabase) indicesFromFilenames
    sequence $ map (removeFile . combine (imageSubdir webDirectory) . filenameForIndex) unreferencedIndices


-- Download a single SafetyCameraImage
--
downloadImage :: FilePath -> SafetyCameraImage -> IO ()
downloadImage webDirectory SafetyCameraImage{..} = do
    downloadURL url path
    return ()
    where
        url = T.unpack sciURL
        path = combine webDirectory $ filenameForIndex sciFileIndex

imagesForSafetyCameras :: [SafetyCamera] -> [SafetyCameraImage]
imagesForSafetyCameras cameras = concat $ map scImages cameras

outputCamera :: OutputContext -> SafetyCamera -> OutputCamera
outputCamera OutputContext{..} SpeedCamera{..} =
    OutputCamera scStartCoordinate scName scRoad scOriginalURL speedLimit (wIdentifier closestWeather) imageFilenames
    where
        closestWeather = closestWeatherLocation scStartCoordinate ocWeather
        imageFilenames = map (T.pack . combine ocImageDir . filenameForIndex . sciFileIndex) scImages
        speedLimit = if scSpeedLimit == SpeedLimitUnknown then inferredSpeedLimit else scSpeedLimit
        inferredSpeedLimit = if (T.take 1 scRoad) == "M" then SpeedLimit70 else SpeedLimitUnknown

filenameForIndex :: Int -> String
filenameForIndex index = (show index) ++ ".jpg"

indexForFilename :: String -> Maybe Int
indexForFilename filename = readMaybe basename
    where
        prefix = T.stripSuffix ".jpg" $ T.pack filename
        basename = case prefix of
            Nothing -> ""
            (Just b) -> T.unpack b

