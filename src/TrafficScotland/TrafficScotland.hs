{-# LANGUAGE RecordWildCards #-}

module TrafficScotland (getCameraList, downloadImages) where

import           Control.Monad                       (when, liftM)
import           Data.Maybe
import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Data.Digest.Pure.SHA       as SHA
import qualified Data.Text                  as T
import           Data.Time.Format
import           Debug.Trace
import           Network.FTP.Client
import           System.Directory
import           System.FilePath.Posix
import           System.IO.Error

import           Geodetics.LatLon
import           Geodetics.OSGridRef
import           Types.TrafficCamera
import           Utils.AtomicFile
import           Utils.CSV

type Text = T.Text
type SHA1 = SHA.Digest SHA.SHA1State


getCameraList :: IO [TrafficCamera]        
getCameraList = trace "FETCH cameraimages.csv" $ do
    h <- openConnection
    (lines, _) <- getlines h "cameraimages.csv"
    let csvRecords = parseCsv (map T.pack lines)
        tsCameras = map makeTrafficCamera csvRecords
    return $! catMaybes tsCameras

downloadImages :: [TrafficCamera] -> FilePath -> IO [TrafficCameraWithImageDigest]
downloadImages cameras directory = do
    h <- openConnection
    results <- sequence $ map (downloadImage h directory) cameras
    return $ catMaybes results

    
makeTrafficCamera :: [Text] -> Maybe TrafficCamera
makeTrafficCamera fields = case fields of
    a:b:c:d:[] -> 
        let
            (roadName, name, direction) = parseLocationName a
            toInt = read . T.unpack
            gridRef = GridRef (toInt c) (toInt d)
            latLon = toLatLon gridRef WGS84
        in
            TrafficScotlandCamera name roadName direction b latLon <$> defaultTimestamp
    _  -> Nothing
    where
        defaultTimestamp = parseTimeM True defaultTimeLocale "%s" "0"

parseLocationName :: Text -> (Text, Text, Direction)
parseLocationName fullName = (roadName, name, direction)
    where
        roadName = head $ T.words fullName
        nameWords = tail $ T.words fullName
        nameWordsWithoutNumericSuffix
            | isNumericSuffix && not isJunction = init nameWords
            | otherwise = nameWords
        isNumericSuffix = (T.unpack $ last nameWords) `elem` (map show [1..9])
        isDirection d = (T.pack d) `T.isSuffixOf` fullName
        isJunction = (T.pack "Junction") `elem` nameWords
        direction
            | isDirection "North" = North
            | isDirection "South" = South
            | isDirection "East" = East
            | isDirection "West" = West
            | otherwise = None
        name
            | direction == None = T.unwords nameWordsWithoutNumericSuffix
            | otherwise = T.unwords $ init nameWordsWithoutNumericSuffix


openConnection :: IO FTPConnection
openConnection = do
    h <- easyConnectFTP "ftp.traffic-scotland.co.uk"
    -- Redacted the original username and password; you'll need to ask
    -- info@trafficscotland.org for your own.
    login h "<USERNAME>" (Just "<PASSWORD>") Nothing
    return h    

getCameraImage :: FTPConnection -> FilePath -> IO (Maybe BS.ByteString)
getCameraImage h filename = trace ("FETCH " ++ filename) $ do
    result <- tryIOError (getbinary h path)
    case result of
        Left _ -> return Nothing
        Right (binary, _) -> return $ Just (BS.pack binary)

    where
        path = "current/" ++ filename
        
downloadImage :: FTPConnection -> FilePath -> TrafficCamera -> IO (Maybe TrafficCameraWithImageDigest)
downloadImage h path camera@TrafficScotlandCamera{..} = do
    imageData <- getCameraImage h filename
    case imageData of
        Just imageData -> do
            atomicFile filepath (flip BS.writeFile imageData)
            return $ return (camera, imageDigest imageData)
        Nothing ->
            return Nothing
    where
        filename = T.unpack tcFilename
        filepath = combine path filename
        imageDigest = T.pack . SHA.showDigest . SHA.sha1
    
