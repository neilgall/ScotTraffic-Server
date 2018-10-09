{-# LANGUAGE OverloadedStrings #-}

module Static (getCameraList) where

import           Control.Applicative
import           Control.Monad                    (liftM)
import           Data.Aeson
import qualified Data.ByteString.Lazy     as BS
import           Data.Maybe
import qualified Data.Text                as T
import           Data.Time.Format
import           Debug.Trace
import           Text.Read

import           Geodetics.LatLon
import           Types.TrafficCamera

data StaticCamera = StaticCamera {
    enabled :: Bool,
    camera :: TrafficCamera
}

instance FromJSON StaticCamera where
    parseJSON = withObject "camera" $ \v -> do
        enabled <- v .: "enabled"
        camera <- StaticURLCamera
            <$> (v .: "filename")
            <*> (v .: "name")
            <*> (v .: "road")
            <*> liftM fromText (v .:? "direction" .!= "none")
            <*> (v .: "url")
            <*> (LatLon <$> (v .: "latitude") <*> (v .: "longitude") <*> pure WGS84)
            <*> defaultTimestamp
        return $ StaticCamera enabled camera
      where
        defaultTimestamp = parseTimeM True defaultTimeLocale "%s" "0"

getCameraList :: IO [TrafficCamera]
getCameraList = do
    json <- BS.readFile "etc/url-cameras.json"
    case eitherDecode json of
        Left error ->
            return []
        Right cameras ->
            return $ map camera . filter enabled $ cameras
