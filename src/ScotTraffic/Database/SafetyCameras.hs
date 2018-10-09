{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module Database.SafetyCameras (store, queryWithMaxAge) where

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class              (liftIO)
import Data.Maybe                          (fromMaybe)
import Data.Text                           (Text)
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow
import Data.Time.Clock

import Database.ScotTraffic
import Database.Utils
import Geodetics.LatLon
import Types.SafetyCamera

wgs84LatLon :: Latitude -> Longitude -> LatLon
wgs84LatLon lat lon = LatLon lat lon WGS84

instance FromRow SafetyCameraImage where
    fromRow = SafetyCameraImage
        <$> field -- file index
        <*> field -- URL

instance ToRow SafetyCameraImage where
    toRow SafetyCameraImage{..} =
        [ toField sciFileIndex,
          toField sciURL ]

instance FromRow SafetyCamera where
    fromRow = SpeedCamera
        <$> field -- identifier
        <*> liftM2 wgs84LatLon field field -- startCoordinate
        <*> liftM2 wgs84LatLon field field -- endCoordinate
        <*> field -- description
        <*> field -- road
        <*> liftM read field -- speedLimit
        <*> liftM (fromMaybe "") field -- originalURL
        <*> pure []

instance ToRow SafetyCamera where
    toRow SpeedCamera{..} =
        [ toField scIdentifier
        , toField $ latitude scStartCoordinate
        , toField $ longitude scStartCoordinate
        , toField $ latitude scEndCoordinate
        , toField $ longitude scEndCoordinate
        , toField scName
        , toField scRoad
        , toField (show scSpeedLimit)
        , toField scOriginalURL ]

store :: [SafetyCamera] -> ScotTraffic ()
store cameras = do
    prepareDatabase
    sequence $ map storeCamera cameras
    return ()

queryWithMaxAge :: NominalDiffTime -> ScotTraffic [SafetyCamera]
queryWithMaxAge maxAge = do
    now <- liftIO getCurrentTime
    let oldestTimestamp = addUTCTime (-maxAge) now
    cameras <- query "select identifier, startLatitude, startLongitude, \
                        \endLatitude, endLongitude, \
                        \description, road, speedLimit, originalURL \
                        \from safetycameras where updateTimestamp > ?" (Only oldestTimestamp)
    mapM cameraWithImages cameras
    
queryImagesForCamera :: SafetyCamera -> ScotTraffic [SafetyCameraImage]
queryImagesForCamera camera =
    query "select fileIndex, imageURL from safetycameraimages where identifier = ?" (Only $ scIdentifier camera)

cameraWithImages :: SafetyCamera -> ScotTraffic SafetyCamera
cameraWithImages camera = do
    images <- queryImagesForCamera camera
    return camera { scImages = images }

prepareDatabase :: ScotTraffic ()
prepareDatabase = do
    execute "create table if not exists safetycameras (\
        \identifier     text primary key, \
        \startLatitude  real not null,   \
        \startLongitude real not null,   \
        \endLatitude    real,            \
        \endLongitude   real,            \
        \description    text not null,   \
        \road           text not null,   \
        \speedLimit     text,            \
        \originalURL    text,            \
        \updateTimestamp timestamp with time zone default now()\
    \)" ()
    execute "create table if not exists safetycameraimages (\
        \identifier     text references safetycameras on delete cascade, \
        \imageURL       text not null unique, \
        \fileIndex      serial \
    \)" ()
    return ()

storeCamera :: SafetyCamera -> ScotTraffic ()
storeCamera camera = do
    let identifier = scIdentifier camera
    execute "delete from safetycameras where identifier = ?" $ Only identifier
    execute "insert into safetycameras (\
        \identifier, startLatitude, startLongitude, endLatitude, endLongitude, \
        \description, road, speedLimit, originalURL) values (?,?,?,?,?,?,?,?,?)" $ camera
    mapM (storeCameraImage identifier) (scImages camera)
    return ()

storeCameraImage :: Text -> SafetyCameraImage -> ScotTraffic ()
storeCameraImage identifier SafetyCameraImage{..} = do
    execute "insert into safetycameraimages (identifier, imageURL) values (?,?)" (identifier, sciURL)
    return ()
        