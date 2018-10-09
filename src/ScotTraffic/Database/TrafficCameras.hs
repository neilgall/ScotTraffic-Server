{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module Database.TrafficCameras (store, storeDigest, queryAll, queryCamerasWithImages) where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.IO.Class               (liftIO)
import           Database.PostgreSQL.Simple.FromField
import           Database.PostgreSQL.Simple.FromRow
import           Database.PostgreSQL.Simple.ToField
import           Database.PostgreSQL.Simple.ToRow
import           Data.Maybe
import qualified Data.Text as T
import           Data.Time.Clock

import           Database.ScotTraffic
import           Database.Utils
import           Geodetics.LatLon
import           Types.TrafficCamera

wgs84LatLon :: Latitude -> Longitude -> LatLon
wgs84LatLon lat lon = LatLon lat lon WGS84

trafficScotlandTag = "TrafficScotland" :: String
staticURLTag = "StaticURL" :: String

instance ToRow TrafficCamera where
    toRow TrafficScotlandCamera{..} = [ toField trafficScotlandTag
                                      , toField tcName
                                      , toField tcFilename
                                      , toField $ latitude tcLatLon
                                      , toField $ longitude tcLatLon
                                      , toField tcRoadName
                                      , toField $ show tcDirection ]
                                      
    toRow StaticURLCamera{..} = [ toField staticURLTag
                                , toField tcName
                                , toField tcImage
                                , toField tcFilename
                                , toField $ latitude tcLatLon
                                , toField $ longitude tcLatLon
                                , toField tcRoadName
                                , toField $ show tcDirection ]

instance FromRow TrafficCamera where
    fromRow = do
        source <- field :: RowParser String
        name <- field
        image <- field :: RowParser (Maybe T.Text)
        latlon <- liftM2 wgs84LatLon field field
        filename <- field
        road <- field
        direction <- liftM read field
        timestamp <- field
        return $ if source == trafficScotlandTag then
            TrafficScotlandCamera name road direction filename latlon timestamp
        else
            StaticURLCamera filename name road direction (fromJust image) latlon timestamp
        
store :: [TrafficCamera] -> ScotTraffic ()
store cameras = do
    prepareDatabase
    sequence $ map storeCamera cameras
    return ()

queryAll :: ScotTraffic [TrafficCamera]
queryAll =
    query "select source, name, image, latitude, longitude, filename, road, direction, updateTimestamp from trafficcameras" ()


queryCamerasWithImages :: ScotTraffic [TrafficCamera]
queryCamerasWithImages =
    query "select source, name, image, latitude, longitude, \
            \filename, road, direction, updateTimestamp \
            \from trafficcameras where digest is not null" ()


prepareDatabase :: ScotTraffic ()
prepareDatabase = do
    execute "create table if not exists trafficcameras (\
                \source text not null, \
                \name text not null, \
                \image text, \
                \latitude real not null, \
                \longitude real not null, \
                \filename text not null, \
                \road text, \
                \direction text, \
                \digest text, \
                \updateTimestamp timestamp with time zone default now() \
                \)" ()
    return ()

storeCamera :: TrafficCamera -> ScotTraffic ()
storeCamera camera = do
    found <- (query "select count(*) from trafficcameras where filename = ?" $ Only (tcFilename camera)) :: ScotTraffic [Int]
    when (head found == 0) $ do
        execute (insertSQL camera) camera
        return ()
    
insertSQL :: TrafficCamera -> Query
insertSQL camera@TrafficScotlandCamera{..} =
    "insert into trafficcameras (source, name, filename, latitude, longitude, road, direction) values (?,?,?,?,?,?,?)"
insertSQL camera@StaticURLCamera{..} = 
    "insert into trafficcameras (source, name, image, filename, latitude, longitude, road, direction) values (?,?,?,?,?,?,?,?)"


storeDigest :: (TrafficCamera, T.Text) -> ScotTraffic ()
storeDigest (camera, newDigest) = do
    maybeOldDigest <- (query "select digest from trafficcameras where filename = ?" $ Only key) :: ScotTraffic [Maybe (Only String)]
    case maybeOldDigest of        
        [Just (Only oldDigest)] -> if (T.pack oldDigest) /= newDigest then storeDigestAndUpdateTimestamp
            else return ()

        _ -> storeDigestAndUpdateTimestamp
        
    where
        key = (tcFilename camera)
        storeDigestAndUpdateTimestamp = do
            now <- liftIO getCurrentTime
            execute "update trafficcameras set digest=?, updateTimestamp=? where filename = ?" (newDigest, now, key)
            return ()
