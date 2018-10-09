{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module Database.Bridges (store, queryBridgeStatus, queryAll) where

import           Control.Applicative
import           Control.Monad
import           Database.PostgreSQL.Simple.FromField
import           Database.PostgreSQL.Simple.FromRow
import           Database.PostgreSQL.Simple.ToField
import           Database.PostgreSQL.Simple.ToRow
import           Data.Maybe (listToMaybe)
import qualified Data.Text as T
import           Data.Time.Clock

import           Database.ScotTraffic
import           Geodetics.LatLon
import           Types.BridgeStatus

instance ToRow BridgeStatus where
    toRow BridgeStatus{..} = [ toField bsIdentifier,
                               toField bsDisplayName,
                               toField bsRoad,
                               toField $ latitude bsCoordinate,
                               toField $ longitude bsCoordinate,
                               toField bsMessage,
                               toField $ fmap wsMaximum bsWindSpeed,
                               toField $ fmap wsAverage bsWindSpeed,
                               toField $ fmap wsDirection bsWindSpeed,
                               toField $ fmap wsLastUpdated bsWindSpeed ]
                               
instance FromRow BridgeStatus where
    fromRow = do
        identifier <- field
        displayName <- field
        road <- field
        latitude <- field
        longitude <- field
        message <- field
        windSpeedMax <- field
        windSpeedAve <- field
        windDirection <- field
        lastUpdated <- field
        let windSpeed = WindSpeed <$> windSpeedMax <*> windSpeedAve <*> windDirection <*> lastUpdated
        return $ BridgeStatus identifier displayName road (LatLon latitude longitude WGS84) message windSpeed

store :: [BridgeStatus] -> ScotTraffic ()
store bridges = do
    prepareDatabase
    sequence $ map storeBridgeStatus bridges
    return ()
    
queryBridgeStatus :: T.Text -> ScotTraffic (Maybe BridgeStatus)
queryBridgeStatus identifier = do
    prepareDatabase
    bridges <- query "select identifier, name, road, latitude, longitude, message, \
        \windSpeedMax, windSpeedAverage, windDirection, lastUpdated \
        \ from bridges where identifier = ?" (Only identifier)
    return $ listToMaybe bridges
    
queryAll :: ScotTraffic [BridgeStatus]
queryAll = do
    prepareDatabase
    query "select identifier, name, road, latitude, longitude, message, \
          \windSpeedMax, windSpeedAverage, windDirection, lastUpdated from bridges" ()

prepareDatabase :: ScotTraffic ()
prepareDatabase = do
    execute "create table if not exists bridges (\
        \identifier       text unique not null, \
        \name             text not null, \
        \road             text not null, \
        \latitude         real not null, \
        \longitude        real not null, \
        \message          text, \
        \windSpeedMax     integer, \
        \windSpeedAverage integer, \
        \windDirection    text,    \
        \lastUpdated      text)"
        ()
    return ()

storeBridgeStatus :: BridgeStatus -> ScotTraffic ()
storeBridgeStatus bridge = do
    execute "delete from bridges where identifier = ?" (Only $ bsIdentifier bridge)
    execute "insert into bridges (identifier, name, road, latitude, longitude, message, \
        \windSpeedMax, windSpeedAverage, windDirection, lastUpdated) values (?,?,?,?,?,?,?,?,?,?)" bridge
    return ()
