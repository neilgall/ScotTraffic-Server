{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module Database.Incidents (queryAlerts, queryRoadworks, storeAlerts, storeRoadworks) where

import           Database.PostgreSQL.Simple.FromField
import           Database.PostgreSQL.Simple.FromRow
import           Database.PostgreSQL.Simple.ToField
import           Database.PostgreSQL.Simple.ToRow
import           Data.Time.Clock
import           Network.URI                             (parseURI, uriToString)
import           Data.Text as T

import           Database.ScotTraffic
import           Geodetics.LatLon
import           Types.Incident

instance FromRow Incident where
    fromRow = do
        title <- field
        road <- field
        description <- field
        latitude <- field
        longitude <- field
        date <- field
        link <- field
        let coord = LatLon latitude longitude WGS84
        case parseURI link of
            Just uri -> return $ Incident title road description coord date uri
            _ -> fail "cannot parse link"
                    
instance ToRow Incident where
    toRow Incident{..} = [ toField incidentTitle,
                           toField incidentRoad,
                           toField incidentDescription,
                           toField . latitude $ incidentCoordinate,
                           toField . longitude $ incidentCoordinate,
                           toField incidentDate,
                           toField . (uriToString id incidentLink) $ "" ]

prepareDatabase :: ScotTraffic ()
prepareDatabase = do
    execute "create table if not exists alerts (\
        \title            text not null, \
        \road             text, \
        \description      text not null, \
        \latitude         real not null, \
        \longitude        real not null, \
        \date             timestamp with time zone,\
        \uri              text)"
        ()
    execute "create table if not exists roadworks (\
        \title            text not null, \
        \road             text, \
        \description      text not null, \
        \latitude         real not null, \
        \longitude        real not null, \
        \date             timestamp with time zone,\
        \uri              text)"
        ()
    return ()

queryAlerts :: ScotTraffic [Alert]
queryAlerts = do
    prepareDatabase
    query "select title, road, description, latitude, longitude, date, uri from alerts" ()

queryRoadworks :: ScotTraffic [Alert]
queryRoadworks = do
    prepareDatabase
    query "select title, road, description, latitude, longitude, date, uri from roadworks" ()
    
storeAlerts :: [Alert] -> ScotTraffic ()
storeAlerts alerts = do
    prepareDatabase
    execute "delete from alerts" ()
    mapM storeAlert alerts
    return ()
    
storeRoadworks :: [Roadwork] -> ScotTraffic ()
storeRoadworks roadworks = do
    prepareDatabase
    execute "delete from roadworks" ()
    mapM storeRoadwork roadworks
    return ()
    
storeAlert :: Alert -> ScotTraffic ()
storeAlert alert = do
    execute "insert into alerts (title, road, description, latitude, longitude, date, uri) values (?,?,?,?,?,?,?)" alert
    return ()

storeRoadwork :: Roadwork -> ScotTraffic ()
storeRoadwork roadwork = do
    execute "insert into roadworks (title, road, description, latitude, longitude, date, uri) values (?,?,?,?,?,?,?)" roadwork
    return ()
