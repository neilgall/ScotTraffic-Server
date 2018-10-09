{-# LANGUAGE OverloadedStrings #-}

module Database.Weather (store, queryWithMaxAge) where

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class               (liftIO)
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow
import Data.Time.Clock

import Database.ScotTraffic
import Geodetics.LatLon
import Types.Weather

wgs84LatLon :: Latitude -> Longitude -> LatLon
wgs84LatLon lat lon = LatLon lat lon WGS84

instance ToRow Weather where
    toRow w = [ toField (wIdentifier w)
              , toField (wName w)
              , toField (latitude . wLatLon $ w)
              , toField (longitude . wLatLon $ w)
              , toField (wWeatherType w)
              , toField (wTemperature w)
              , toField (wWindSpeed w)
              , toField (wWindDirection w) ]

instance FromRow Weather where
    fromRow = Weather
        <$> field -- identifier
        <*> field -- name
        <*> liftM2 wgs84LatLon field field
        <*> field -- weather type
        <*> field -- temperature
        <*> field -- wind speed
        <*> field -- wind direction

store :: [Weather] -> ScotTraffic ()
store weather = do
    prepareDatabase
    sequence $ map storeWeather weather
    return ()

queryWithMaxAge :: NominalDiffTime -> ScotTraffic [Weather]
queryWithMaxAge maxAge = do
    now <- liftIO getCurrentTime
    let oldestTimestamp = addUTCTime (-maxAge) now
    weather <- query "select identifier, name, latitude, longitude, \
                    \weatherType, temperature, windSpeed, windDirection \
                    \from weather where updateTimestamp > ?" (Only oldestTimestamp)
    return weather

prepareDatabase :: ScotTraffic ()
prepareDatabase = do
    execute "create table if not exists weather (\
        \identifier integer not null, \
        \name varchar(100) not null, \
        \latitude real not null, \
        \longitude real not null, \
        \weatherType varchar(20) not null, \
        \temperature real not null, \
        \windSpeed real, \
        \windDirection varchar(8), \
        \updateTimestamp timestamp with time zone default now()\
        \)" ()
    return ()

storeWeather :: Weather -> ScotTraffic ()
storeWeather weather = do
    execute "delete from weather where identifier = ?" $ Only (wIdentifier weather)
    execute "insert into weather (\
        \identifier, name, latitude, longitude, weatherType, \
        \temperature, windSpeed, windDirection) values (?,?,?,?,?,?,?,?)" $ weather
    return ()

