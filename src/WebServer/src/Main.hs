{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Monad                (liftM)
import           Control.Monad.IO.Class       (liftIO)
import qualified Data.ByteString.Char8 as BS
import           Data.String.Conversions      (convertString)
import           Snap.Core
import           Snap.Util.FileServe
import           Snap.Http.Server

import           Database.ScotTraffic
import qualified Database.Notifications

main :: IO ()
main = do
    initDB
    quickHttpServe site

initDB :: IO ()
initDB =
    withScotTraffic "localhost" $ Database.Notifications.prepareDatabase


site :: Snap ()
site = route [ ("notifications/:bridgeIdentifier/:deviceToken", notificationsHandler) ]

notificationsHandler :: Snap ()
notificationsHandler = do
    method <- liftM rqMethod getRequest
    deviceToken <- getParam "deviceToken"
    bridgeIdentifier <- getParam "bridgeIdentifier"
    result <- liftIO $ updateDatabase deviceToken bridgeIdentifier method
    writeBS result

updateDatabase :: Maybe BS.ByteString -> Maybe BS.ByteString -> Method -> IO BS.ByteString
updateDatabase deviceToken bridgeIdentifier method = 
    case (deviceToken, bridgeIdentifier) of
        (Just device, Just bridge) -> do
            operation <- databaseOperationForMethod method
            result <- withScotTraffic "localhost" $ operation device bridge
            return $ convertString . show $ result

        (_, _) ->
            return "missing parameters"

databaseOperationForMethod :: Monad m => Method -> m (BS.ByteString -> BS.ByteString -> ScotTraffic Bool)
databaseOperationForMethod method =
    case method of
        GET    -> return Database.Notifications.isDeviceRegistered
        PUT    -> return Database.Notifications.registerDevice
        DELETE -> return Database.Notifications.unregisterDevice
        _      -> fail "invalid method"
