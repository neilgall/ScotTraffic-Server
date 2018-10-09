{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module Database.Notifications (
    prepareDatabase,
    isDeviceRegistered,
    registerDevice,
    unregisterDevice,
    deviceTokensForBridge
) where

import           Control.Monad
import           Data.Bool
import qualified Data.ByteString.Char8       as BS

import           Database.ScotTraffic

prepareDatabase :: ScotTraffic ()
prepareDatabase = do
    execute "create table if not exists notifications (\
            \deviceToken text not null, \
            \bridgeIdentifier text not null, \
            \lastUpdated timestamp with time zone default now(), \
            \primary key (deviceToken, bridgeIdentifier) \
            \)" ()
    return ()

isDeviceRegistered :: BS.ByteString -> BS.ByteString -> ScotTraffic Bool
isDeviceRegistered deviceToken bridgeIdentifier = do
    let pair = (BS.unpack deviceToken, BS.unpack bridgeIdentifier)
    [Only count] <- query "select count(*) from notifications \
                          \where deviceToken = ? and bridgeIdentifier = ?" pair
    return $ (count :: Int) > 0
    
    
registerDevice :: BS.ByteString -> BS.ByteString -> ScotTraffic Bool
registerDevice deviceToken bridgeIdentifier = do
    isRegistered <- isDeviceRegistered deviceToken bridgeIdentifier
    case isRegistered of
        True  -> return False
        False -> do
            let pair = (BS.unpack deviceToken, BS.unpack bridgeIdentifier)
            execute "insert into notifications (deviceToken, bridgeIdentifier) values (?,?)" pair
            return True


unregisterDevice :: BS.ByteString -> BS.ByteString -> ScotTraffic Bool
unregisterDevice deviceToken bridgeIdentifier = do
    let pair = (BS.unpack deviceToken, BS.unpack bridgeIdentifier)
    execute "delete from notifications where deviceToken = ? and bridgeIdentifier = ?" pair
    return True
    
    
deviceTokensForBridge :: BS.ByteString -> ScotTraffic [BS.ByteString]
deviceTokensForBridge bridgeIdentifier = do
    deviceTokens <- (query "select deviceToken from notifications where bridgeIdentifier = ?"
                        (Only . BS.unpack $ bridgeIdentifier)) :: ScotTraffic [Only String]

    return $ map (\(Only t) -> BS.pack t) $ deviceTokens
