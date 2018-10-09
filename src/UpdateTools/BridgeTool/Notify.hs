{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module Notify (notifyOnChange) where

import           Control.Monad                (when)
import           Control.Monad.IO.Class       (liftIO)
import           Data.ByteString              (hPutStrLn)
import           Data.String.Conversions      (convertString)
import           Data.Text                    (Text, append)
import           GHC.IO.Handle                (hClose)
import           System.FilePath.Posix        (combine)
import           System.Posix.Env             (getEnv)
import           System.Process

import qualified Database.Bridges
import qualified Database.Notifications
import           Database.ScotTraffic
import           Types.BridgeStatus

body :: BridgeStatus -> Text
body BridgeStatus{..} =
    bsDisplayName `append` ": " `append` bsMessage

customParams :: BridgeStatus -> Text
customParams BridgeStatus{..} =
    "{\"bridgeIdentifier\": \"" `append` bsIdentifier `append` "\"}"

notify :: BridgeStatus -> ScotTraffic ()
notify bridgeStatus = do
    tokens <- Database.Notifications.deviceTokensForBridge (convertString . bsIdentifier $ bridgeStatus)
    server <- scotTrafficServer

    liftIO $ do 
        (Just home) <- getEnv "HOME"
        let cmdSpec = [ combine home "bin/apns-send.py"
                      , sandboxOption server
                      , "--message", (convertString . body) bridgeStatus
                      , "--custom", (convertString . customParams) bridgeStatus ]

        (Just pipe, _, _, _) <-
            createProcess (proc "/usr/bin/python" cmdSpec) { std_in = CreatePipe }

        sequence $ map (hPutStrLn pipe . convertString) tokens

        hClose pipe
        
sandboxOption :: ScotTrafficServer -> String
sandboxOption ScotTrafficDevelopment = "--sandbox"
sandboxOption ScotTrafficProduction  = "--production"

notifyOnChange :: BridgeStatus -> ScotTraffic ()
notifyOnChange bridgeStatus = do
    maybePreviousStatus <- Database.Bridges.queryBridgeStatus (bsIdentifier bridgeStatus)
    when (shouldNotify maybePreviousStatus) $ notify bridgeStatus
  where
    shouldNotify maybePreviousStatus =
        case maybePreviousStatus of
            Just previousStatus -> (bsMessage previousStatus) /= (bsMessage bridgeStatus)
            Nothing -> True

