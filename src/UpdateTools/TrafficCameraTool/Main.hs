{-# LANGUAGE RecordWildCards #-}

import           Database.ScotTraffic
import           Database.TrafficCameras
import           TrafficScotland
import           Types.TrafficCamera

import           Highland
import           Static

--------------------------------------------------
-- Top level task actions

trafficCameraLists = [
    Highland.getCameraList,
    Static.getCameraList,
    TrafficScotland.getCameraList ]

updateTrafficCameras :: String -> IO ()
updateTrafficCameras db = do
    cameras <- sequence trafficCameraLists
    withScotTraffic db $ Database.TrafficCameras.store (foldl (++) [] cameras)
    return ()
    
    
main =
    updateTrafficCameras "localhost"
