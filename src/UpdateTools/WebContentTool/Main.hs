{-# LANGUAGE RecordWildCards #-}

import           Control.Monad.IO.Class       (liftIO)
import           Data.Time.Clock              (NominalDiffTime)
import           System.Console.GetOpt
import           System.Environment
import           System.FilePath.Posix

import           Database.Bridges
import           Database.Incidents
import           Database.SafetyCameras
import           Database.ScotTraffic
import           Database.TrafficCameras
import           Database.Weather
import           TrafficScotland
import           Types.TrafficCamera

import           Bridges
import           Incidents
import           SafetyCameras
import           TrafficCameras
import           Weather
import           WebImages

--------------------------------------------------
-- Main actions

generateWebJSON :: Options -> IO ()
generateWebJSON opts =
    withScotTraffic (dbHost opts) $ do
        trafficCameras <- Database.TrafficCameras.queryCamerasWithImages
        safetyCameras <- Database.SafetyCameras.queryWithMaxAge (maxSafetyCameraAge opts)
        weather <- Database.Weather.queryWithMaxAge (maxWeatherAge opts)
        bridges <- Database.Bridges.queryAll
        alerts <- Database.Incidents.queryAlerts
        roadworks <- Database.Incidents.queryRoadworks
        
        liftIO $ do
            Weather.generate weather (webDirectory opts)
            Bridges.generate bridges (webDirectory opts)
            Incidents.generate alerts $ combine (webDirectory opts) "alerts.json"
            Incidents.generate roadworks $ combine (webDirectory opts) "roadworks.json"
            TrafficCameras.generate trafficCameras weather (maxTrafficCameraAge opts) (webDirectory opts) trafficCamerasDir
            SafetyCameras.generate safetyCameras weather (webDirectory opts)
            return ()

downloadTrafficCameraImages :: String -> FilePath -> IO ()
downloadTrafficCameraImages db webDir =
    withScotTraffic db $ do
        cameras <- Database.TrafficCameras.queryAll
        camerasAndDigests <- liftIO $ downloadTrafficCameraImagesFromSources cameras webDir
        sequence $ map storeDigest camerasAndDigests
        return ()

downloadSafetyCameraImages :: String -> FilePath -> NominalDiffTime -> IO ()
downloadSafetyCameraImages db imageDir maxAge =
    withScotTraffic db $ do
        cameras <- Database.SafetyCameras.queryWithMaxAge maxAge
        liftIO $ SafetyCameras.downloadImages cameras imageDir
        return ()

pruneSafetyCameraImages :: String -> FilePath -> NominalDiffTime -> IO ()
pruneSafetyCameraImages db imageDir maxAge =
    withScotTraffic db $ do
        cameras <- Database.SafetyCameras.queryWithMaxAge maxAge
        liftIO $ SafetyCameras.pruneImages cameras imageDir
        return ()


--------------------------------------------------
-- Traffic Camera sources
        
trafficCamerasDir = "trafficcamera"

downloadTrafficCameraImagesFromSources :: [TrafficCamera] -> FilePath -> IO [TrafficCameraWithImageDigest]
downloadTrafficCameraImagesFromSources cameras webDir = do
    t <- TrafficScotland.downloadImages (filter isTrafficScotlandCamera cameras) imageDir
    s <- WebImages.downloadImages (filter isWebCamera cameras) imageDir
    return $ s ++ t
    where
        imageDir = combine webDir trafficCamerasDir
        isTrafficScotlandCamera TrafficScotlandCamera{..} = True
        isTrafficScotlandCamera _ = False
        isWebCamera StaticURLCamera{..} = True
        isWebCamera _ = False


--------------------------------------------------
-- Argument processing

data Options = Options {
        dbHost              :: String,
        webDirectory        :: FilePath,
        maxTrafficCameraAge :: NominalDiffTime,
        maxSafetyCameraAge  :: NominalDiffTime,
        maxWeatherAge       :: NominalDiffTime,
        tasks               :: [IO ()]
    }
    
readNominalDiffTime :: String -> NominalDiffTime
readNominalDiffTime s = let i = read s :: Int
                         in fromIntegral i :: NominalDiffTime
    
defaultOptions :: Options
defaultOptions = Options {
        dbHost              = "localhost",
        webDirectory        = "./web",
        maxTrafficCameraAge = 1800,
        maxSafetyCameraAge  = 86400*7,
        maxWeatherAge       = 7200,
        tasks               = []
    }
    
addTask opt task = opt { tasks = (tasks opt) ++ [task] }

options :: [OptDescr (Options -> IO Options)]
options =
    [ Option "h" ["host"]
        (ReqArg
         (\arg opt -> return opt { dbHost = arg })
         "hostname")
        "Set Traffic Cameras database host"

    , Option "w" ["web-dir"]
        (ReqArg (\arg opt -> return opt { webDirectory = arg })
        "web directory")
        "Set the web content directory"

    , Option "m" ["maxTrafficCameraAge"]
        (ReqArg (\arg opt -> return opt { maxTrafficCameraAge = readNominalDiffTime arg })
        "maximum image age (seconds)")
        "Set the maximum age for a Traffic Camera database entry for its image to be marked as stale"

    , Option "n" ["maxSafetyCameraAge"]
        (ReqArg (\arg opt -> return opt { maxSafetyCameraAge = readNominalDiffTime arg })
        "maximum camera age (seconds)")
        "Set the maximum age for a Safety Camera database entry for it to be output for web content"

    , Option "t" ["trafficCameraImages"]
        (NoArg (\opt -> return $ addTask opt $ downloadTrafficCameraImages (dbHost opt) (webDirectory opt)))
        "Download active traffic camera images to the web directory"
        
    , Option "s" ["safetyCameraImages"]
        (NoArg (\opt -> return $ addTask opt $ downloadSafetyCameraImages (dbHost opt) (webDirectory opt) (maxSafetyCameraAge opt)))
        "Download active safety camera images to the web directory"
        
    , Option "p" ["pruneSafetyCameraImages"]
        (NoArg (\opt -> return $ addTask opt $ pruneSafetyCameraImages (dbHost opt) (webDirectory opt) (maxSafetyCameraAge opt)))
        "Prune safety camera images from the web directory which are not referenced by the database"
                
    , Option "j" ["generateWebJSON"]
        (NoArg (\opt -> return $ addTask opt $ generateWebJSON opt))
        "Generate statically served web content from the current database"
    ]

main = do
        args <- getArgs
    
        -- get a list of options actions from the args
        let (actions, nonOptions, errors) = getOpt RequireOrder options args
    
        -- thread deafultOptions through the options actions
        opts <- foldl (>>=) (return defaultOptions) actions
        sequence $ tasks opts
 