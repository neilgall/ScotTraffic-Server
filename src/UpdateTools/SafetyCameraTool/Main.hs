
import           System.Console.GetOpt
import           System.Environment

import           Scraper
import           Database.SafetyCameras
import           Database.ScotTraffic

--------------------------------------------------

updateSafetyCameras :: String -> IO ()
updateSafetyCameras db = do
    cameras <- scrape
    withScotTraffic db $ Database.SafetyCameras.store cameras
    return ()

main = updateSafetyCameras "localhost"
