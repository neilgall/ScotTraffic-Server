
import           Control.Monad.IO.Class (liftIO)
import           Database.ScotTraffic
import           Database.Weather

import           MetOffice.DataPoint

updateWeather :: String -> IO ()
updateWeather db =
    withScotTraffic db $ do
        weather <- liftIO getWeather
        Database.Weather.store weather
        return ()

main =
    updateWeather "localhost"
