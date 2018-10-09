
import           Control.Monad.IO.Class       (liftIO)
import           Data.Maybe                   (catMaybes)

import qualified Database.Bridges
import           Database.ScotTraffic
import           Types.BridgeStatus

import qualified Bridges.Forth
import qualified Bridges.Tay
import           Notify

updateBridges :: ScotTraffic ()
updateBridges = do
    maybeBridges <- liftIO $ sequence [ Bridges.Forth.getBridgeStatus, Bridges.Tay.getBridgeStatus ]
    let bridges = catMaybes maybeBridges
    sequence $ map notifyOnChange bridges
    Database.Bridges.store bridges
    return ()

main = withScotTraffic "localhost" updateBridges
