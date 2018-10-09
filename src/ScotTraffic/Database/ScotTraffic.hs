{-# LANGUAGE OverloadedStrings #-}

module Database.ScotTraffic (
    withScotTraffic,
    query,
    execute,
    scotTrafficServer,
    ScotTrafficServer(..),
    ScotTraffic,
    Postgres.Only(..),
    Postgres.Query(..)
) where

import           Control.Monad.Reader
import           Data.Int                                  (Int64)
import qualified Database.PostgreSQL.Simple as Postgres
import           Database.PostgreSQL.Simple.FromRow
import           Database.PostgreSQL.Simple.ToRow

data ScotTrafficServer = ScotTrafficDevelopment | ScotTrafficProduction
type ScotTrafficContext = Postgres.Connection
type ScotTraffic a = ReaderT ScotTrafficContext IO a

instance FromRow ScotTrafficServer where
    fromRow = do
        serverType <- field :: RowParser String
        return $ if serverType == "production"
                 then ScotTrafficProduction
                 else ScotTrafficDevelopment

connect :: String -> IO Postgres.Connection
connect host =
    Postgres.connect Postgres.defaultConnectInfo {
        Postgres.connectDatabase = "scottraffic",
        Postgres.connectUser     = "scottraffic",
        Postgres.connectPassword = "v69EomYMatjBJd",
        Postgres.connectHost     = host
    }

query :: (ToRow q, FromRow r) => Postgres.Query -> q -> ScotTraffic [r]
query query args = do
    conn <- ask
    liftIO $ Postgres.query conn query args

execute :: (ToRow q) => Postgres.Query -> q -> ScotTraffic Int64
execute query args = do
    conn <- ask
    liftIO $ Postgres.execute conn query args

scotTrafficServer :: ScotTraffic ScotTrafficServer
scotTrafficServer = do
    conn <- ask
    server <- liftIO $ Postgres.query conn "select serverType from instance" ()
    return $ head server

withScotTraffic :: String -> ScotTraffic a -> IO a
withScotTraffic host f = do
    conn <- connect host
    runReaderT f conn
