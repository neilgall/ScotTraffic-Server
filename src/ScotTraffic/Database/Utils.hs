module Database.Utils where

import Database.PostgreSQL.Simple.FromRow
import Data.Text

instance FromRow Text where
    fromRow = field

instance FromRow Int where
    fromRow = field
    