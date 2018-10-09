module Utils.CSV (parseCsv) where

import qualified Data.Text as T

parseCsv :: [T.Text] -> [[T.Text]]
parseCsv lines = map (parseRecord . T.strip) lines
    where parseRecord = T.split (== ',')
