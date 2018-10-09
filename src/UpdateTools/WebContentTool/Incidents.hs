{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module Incidents (generate) where

import qualified Data.ByteString.Lazy  as BS
import           Data.Aeson

import           Geodetics.LatLon
import           Types.Incident
import           Utils.AtomicFile

instance ToJSON Incident where
    toJSON Incident{..} = object [
        "title"       .= incidentTitle,
        "road"        .= incidentRoad,
        "description" .= incidentDescription,
        "latitude"    .= latitude incidentCoordinate,
        "longitude"   .= longitude incidentCoordinate,
        "date"        .= incidentDate,
        "link"        .= show incidentLink ]
        
generate :: [Incident] -> FilePath -> IO ()
generate incidents filePath = atomicFile filePath (flip BS.writeFile json)
     where
         json = encode incidents
