{-# LANGUAGE OverloadedStrings #-}

module Types.Incident(
    Alert(..),
    Roadwork(..),
    Incident(..)
) where
    
import           Data.Text
import           Data.Time.Clock          (UTCTime)
import           Network.URI              (URI)

import           Geodetics.LatLon
    
data Incident = Incident {
    incidentTitle        :: Text,
    incidentRoad         :: Text,
    incidentDescription  :: Text,
    incidentCoordinate   :: LatLon,
    incidentDate         :: UTCTime,
    incidentLink         :: URI
}

type Alert = Incident
type Roadwork = Incident
