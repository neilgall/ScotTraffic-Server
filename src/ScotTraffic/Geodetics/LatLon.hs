-- Geodesy tools for an ellipsoidal earth model         (c) Chris Veness 2005-2015 / MIT Licence
-- Haskell port by Neil Gall
--
-- Includes methods for converting lat/lon coordinates between different coordinate systems.
--   - www.movable-type.co.uk/scripts/latlong-convert-coords.html
--
--  Usage: to eg convert WGS 84 coordinate to OSGB coordinate:
--   let wgs84 = LatLon latWGS84 lonWGS84 WGS84
--   let osgb = convertDatum wgs84 OSGB36
--
--  q.v. Ordnance Survey 'A guide to coordinate systems in Great Britain' Section 6
--   - www.ordnancesurvey.co.uk/docs/support/guide-coordinate-systems-great-britain.pdf

module Geodetics.LatLon (
    Datum(..),
    Latitude, Longitude, LatLon(..),
    convertDatum, toDegrees, toRadians, distance
) where

type Latitude = Double
type Longitude = Double

data Datum = WGS84 | NAD83 | OSGB36 | ED50 | Irl1975 | TokyoJapan
    deriving(Eq, Read, Show)

data LatLon = LatLon {
    latitude :: Latitude,
    longitude :: Longitude,
    datum :: Datum
  } deriving(Eq, Read, Show)

data Ellipsoid = Ellipsoid Double Double Double
  deriving(Show)

data HelmertTransform = HelmertTransform {
    tx :: Double, ty :: Double, tz :: Double,  -- metres
    rx :: Double, ry :: Double, rz :: Double,  -- seconds
    s  :: Double                               -- ppm
  } deriving (Show)

data Vector3d = Vector3d {
    x :: Double,
    y :: Double,
    z :: Double
  } deriving(Show)

ellipsoid :: Datum -> Ellipsoid
ellipsoid WGS84      = Ellipsoid 6378137     6356752.31425 (1/298.257223563)
ellipsoid NAD83      = Ellipsoid 6378137     6356752.31414 (1/298.257222101)
ellipsoid OSGB36     = Ellipsoid 6377563.396 6356256.909   (1/299.3249646)
ellipsoid ED50       = Ellipsoid 6378388     6356911.946   (1/297.0)
ellipsoid Irl1975    = Ellipsoid 6377340.189 6356034.448   (1/299.3249646)
ellipsoid TokyoJapan = Ellipsoid 6377397.155 6356078.963   (1/299.152815351)

helmertTransform :: Datum -> HelmertTransform
helmertTransform WGS84 = HelmertTransform {
    tx = 0, ty = 0, tz = 0,
    rx = 0, ry = 0, rz = 0,
    s = 0
  }
helmertTransform NAD83 = HelmertTransform {
    tx = 1.004,  ty = -1.910,  tz = -0.515,
    rx = 0.0267, ry = 0.00034, rz = 0.011,
    s = -0.0015
  }
helmertTransform OSGB36 = HelmertTransform {
    tx = -446.448, ty = 125.157, tz = -542.060,
    rx = -0.1502,  ry = -0.2470, rz = -0.8421,
    s = 20.4894
  }
helmertTransform ED50 = HelmertTransform {
    tx = 89.5,  ty = 93.8, tz = 123.1,
    rx = 0.0,   ry = 0.0,  rz = 0.156,
    s = -1.2
 }
helmertTransform Irl1975 = HelmertTransform {
    tx = -482.530, ty = 130.596, tz = -564.557,
    rx = -1.042,   ry = -0.214,  rz = -0.631,
    s = -8.150
 }
helmertTransform TokyoJapan = HelmertTransform {
    tx = 148, ty = -507, tz = -685,
    rx = 0,   ry = 0,    rz = 0,
    s = 0
  }

invert :: HelmertTransform -> HelmertTransform
invert (HelmertTransform tx ty tz rx ry rz s) =
    HelmertTransform (-tx) (-ty) (-tz) (-rx) (-ry) (-rz) (-s)


-- Converts a LatLon coordinate to a new coordinate system
convertDatum :: LatLon -> Datum -> LatLon
convertDatum c@(LatLon lat lon oldDatum) newDatum =
  case (oldDatum, newDatum) of
    (WGS84, _) -> convert (helmertTransform newDatum)
    (_, WGS84) -> convert (invert $ helmertTransform oldDatum)
    _          -> convertDatum (convertDatum c WGS84) newDatum
  where
    convert transform = toLatLonE (applyTransform cartesian transform) newDatum
    cartesian = toCartesian c

-- Converts a LatLon to a cartesian coordinate
toCartesian :: LatLon -> Vector3d
toCartesian (LatLon lat lon datum) = Vector3d x y z
  where
    φ = toRadians lat
    λ = toRadians lon
    h = 0 -- height above ellipsoid - not currently used
    (Ellipsoid a b _) = ellipsoid datum
    eSq = (a*a - b*b) / (a*a)
    ν = a / sqrt (1 - eSq * sin φ ** 2)
    x = (ν + h) * cos φ * cos λ
    y = (ν + h) * cos φ * sin λ
    z = ((1 - eSq) * ν + h) * sin φ

-- Converts a cartesian coordinate to a LatLon with the specified datum
toLatLonE :: Vector3d -> Datum -> LatLon
toLatLonE (Vector3d x y z) datum = LatLon (toDegrees φ) (toDegrees λ) datum
  where
    (Ellipsoid a b f) = ellipsoid datum
    e2 = (a*a - b*b) / (a*a)       -- 1st eccentricity squared
    ε2 = (a*a - b*b) / (b*b)       -- 2nd eccentricity squared
    p = sqrt (x*x + y*y)           -- distance from minor axis
    r = sqrt (p*p + z*z)           -- polar radius

    -- parametric latitude (Bowring eqn 17, replacing tanβ = z·a / p·b)
    tanβ = (b*z) / (a*p) * (1 + ε2*b / r)
    sinβ = tanβ / sqrt(1 + tanβ**2)
    cosβ = sinβ / tanβ

    -- geodetic latitude (Bowring eqn 18)
    φ = atan2 (z + ε2*b*sinβ**3) (p - e2*a*cosβ**3)

    -- longitude
    λ = atan2 y x

    -- height above ellipsoid (Bowring eqn 7) [not currently used]
    ν = a * sqrt (1 - e2 * sin φ **2) -- length of the normal terminated by the minor axis
    h = p * cos φ + z * sin φ - (a*a / ν)


-- Applies the Helmert Transform to a cartesian coordinate
applyTransform :: Vector3d -> HelmertTransform -> Vector3d
applyTransform (Vector3d x y z) (HelmertTransform tx ty tz rx ry rz s) = Vector3d x' y' z'
  where
    rx' = toRadians (rx / 3600) -- normalise seconds to radians
    ry' = toRadians (ry / 3600)
    rz' = toRadians (rz / 3600)
    s' = s / 1e6 + 1            -- normalise ppm to (s+1)

    -- apply transform
    x' = tx + x*s'  + y*rz' + z*ry'
    y' = ty + x*rz' + y*s'  + z*rx'
    z' = tz + x*ry' + y*rx' + z*s'


toRadians :: Double -> Double
toRadians deg = deg / 180.0 * pi

toDegrees :: Double -> Double
toDegrees rad = rad * 180.0 / pi

-- Equirectangular approximation of distance bewteen two LatLon points.
distance :: LatLon -> LatLon -> Double
distance (LatLon x1 y1 WGS84) (LatLon x2 y2 WGS84) = sqrt (xd*xd + yd*yd) * 6371000
    where
        xd = toRadians (x2 - x1) * cos ((y1 + y2) / 2)
        yd = toRadians (y2 - y1)

distance from to = distance (convertDatum from WGS84) (convertDatum to WGS84)