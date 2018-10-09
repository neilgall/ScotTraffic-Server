--  Ordnance Survey Grid Reference functions            (c) Chris Veness 2005-2015 / MIT Licence
--  Haskell port by Neil Gall
-- 
--   - www.movable-type.co.uk/scripts/latlong-gridref.html 
--   - www.ordnancesurvey.co.uk/docs/support/guide-coordinate-systems-great-britain.pdf 
-- 
--  Converted 2015 to work with WGS84 by default, OSGB36 as option 
--   - www.ordnancesurvey.co.uk/blog/2014/12/confirmation-on-changes-to-latitude-and-longitude 
--
--  Formulation implemented here due to Thomas, Redfearn, etc is as published by OS, but is
--  inferior to Krüger as used by e.g. Karney 2011. 

module Geodetics.OSGridRef (GridRef(..), toGridRef, toLatLon) where

import Geodetics.LatLon

data GridRef = GridRef {
      easting :: Int,
      northing :: Int
    }
  deriving(Show)

airy1830Major = 6377563.396
airy1830Minor = 6356256.909
natGridScaleFactor = 0.9996012717

-- Converts latitude/longitude to Ordnance Survey grid reference easting/northing coordinate.
--
-- example
--   let p = LatLon 52.65798 1.71605 WGS84
--   let grid = toGridRef p  -- TG 51409 13177
--
toGridRef :: LatLon -> GridRef
toGridRef (LatLon lat lon OSGB36) = GridRef easting northing
  where
    φ = toRadians lat
    λ = toRadians lon
    a = airy1830Major
    b = airy1830Minor
    f0 = natGridScaleFactor
    φ0 = toRadians 49.0
    λ0 = toRadians (-2)                                -- NatGrid true origin is 49°N,2°W
    n0 = -100000.0
    e0 = 400000.0                                      -- northing & easting of true origin, metres
    e2 = 1 - (b*b)/(a*a)                               -- eccentricity squared
    n = (a-b) / (a+b)

    sin2φ = (sin φ) ** 2
    ν = a * f0 / sqrt (1 - e2 * sin2φ)                 -- nu = transverse radius of curvature
    ρ = a * f0 * (1 - e2) / (1 - e2 * sin2φ) ** 1.5    -- rho = meridional radius of curvature
    η2 = ν / ρ - 1

    ma = (1 + n + (5.0/4.0) * n**2 + (5.0/4.0) * n**3) * (φ-φ0)
    mb = (3 * n + 3 * n**2 + (21.0/8.0) * n**3) * sin(φ-φ0) * cos(φ+φ0)
    mc = ((15.0/8.0) * n**2 + (15.0/8.0) * n**3) * sin(2*(φ-φ0)) * cos(2*(φ+φ0))
    md = (35.0/24.0) * n**3 * sin(3*(φ-φ0)) * cos(3*(φ+φ0))
    m = b * f0 * (ma - mb + mc - md);                  -- meridional arc

    cos3φ = (cos φ) ** 3
    cos5φ = (cos φ) ** 5
    tan2φ = (tan φ) ** 2
    tan4φ = (tan φ) ** 4

    i = m + n0
    ii = (ν / 2) * sin φ * cos φ
    iii = (ν / 24) * sin φ * cos3φ * (5 - tan2φ + 9*η2)
    iiia = (ν / 720) * sin φ * cos5φ * (61 - 58*tan2φ + tan4φ)
    iv = ν * cos φ
    v = (ν / 6) * cos3φ * (ν / ρ - tan2φ)
    vi = (ν / 120) * cos5φ * (5 - 18*tan2φ + tan4φ + 14*η2 - 58*tan2φ * η2)

    δλ = λ-λ0
    northing = round (i + ii * δλ**2 + iii * δλ**4 + iiia * δλ**6)
    easting = round (e0 + iv * δλ + v * δλ**3 + vi * δλ**5)


toGridRef c@(LatLon lat lon datum) = toGridRef (convertDatum c OSGB36)

-- Converts Ordnance Survey grid reference easting/northing coordinate to latitude/longitude
--
-- example
--   let grid = GridRef 651409, 313177
--   let p = toLatLon grid WGS84   -- 52°39′29″N, 001°42′58″E
--   let q = toLatLon grid OSGB36  -- 52°39′27″N, 001°43′04″E
--
toLatLon :: GridRef -> Datum -> LatLon
toLatLon (GridRef easting northing) OSGB36 = LatLon (toDegrees φ') (toDegrees λ) OSGB36
  where
    a = airy1830Major
    b = airy1830Minor
    f0 = natGridScaleFactor
    φ0 = toRadians 49.0    -- ΝatGrid true origin latitude
    λ0 = toRadians (-2.0)  -- NatGrid true origin longitude
    n0 = -100000.0         -- NatGrid true origin northing (m)
    e0 = 400000.0          -- NatGrid true origin easting (m)
    e2 = 1 - (b*b)/(a*a)   -- eccentricity squared
    n = (a-b) / (a+b)
    δe = fromIntegral easting - e0
    δn = fromIntegral northing - n0
    (φ, _) = meridionalArc (φ0, 0.0)
    ν = a * f0 / sqrt (1 - e2 * sin2φ)               -- nu = transverse radius of curvature
    ρ = a * f0 * (1 - e2) / (1 - e2 * sin2φ) ** 1.5  -- rho = meridional radius of curvature
    η2 = ν / ρ - 1

    secφ = 1 / cos φ
    sin2φ = (sin φ) ** 2.0
    tan2φ = (tan φ) ** 2.0
    tan4φ = (tan φ) ** 4.0
    tan6φ = (tan φ) ** 6.0

    vii = tan φ / (2 * ρ * ν)
    viii = tan φ / (24 * ρ * ν**3) * (5 + (3 * tan2φ) + η2 - (9 * η2 * tan2φ))
    ix = tan φ / (720 * ρ * ν**5) * (61 + 90 * tan2φ + 45 * tan4φ)
    x = secφ / ν
    xi = secφ / (6 * ν**3) * (ν / ρ + 2 * tan2φ)
    xii = secφ / (120 * ν**5) * (5 + 28 * tan2φ + 24 * tan4φ)
    xiia = secφ / (5040 * ν**7) * (61 + 662 * tan2φ + 1320 * tan4φ + 720 * tan6φ)
    φ' = φ - vii * δe**2 + viii * δe**4 - ix * δe**6
    λ = λ0 + x * δe - xi * δe**3 + xii * δe**5 - xiia * δe**7

    meridionalArc :: (Double, Double) -> (Double, Double)
    meridionalArc (φ, m)
      | (δn - m') < 0.00001 = (φ', m')
      | otherwise = meridionalArc (φ', m')
        where
          φ' = (δn - m) / (a * f0) + φ
          φd = φ' - φ0
          φs = φ' + φ0
          ma = (1 + n + (5.0/4.0) * n**2 + (5.0/4.0) * n**3) * φd
          mb = (3 * n + 3 * n**2 + (21.0/8.0 * n**3)) * sin φd * cos φs
          mc = (15.0/8.0 * n**2 + 15.0/8.0 * n**3) * sin (2 * φd) * cos (2 * φs)
          md = (35.0/24.0 * n**3) * sin (3 * φd) * cos (3 * φs)
          m' = b * f0 * (ma - mb + mc - md)

toLatLon gridRef datum = convertDatum (toLatLon gridRef OSGB36) datum
