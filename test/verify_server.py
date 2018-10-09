#!/usr/local/bin/python3
# ScotTraffic server content validator

import sys
import json
import http.client

fails = 0

def fail(msg):
    global fails
    print("FAIL " + msg)
    fails += 1

def getJSON(path, conn):
    conn.request("GET", "https://%s/%s" % (conn.host, path))
    content = conn.getresponse().read().decode("utf8")
    return json.loads(content)
    
def verifyImageAtURL(path, conn):
    try:
        conn.request("HEAD", "https://%s/%s" % (conn.host, path))
        response = conn.getresponse()
        response.read()
        assert response.status == 200, "invalid HTTP status %d" % response.status
        headers = dict(response.getheaders())
        assert headers["Content-Type"] == "image/jpeg", "invalid content type"
        assert int(headers["Content-Length"]) > 0, "zero content length"
    except Exception as e:
        assert False, "cannot open %s: %s" % (path, e)

def verifyTrafficCamera(cam, conn):
    assert cam.get("image") is not None and cam["image"] != "", "no image"
    assert cam.get("direction") is None or cam["direction"] in ["N","S","E","W"], "bad direction"
    assert cam.get("available") == True or cam["available"] == False, "no available flag"
    verifyImageAtURL(cam["image"], conn)

def verifyCoordinates(dict, latKey, lonKey):
    latitude = float(dict.get(latKey, 0))
    longitude = float(dict.get(lonKey, 0))
    assert 54.5 < latitude < 60.8, ("latitude %f" % latitude)
    assert -7.6 < longitude < -0.8, ("longitude %f" % longitude) 

def verifyTrafficCameraLocation(loc, conn):
    verifyCoordinates(loc, "latitude", "longitude")
    assert loc.get("name") is not None and loc["name"] != "", "no location name"
    assert loc.get("road") is not None, "no road name"
    assert loc.get("cameras") is not None, "no cameras"
    for cam in loc["cameras"]:
        verifyTrafficCamera(cam, conn)
        
def verifyTrafficCameraAvailability(locations):
    total = 0
    available = 0
    for loc in locations:
        cameras = loc["cameras"]
        total += len(cameras)
        for cam in cameras:
            available += 1 if cam.get("available") == True else 0
    # at least 80% of the cameras should be available to pass
    assert available >= (total * 0.8), "more than 20% of cameras unavailable"

def verifyTrafficCameras(conn):
    locations = getJSON("trafficcameras.json", conn)
    for loc in locations:
        try:
            verifyTrafficCameraLocation(loc, conn)
        except Exception as e:
            fail("TrafficCameraLocation %s: %s" % (loc.get("name"), e))
            #raise e
    verifyTrafficCameraAvailability(locations)

def verifySafetyCamera(cam, conn):
    verifyCoordinates(cam, "latitude", "longitude")
    assert cam.get("name") is not None and cam["name"] != "", "no name"
    assert cam.get("road") is not None, "no road"
    assert cam.get("speedLimit") in ["20", "30", "40", "50", "60", "70", "nsl", ""], ("invalid speed limit " + cam["speedLimit"])
    for image in cam.get("images"):
        verifyImageAtURL(image, conn)
        
def verifySafetyCameras(conn):
    for cam in getJSON("safetycameras.json", conn):
        try:
            verifySafetyCamera(cam, conn)
        except Exception as e:
            fail("SafetyCamera %s: %s" % (cam.get("name"), e))
    
def verifyIncident(incident):
    verifyCoordinates(incident, "lat", "lon")
    assert incident["title"] is not None and incident["title"] != "", "missing title"
    assert incident["desc"] is not None, "missing description"
    assert incident["type"] == "incidents" or incident["type"] == "roadworks", "invalid type"
    
def verifyIncidents(server):
    for incident in getJSON("incidents.json", conn):
        try:
            verifyIncident(incident)
        except Exception as e:
            fail("Incident %s: %s" % (cam.get("title"), e))

def verifyWeather(weather):
    verifyCoordinates(weather, "latitude", "longitude")
    assert weather.get("name") is not None and weather["name"] != "", "missing name"
    assert weather.get("temp") is not None, "missing temperature"
    assert weather.get("type") is not None
    assert weather.get("windSpeed") is not None
    assert weather.get("windDir") is not None
    float(weather["temp"]) # raises exception on parse fail

def verifyWeatherLocations(server):
    for weather in getJSON("weather.json", conn):
        try:
            verifyWeather(weather)
        except Exception as e:
            fail("Weather %s: %s" % (weather.get("name"), e))

if __name__ == "__main__":
    host = "scottraffic.co.uk"
    if len(sys.argv) > 1 and sys.argv[1] == "dev":
        host = "dev.scottraffic.co.uk"

    conn = http.client.HTTPSConnection(host)

    try:    
        verifyTrafficCameras(conn)
        verifySafetyCameras(conn)
        verifyIncidents(conn)
        verifyWeatherLocations(conn)
    except Exception as e:
        fail("Top level: " + str(e))
    
    conn.close()
            
    if fails > 0:
        print("%d failures" % fails)
        sys.exit(1)
