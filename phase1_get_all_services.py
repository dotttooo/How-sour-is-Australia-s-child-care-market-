import requests
import csv
import time
import os
import json

ALGOLIA_APP_ID = "CGQW4YLCUR"
ALGOLIA_API_KEY = "59d33900544ce513400031e6bff95522"
INDEX_NAME = "production_services"

URL = f"https://{ALGOLIA_APP_ID}-2.algolianet.com/1/indexes/*/queries"

HEADERS = {
    "x-algolia-application-id": ALGOLIA_APP_ID,
    "x-algolia-api-key": ALGOLIA_API_KEY,
    "content-type": "application/json",
    "accept": "application/json",
    "origin": "https://startingblocks.gov.au",
    "referer": "https://startingblocks.gov.au/",
}

# Australia bounding box
LAT_MIN, LAT_MAX = -44.0, -10.0
LNG_MIN, LNG_MAX = 112.0, 154.0
STEP = 0.4
RADIUS = 25_000

CSV_FILE = "phase1_services_raw.csv"
CHECKPOINT_FILE = "checkpoint.json"

# ----------------------------
# Load previous progress
# ----------------------------
services = {}
start_lat, start_lng = LAT_MIN, LNG_MIN

if os.path.exists(CSV_FILE):
    print(f"Loading existing services from {CSV_FILE}...")
    with open(CSV_FILE, "r", encoding="utf-8") as f:
        reader = csv.DictReader(f)
        for row in reader:
            services[row["serviceId"]] = row

if os.path.exists(CHECKPOINT_FILE):
    print(f"Resuming from checkpoint {CHECKPOINT_FILE}...")
    with open(CHECKPOINT_FILE, "r", encoding="utf-8") as f:
        checkpoint = json.load(f)
        start_lat = checkpoint.get("lat", LAT_MIN)
        start_lng = checkpoint.get("lng", LNG_MIN)

# ----------------------------
# Scraping loop
# ----------------------------
lat = start_lat
while lat <= LAT_MAX:
    lng = start_lng if lat == start_lat else LNG_MIN
    while lng <= LNG_MAX:
        try:
            print(f"Searching @ {lat:.2f},{lng:.2f}")

            payload = {
                "requests": [
                    {
                        "indexName": INDEX_NAME,
                        "params": (
                            f"aroundLatLng={lat},{lng}"
                            f"&aroundRadius={RADIUS}"
                            f"&hitsPerPage=1000"
                            f"&page=0"
                        )
                    }
                ]
            }

            r = requests.post(URL, headers=HEADERS, json=payload, timeout=30)
            r.raise_for_status()

            hits = r.json()["results"][0]["hits"]

            for h in hits:
                services[h["serviceId"]] = {
                    "serviceId": h["serviceId"],
                    "publicId": h.get("publicId"),
                    "providerId": h.get("providerId"),
                    "name": h.get("name"),
                    "rating": h.get("rating"),
                    "ratingNumber": h.get("ratingNumber"),
                    "lat": h.get("_geoloc", {}).get("lat"),
                    "lng": h.get("_geoloc", {}).get("lng"),
                }

            # ----------------------------
            # Save checkpoint and CSV
            # ----------------------------
            with open(CSV_FILE, "w", newline="", encoding="utf-8") as f:
                writer = csv.DictWriter(
                    f,
                    fieldnames=[
                        "serviceId",
                        "publicId",
                        "providerId",
                        "name",
                        "rating",
                        "ratingNumber",
                        "lat",
                        "lng",
                    ],
                )
                writer.writeheader()
                writer.writerows(services.values())

            with open(CHECKPOINT_FILE, "w", encoding="utf-8") as f:
                json.dump({"lat": lat, "lng": lng}, f)

            # polite throttling
            time.sleep(0.15)

            lng += STEP

        except Exception as e:
            print(f"Error at {lat},{lng}: {e}")
            print("Sleeping for 10 seconds and retrying...")
            time.sleep(10)

    lat += STEP

print(f"Total unique services: {len(services)}")
print(f"Saved final CSV: {CSV_FILE}")

# Remove checkpoint when finished
if os.path.exists(CHECKPOINT_FILE):
    os.remove(CHECKPOINT_FILE)