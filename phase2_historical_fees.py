import requests
from bs4 import BeautifulSoup
import json
import csv
import time
import os

INPUT_FILE = "phase1_services_raw.csv"
OUTPUT_FILE = "phase2_historical_fees.csv"
CHECKPOINT_FILE = "phase2_checkpoint.txt"

HEADERS = {
    "User-Agent": "Mozilla/5.0"
}

# =========================
# RESUME LOGIC
# =========================
START_INDEX = 0
if os.path.exists(CHECKPOINT_FILE):
    with open(CHECKPOINT_FILE, "r") as f:
        START_INDEX = int(f.read().strip())
    print(f"🔁 Resuming from row {START_INDEX}")

# =========================
# CSV FIELDNAMES (FIXED)
# =========================
FIELDNAMES = [
    "publicId",
    "serviceId",
    "providerId",
    "serviceName",
    "Rating",
    "ratingNumber",
    "latitude",
    "longitude",
    "ageGroup",
    "sessionType",
    "date",
    "rate",
]

session = requests.Session()
session.headers.update(HEADERS)

# =========================
# OPEN OUTPUT CSV (APPEND SAFE)
# =========================
file_exists = os.path.exists(OUTPUT_FILE)

with open(OUTPUT_FILE, "a", newline="", encoding="utf-8") as out_f:
    writer = csv.DictWriter(
        out_f,
        fieldnames=FIELDNAMES,
        extrasaction="ignore",  # <- bulletproof against future mismatches
    )

    if not file_exists:
        writer.writeheader()

    with open(INPUT_FILE, newline="", encoding="utf-8") as in_f:
        reader = list(csv.DictReader(in_f))

        for i, row in enumerate(reader):
            if i < START_INDEX:
                continue

            public_id = row["publicId"]
            url = f"https://startingblocks.gov.au/find-child-care/{public_id}"

            print(f"[{i}] Scraping {row['name']}")

            try:
                resp = session.get(url, timeout=15)
                soup = BeautifulSoup(resp.text, "html.parser")

                script = soup.find("script", id="__NEXT_DATA__")
                if not script:
                    print("  ❌ __NEXT_DATA__ not found")
                    continue

                data = json.loads(script.string)
                page_props = data["props"]["pageProps"]
                historical_fees = page_props.get("historicalFees", [])

                if not historical_fees:
                    print("  ⚠️ No historical fees")
                    continue

                for fee in historical_fees:
                    writer.writerow({
                        "publicId": row["publicId"],
                        "serviceId": row["serviceId"],
                        "providerId": row["providerId"],
                        "serviceName": row["name"],
                        "Rating": row.get("rating"),
                        "ratingNumber": row.get("ratingNumber"),
                        "latitude": row.get("lat"),
                        "longitude": row.get("lng"),
                        "ageGroup": fee.get("age_group"),
                        "sessionType": fee.get("session_type"),
                        "date": fee.get("date"),
                        "rate": fee.get("rate"),
                    })

                # save checkpoint AFTER success
                with open(CHECKPOINT_FILE, "w") as cp:
                    cp.write(str(i + 1))

            except Exception as e:
                print(f"  ❌ Error: {e}")

            time.sleep(0.3)  # faster but still polite

print("\n✅ Phase 2 complete")