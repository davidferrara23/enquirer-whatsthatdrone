### CINCINNATI ENQUIRER
### Cincinnati Police Department
### Scrape SkyDio Drone Flight Paths
### by David Ferrara

import requests
import json
import os
import tempfile
from datetime import datetime

# Output GeoJSON file
geojson_file = "data/flight_paths.geojson" # replace with your desired path

# ArcGIS FeatureServer endpoint
url = "https://services7.arcgis.com/mnhQTdIYDA7UoY2l/arcgis/rest/services/b322b378-b726-4bd3-93fe-bab0b311191b-production/FeatureServer/0/query"

params = {
    "where": "flight_purpose = 'Call for Service'",
    "outFields": "*",
    "f": "geojson",
    "returnGeometry": "true",
    "spatialRel": "esriSpatialRelIntersects",
    "resultOffset": 0,
    "resultRecordCount": 2000,
    "resultType": "standard"
}

def log_message(message):
    log_file = "scrape.log"
    timestamp = datetime.now().strftime("%Y-%m-%d %H:%M:%S")
    with open(log_file, "a", encoding="utf-8") as log:
        log.write(f"[{timestamp}] {message}\n")

def load_existing_features(path):
    if not os.path.exists(path):
        return [], set()

    try:
        with open(path, "r", encoding="utf-8") as f:
            existing_data = json.load(f)
        existing_features = existing_data.get("features", [])
        existing_ids = {
            feat.get("properties", {}).get("flight_id")
            for feat in existing_features
            if isinstance(feat, dict)
        }
        return existing_features, existing_ids
    except json.JSONDecodeError as e:
        timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
        corrupt_path = f"{path}.{timestamp}.corrupt"
        try:
            os.replace(path, corrupt_path)
            msg = (
                f"⚠️ Existing file is corrupt JSON ({e}). Moved to {corrupt_path} and rebuilding from API."
            )
        except Exception as move_err:
            msg = (
                f"⚠️ Existing file is corrupt JSON ({e}) and could not be moved ({move_err}). "
                "Proceeding with empty dataset in memory."
            )
        print(msg)
        log_message(msg)
        return [], set()


# Load existing features if file exists
existing_features, existing_ids = load_existing_features(geojson_file)

page_size = int(params.get("resultRecordCount", 2000))
offset = 0
fetched = 0
appended = 0

while True:
    try:
        params["resultOffset"] = offset
        params["resultRecordCount"] = page_size
        response = requests.get(url, params=params, timeout=60)

        if response.status_code != 200:
            msg = f"❌ Failed to fetch data (offset={offset}): {response.status_code}"
            print(msg)
            log_message(msg)
            break

        data = response.json()
        features = data.get("features", [])
        num = len(features)
        fetched += num

        if num == 0:
            # No more results
            break

        # Append only new features
        for feat in features:
            fid = feat["properties"].get("flight_id")
            if fid not in existing_ids:
                existing_features.append(feat)
                existing_ids.add(fid)
                appended += 1

        # If fewer than page_size returned, we're at the end
        if num < page_size:
            break

        offset += page_size
    except requests.RequestException as e:
        msg = f"❌ Network error while fetching data (offset={offset}): {e}"
        print(msg)
        log_message(msg)
        break
    except KeyboardInterrupt:
        msg = f"⚠️ Scrape interrupted at offset={offset}. Saving progress from fetched pages..."
        print(msg)
        log_message(msg)
        break

# Save combined features if any appended
if appended > 0:
    out = {"type": "FeatureCollection", "features": existing_features}
    tmp_path = None
    try:
        target_dir = os.path.dirname(os.path.abspath(geojson_file)) or "."
        os.makedirs(target_dir, exist_ok=True)
        fd, tmp_path = tempfile.mkstemp(prefix="flight_paths_", suffix=".geojson.tmp", dir=target_dir)

        # Write to a temp file first, then atomically replace the target file.
        with os.fdopen(fd, "w", encoding="utf-8") as f:
            json.dump(out, f)
            f.flush()
            os.fsync(f.fileno())

        os.replace(tmp_path, geojson_file)
        msg = f"✅ Fetched {fetched} features, appended {appended} new flight paths to {geojson_file}"
        print(msg)
        log_message(msg)
    except KeyboardInterrupt:
        if tmp_path and os.path.exists(tmp_path):
            os.remove(tmp_path)
        msg = "⚠️ Save interrupted. Temp file cleaned up; target file was not replaced."
        print(msg)
        log_message(msg)
        raise SystemExit(1)
    except Exception as e:
        if tmp_path and os.path.exists(tmp_path):
            os.remove(tmp_path)
        msg = f"❌ Failed to save {geojson_file} atomically: {e}"
        print(msg)
        log_message(msg)
else:
    msg = f"ℹ️ Fetched {fetched} features. No new flight paths to append."
    print(msg)
    log_message(msg)
