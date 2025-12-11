# What's that drone? Cincinnati police drone flight lookup tool

An interactive web tool that allows Cincinnati residents to search for police drone flights near their address. Built by the Cincinnati Enquirer.

**Live Tool:** [https://www.cincinnati.com/story/news/crime/2025/12/11/look-up-cincinnati-police-drone-flights-near-your-home-address-here/87605065007/](https://www.cincinnati.com/story/news/crime/2025/12/11/look-up-cincinnati-police-drone-flights-near-your-home-address-here/87605065007/)

## Features

- 🔍 **Address Search** - Search any Cincinnati address to find nearby drone flights
- 📏 **Adjustable Radius** - Filter flights from 100 feet to 2 miles
- 🗺️ **Interactive Map** - View flight paths, click for details
- 📊 **Flight Statistics** - See total flights, closest distance, most recent activity, top reasons
- 🔗 **Social Sharing** - Share results on Facebook, X/Twitter, Reddit
- ℹ️ **Flight Purpose Matching** - Drone flights matched to police calls for service when possible

## How It Works

1. **Scrape Flight Data** (`scrape.py`)
   - Fetches drone flight paths from Cincinnati police ArcGIS FeatureServer
   - Runs daily to capture new flights
   - Stores as GeoJSON with flight metadata

2. **Match to Police Calls** (`match_flights_to_cfs.R`)
   - Downloads Cincinnati police calls for service (CFS) data from Socrata API
   - Matches CFS addresses to street centerlines for spatial accuracy
   - Matches flights to nearby calls within time/distance windows
   - Uses incremental caching for faster daily updates
   - Filters out empty/invalid flight geometries

3. **Generate Web Data** (`gen_flight_geojson.R`)
   - Processes matched flights for web display
   - Converts timestamps, calculates flight durations
   - Outputs optimized GeoJSON for the web tool

### Matching Methodology

**Flight-to-Call Matching:**
- **Spatial:** Flight path must come within 50 feet of a call location
- **Temporal:** Flight must occur within 10 minutes of the call
- **Hover Factor:** Prioritizes flights that spent time near the call location (not just passing by)

**Address Matching:**
- Parses CFS addresses to extract block numbers and street names
- Matches to Hamilton County street centerline database

**Why "Unknown"?**
- No police call occurred near the flight in the time window

## Data Sources

- **Drone Flights:** Cincinnati Police Department ArcGIS FeatureServer
- **Calls for Service:** [Cincinnati Police Dispatch Data (Socrata)](https://data.cincinnati-oh.gov/resource/gexm-h6bt.csv)
- **Street Centerlines:** Hamilton County GIS - Countywide Street Centerlines

## Setup & Usage

### Prerequisites

- **R** (4.3.0+) with packages:
  - `sf`, `tidyverse`, `lubridate`, `janitor`, `progress`, `geojsonsf`
- **Python** (3.8+) with packages:
  - `requests` (for scraping)
- **Git** (for version control)

### Installation

1. Clone the repository:
```bash
git clone https://github.com/yourusername/whats-that-drone.git
cd whats-that-drone
```

2. Install R dependencies:
```r
install.packages(c("sf", "tidyverse", "lubridate", "janitor", "progress", "geojsonsf"))
```

3. Install Python dependencies:
```bash
pip install requests
```

4. Download street centerlines data and place in `data/` folder

### Running Manually

**Full update:**
```bash
# 1. Scrape latest flights
python scrape.py

# 2. Match flights to calls
Rscript match_flights_to_cfs.R

# 3. Generate web data
Rscript gen_flight_geojson.R

# 4. Deploy (if using GitHub Pages)
git add flights.geojson
git commit -m "Update flight data"
git push
```

### Incremental Updates

The system uses intelligent caching to speed up daily updates:

**First run:** ~30 minutes (processes all CFS data and matches all flights)  
**Daily updates:** ~2-5 minutes (only processes new CFS records and new flights)  
**No new data:** <1 minute (uses cached data)

Cache files are stored in `data/cfs_with_centerlines_YYYYMMDD.rds` and automatically cleaned up after 7 days.

## Citation

If you use this data or code, please cite it and [link to the published story.](https://www.cincinnati.com/story/news/crime/2025/12/11/look-up-cincinnati-police-drone-flights-near-your-home-address-here/87605065007/)