### CINCINNATI ENQUIRER
### gen_flight_geojson.R
### Create a single optimized GeoJSON for web use
### by David Ferrara

library(sf)
library(tidyverse)

### Load flight data (use CFS-matched version if it exists)
flight_input <- if (file.exists("./whats-that-drone/data/flight_paths_matched.geojson")) {
  message("Loading CFS-matched flight data...")
  "./whats-that-drone/data/flight_paths_matched.geojson"
} else {
  message("Loading original flight data...")
  "./whats-that-drone/data/flight_paths.geojson"
}

flights_sf <- st_read(flight_input)
flights_sf$takeoff <- as.POSIXct(flights_sf$takeoff / 1000, origin = "1970-01-01", tz = "America/New_York")
flights_sf$landing <- as.POSIXct(flights_sf$landing / 1000, origin = "1970-01-01", tz = "America/New_York")
flights_sf <- flights_sf %>% distinct(flight_id, .keep_all = TRUE)
flights_sf <- st_transform(flights_sf, 4326)

# Clean geometries
extract_linestring <- function(geom) {
  geom_type <- st_geometry_type(geom)
  if (geom_type == "LINESTRING") return(geom)
  if (geom_type == "MULTILINESTRING") return(st_cast(geom, "LINESTRING")[[1]])
  if (geom_type == "GEOMETRYCOLLECTION") {
    collection <- st_collection_extract(geom, type = "LINESTRING")
    if (length(collection) > 0) return(st_geometry(collection)[[1]])
  }
  return(NULL)
}

clean_geoms <- lapply(1:nrow(flights_sf), function(i) extract_linestring(flights_sf$geometry[i]))
valid_indices <- !sapply(clean_geoms, is.null)

flights_clean <- flights_sf[valid_indices, ] %>%
  st_drop_geometry() %>%
  mutate(geometry = st_sfc(clean_geoms[valid_indices], crs = st_crs(flights_sf))) %>%
  st_as_sf() %>%
  select(flight_id, takeoff, landing, flight_purpose, geometry)

cat("✓ Cleaned to", nrow(flights_clean), "flights\n")

# Write single GeoJSON file
st_write(flights_clean, "whats-that-drone/flights.geojson", delete_dsn = TRUE)

file_size <- file.info("whats-that-drone/flights.geojson")$size / 1024 / 1024
cat("✓ Created flights.geojson -", round(file_size, 2), "MB\n")