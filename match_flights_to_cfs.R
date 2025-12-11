### CINCINNATI ENQUIRER
### match_cfs_to_flights.R
### Match calls for service to flight paths and update flight_purpose field
### Run this BEFORE gen_flight_geojson.R
### by David Ferrara

### VARS

cfs_csv <- "./whats-that-drone/data/cfs_2025_092125.csv"  # adjust path as needed
flight_paths_geojson <- "./flight_paths.geojson"  # input flight paths
county_centerlines_geojson <- "./whats-that-drone/data/Countywide_Street_Centerlines.geojson"

# Cache files
today_date <- format(Sys.Date(), "%Y%m%d")
cfs_cache_file <- paste0("./whats-that-drone/data/cfs_with_centerlines_", today_date, ".rds")
previous_matched_file <- "./whats-that-drone/data/flight_paths_matched.geojson"

### INITIALIZE

library(sf)
library(tidyverse)
library(lubridate)
library(janitor)
library(progress)

### FUNCTIONS

# Parse CFS address
parse_cfs_address <- function(address) {
  if(is.na(address) || address == "") {
    return(list(block_num = NA, street_name = NA))
  }
  
  pattern <- "^([0-9]+)XX\\s+(.+)$"
  if(!grepl(pattern, address)) {
    return(list(block_num = NA, street_name = NA))
  }
  
  block_num <- as.numeric(gsub(pattern, "\\1", address)) * 100
  street_name <- toupper(trimws(gsub(pattern, "\\2", address)))
  
  return(list(block_num = block_num, street_name = street_name))
}

# Match address to centerline
match_address_to_centerline <- function(address, centerlines_df) {
  parsed <- parse_cfs_address(address)
  if(is.na(parsed$block_num)) return(NULL)
  
  block_num <- parsed$block_num
  street_name <- parsed$street_name
  
  matches <- centerlines_df %>%
    filter(street_full == street_name)
  
  if(nrow(matches) == 0) return(NULL)
  
  block_matches <- matches %>%
    filter(
      (block_num >= left_block_from & block_num <= left_block_to) |
        (block_num >= right_block_from & block_num <= right_block_to)
    )
  
  if(nrow(block_matches) > 0) {
    return(block_matches[1,])
  } else {
    matches <- matches %>%
      mutate(
        distance = pmin(
          abs(block_num - left_block_from),
          abs(block_num - left_block_to),
          abs(block_num - right_block_from),
          abs(block_num - right_block_to)
        )
      ) %>%
      arrange(distance)
    return(matches[1,])
  }
}

# Match CFS to centerlines
match_cfs_to_centerlines <- function(cfs_df, centerlines_df) {
  message("Matching CFS addresses to street centerlines...")
  
  total_rows <- nrow(cfs_df)
  pb <- progress::progress_bar$new(
    format = " Matching CFS [:bar] :percent (:current/:total) ETA: :eta",
    total = total_rows,
    clear = FALSE,
    width = 60
  )
  
  cfs_with_streets <- cfs_df %>%
    rowwise() %>%
    mutate(
      street_segment = {
        pb$tick()
        list(match_address_to_centerline(address_x, centerlines_df))
      },
      has_street_match = !is.null(street_segment)
    ) %>%
    ungroup()
  
  pb$terminate()
  
  matched_idx <- which(cfs_with_streets$has_street_match)
  if (length(matched_idx) > 0) {
    cfs_streets_sf <- cfs_with_streets[matched_idx, ]
    sfc_list <- purrr::map(cfs_with_streets$street_segment[matched_idx], function(s) {
      if (is.null(s) || !inherits(s, "sf")) return(NULL)
      g <- sf::st_geometry(s)
      if (length(g) < 1) return(NULL)
      sf::st_sfc(g[[1]], crs = sf::st_crs(s))
    })
    geom_sfc <- do.call(c, sfc_list)
    sf::st_geometry(cfs_streets_sf) <- geom_sfc
    sf::st_crs(cfs_streets_sf) <- sf::st_crs(centerlines_df)
    cfs_streets_sf <- cfs_streets_sf %>%
      select(-street_segment, -has_street_match) %>%
      mutate(match_type = "street")
  } else {
    cfs_streets_sf <- cfs_with_streets[0, ] %>%
      st_as_sf(crs = st_crs(centerlines_df)) %>%
      mutate(match_type = character())
  }
  
  cfs_points_sf <- cfs_with_streets %>%
    filter(!has_street_match) %>%
    st_as_sf(coords = c("longitude_x", "latitude_x"), crs = 4326, remove = FALSE) %>%
    st_transform(st_crs(centerlines_df)) %>%
    select(-street_segment, -has_street_match) %>%
    mutate(match_type = "point")
  
  message(sprintf("Matched %d of %d calls to street segments (%.1f%%)",
                  nrow(cfs_streets_sf), nrow(cfs_df),
                  ifelse(nrow(cfs_df) == 0, 0, nrow(cfs_streets_sf)/nrow(cfs_df)*100)))
  
  combined_sf <- if (nrow(cfs_streets_sf) == 0) {
    cfs_points_sf
  } else if (nrow(cfs_points_sf) == 0) {
    cfs_streets_sf
  } else {
    rbind(cfs_streets_sf, cfs_points_sf)
  }
  
  return(combined_sf)
}

# Match flights to CFS
match_flights_to_cfs <- function(cfs_sf, flights_sf, buffer_distance = 50, time_window_mins = 10) {
  if (st_crs(cfs_sf) != st_crs(flights_sf)) {
    cfs_sf <- st_transform(cfs_sf, st_crs(flights_sf))
  }
  
  cfs_sf <- cfs_sf %>% filter(!is.na(create_time_incident), !is.na(closed_time_incident))
  
  matches_list <- list()
  match_count <- 0
  total_flights <- nrow(flights_sf)
  
  if (total_flights == 0) {
    message("No flights to process.")
    return(NULL)
  }
  
  pb <- progress::progress_bar$new(
    format = " Matching flights [:bar] :percent (:current/:total) ETA: :eta",
    total = total_flights,
    clear = FALSE,
    width = 60
  )
  
  for (i in seq_len(total_flights)) {
    pb$tick()
    
    flight <- flights_sf[i, ]
    if (is.na(flight$takeoff) || is.na(flight$landing)) next
    
    ft <- flight$takeoff
    fl <- flight$landing
    window_start <- ft - as.difftime(time_window_mins, units = "mins")
    
    temporally_possible_calls <- cfs_sf %>%
      filter(
        create_time_incident <= ft,
        create_time_incident <= fl,
        create_time_incident >= window_start,
        closed_time_incident >= ft
      )
    
    if (nrow(temporally_possible_calls) == 0) next
    
    temporally_possible_buffers <- st_buffer(temporally_possible_calls, buffer_distance)
    intersects <- st_intersects(flight, temporally_possible_buffers, sparse = FALSE)[1, ]
    if (!any(intersects)) next
    
    intersecting_calls <- temporally_possible_calls[intersects, ]
    intersecting_buffers <- temporally_possible_buffers[intersects, ]
    
    for (j in seq_len(nrow(intersecting_calls))) {
      call <- intersecting_calls[j, ]
      call_buffer <- intersecting_buffers[j, ]
      
      call_create <- call$create_time_incident
      time_diff <- as.numeric(difftime(ft, call_create, units = "mins"))
      
      if (call_create > ft) next
      if (call$closed_time_incident < ft) next
      
      dist <- as.numeric(st_distance(flight, call))
      
      flight_intersection <- st_intersection(flight, call_buffer)
      hover_factor <- if (length(flight_intersection) > 0 && !st_is_empty(flight_intersection)) {
        min(as.numeric(st_length(flight_intersection)) / as.numeric(st_length(flight)), 1)
      } else 0
      
      match_type <- if (!is.null(call$match_type)) call$match_type else "point"
      match_count <- match_count + 1
      
      matches_list[[match_count]] <- data.frame(
        flight_id = flight$object_id,
        call_id = call$event_number,
        incident_type = call$incident_type_id,
        disposition = call$disposition_text,
        address = call$address_x,
        time_diff_mins = time_diff,
        distance_m = dist,
        hover_factor = hover_factor,
        match_score = (1 - pmin(abs(time_diff) / time_window_mins, 1)) * 0.4 +
          (1 - pmin(dist / buffer_distance, 1)) * 0.3 +
          hover_factor * 0.3,
        stringsAsFactors = FALSE
      )
    }
  }
  
  pb$terminate()
  
  if (length(matches_list) == 0) {
    message("No matches found.")
    return(NULL)
  }
  
  matches_df <- bind_rows(matches_list) %>%
    group_by(flight_id) %>%
    arrange(desc(match_score)) %>%
    slice(1) %>%  # Keep only best match per flight
    ungroup()
  
  return(matches_df)
}

### LOAD CFS DATA (use cache or process)

# Check if we have today's cached CFS data
if (file.exists(cfs_cache_file)) {
  message("✓ Found cached CFS data from today: ", cfs_cache_file)
  cfs_with_centerlines <- readRDS(cfs_cache_file)
  message("✓ Loaded ", nrow(cfs_with_centerlines), " CFS records with centerlines")
} else {
  # Check for yesterday's cache - use as base for CFS
  yesterday_date <- format(Sys.Date() - 1, "%Y%m%d")
  yesterday_cache <- paste0("./whats-that-drone/data/cfs_with_centerlines_", yesterday_date, ".rds")
  
  if (file.exists(yesterday_cache)) {
    message("✓ Found yesterday's CFS cache - using as base")
    cfs_with_centerlines <- readRDS(yesterday_cache)
    latest_cfs_date <- max(cfs_with_centerlines$create_time_incident, na.rm = TRUE)
    message("✓ Latest CFS in cache: ", latest_cfs_date)
    
    # Only fetch NEW CFS records since yesterday
    message("Fetching only NEW CFS records from API...")
    latest_date <- format(latest_cfs_date, "%Y-%m-%dT%H:%M:%S")
    base_url <- "https://data.cincinnati-oh.gov/resource/gexm-h6bt.csv"
    limit <- 1000
    offset <- 0
    all_new_rows <- list()
    
    # First, get total count for progress bar
    count_url <- paste0(base_url, "?$select=count(*)&$where=", 
                        URLencode(paste0("create_time_incident>'", latest_date, "'"), reserved = TRUE))
    total_count <- tryCatch({
      count_result <- read.csv(count_url, stringsAsFactors = FALSE)
      as.numeric(count_result[1,1])
    }, error = function(e) {
      message("Could not get count, will fetch until no more records")
      NA
    })
    
    if (!is.na(total_count) && total_count > 0) {
      message("Found ", total_count, " new CFS records to fetch")
      pb <- progress::progress_bar$new(
        format = " Fetching CFS [:bar] :percent (:current/:total) ETA: :eta",
        total = ceiling(total_count / limit),
        clear = FALSE,
        width = 60
      )
    } else {
      pb <- NULL
    }
    
    repeat {
      where_value <- paste0("create_time_incident>'", latest_date, "'")
      socrata_url <- paste0(base_url, "?$where=", URLencode(where_value, reserved = TRUE), 
                            "&$limit=", limit, "&$offset=", format(offset, scientific = FALSE))
      
      batch <- tryCatch(
        read.csv(socrata_url, stringsAsFactors = FALSE, na.strings = c("", "NA")),
        error = function(e) { 
          message("Error fetching batch: ", e$message)
          return(data.frame()) 
        }
      )
      
      if (nrow(batch) == 0) break
      
      all_new_rows[[length(all_new_rows) + 1]] <- batch
      if (!is.null(pb)) pb$tick()
      
      offset <- offset + limit
      if (nrow(batch) < limit) break
    }
    
    if (!is.null(pb)) pb$terminate()

    new_cfs <- bind_rows(all_new_rows)
    
    if (nrow(new_cfs) > 0) {
      message("✓ Found ", nrow(new_cfs), " NEW CFS records")
      
      # Process new CFS records
      new_cfs <- new_cfs %>%
        clean_names() %>%
        mutate(
          create_time_incident = as.POSIXct(create_time_incident, format = "%Y-%m-%dT%H:%M:%S", tz = "America/New_York"),
          closed_time_incident = as.POSIXct(closed_time_incident, format = "%Y-%m-%dT%H:%M:%S", tz = "America/New_York")
        ) %>%
        filter(
          create_time_incident >= as.POSIXct("2025-07-28 00:00:00", tz = "America/New_York"),
          !(disposition_text %in% c("USED CLEAR BUTTON", "CAN:CANCEL", "DUP: DUPLICATE", 
                                    "TC:TRANSFERRED CALL", "26: AVAIL/DETAIL COMPLETED",
                                    "TEST: TEST", "ERR: ERROR INCIDENT", "NOT NOT A DISPOSITION",
                                    "UCPD DETAIL COMPLETE", "UCPD NO SERVICES RE", "NR:NO REPORT"))
        )
      
      # Convert to spatial and match to centerlines
      new_cfs_sf <- st_as_sf(new_cfs, coords = c("longitude_x", "latitude_x"), crs = 4326, remove = FALSE) %>%
        st_transform(32616)
      
      # Load centerlines (only if we have new CFS to match)
      message("Loading centerlines for new CFS...")
      centerlines <- st_read(county_centerlines_geojson, quiet = TRUE) %>%
        clean_names() %>%
        mutate(
          street_full = case_when(
            !is.na(prefix) & prefix != "" ~ paste(toupper(trimws(prefix)), toupper(trimws(name)), toupper(trimws(suffix))),
            TRUE ~ paste(toupper(trimws(name)), toupper(trimws(suffix)))
          ),
          street_full = trimws(street_full),
          left_from = as.numeric(l_f_add),
          left_to = as.numeric(l_t_add),
          right_from = as.numeric(r_f_add),
          right_to = as.numeric(r_t_add),
          left_block_from = floor(left_from/100) * 100,
          left_block_to = floor(left_to/100) * 100,
          right_block_from = floor(right_from/100) * 100,
          right_block_to = floor(right_to/100) * 100
        ) %>%
        filter(!is.na(street_full), street_full != "", (!is.na(left_from) | !is.na(right_from)))
      
      new_cfs_matched <- match_cfs_to_centerlines(new_cfs_sf, centerlines)
      
      # Ensure column consistency before combining
      common_cols <- intersect(names(cfs_with_centerlines), names(new_cfs_matched))
      
      # Also check for geometry column type consistency
      if (!"geometry" %in% common_cols && st_crs(cfs_with_centerlines) != st_crs(new_cfs_matched)) {
        new_cfs_matched <- st_transform(new_cfs_matched, st_crs(cfs_with_centerlines))
      }
      
      # Select only common columns from both
      cfs_old <- cfs_with_centerlines %>% select(all_of(common_cols))
      cfs_new <- new_cfs_matched %>% select(all_of(common_cols))
      
      # Combine with yesterday's cache
      cfs_with_centerlines <- rbind(cfs_old, cfs_new)
      message("✓ Combined: ", nrow(cfs_with_centerlines), " total CFS records")
    } else {
      message("✓ No new CFS records - using yesterday's cache as-is")
    }
    
    # Save today's cache (whether we added new records or not)
    message("💾 Saving today's CFS cache: ", cfs_cache_file)
    saveRDS(cfs_with_centerlines, cfs_cache_file)
    message("✓ Cache saved for ", today_date)
    
  } else {
    # No cache exists - do full process (your existing code)
    message("No cache found - processing from scratch...")
    message("Loading calls for service data...")
    cfs <- read_csv(
      cfs_csv,
      col_types = cols(
        ADDRESS_X = col_character(),
        AGENCY = col_character(),
        DISPOSITION_TEXT = col_character(),
        EVENT_NUMBER = col_character(),
        INCIDENT_TYPE_ID = col_character(),
        INCIDENT_TYPE_DESC = col_character(),
        PRIORITY = col_integer(),
        CREATE_TIME_INCIDENT = col_datetime(format = "%Y %b %d %H:%M:%S %p"),
        ARRIVAL_TIME_PRIMARY_UNIT = col_datetime(format = "%Y %b %d %H:%M:%S %p"),
        CLOSED_TIME_INCIDENT = col_datetime(format = "%Y %b %d %H:%M:%S %p"),
        DISPATCH_TIME_PRIMARY_UNIT = col_datetime(format = "%Y %b %d %H:%M:%S %p"),
        BEAT = col_character(),
        DISTRICT = col_character(),
        CPD_NEIGHBORHOOD = col_character(),
        LATITUDE_X = col_double(),
        LONGITUDE_X = col_double()
      )
    )
    
    cfs <- cfs %>%
      clean_names() %>%
      mutate(
        create_time_incident = lubridate::force_tz(create_time_incident, "America/New_York"),
        closed_time_incident = lubridate::force_tz(closed_time_incident, "America/New_York")
      )
    
    # Load most recent CAD calls via Socrata API
    message("Checking for new CFS data from Socrata API...")
    latest_date <- format(max(cfs$create_time_incident, na.rm = TRUE), "%Y-%m-%dT%H:%M:%S")
    base_url <- "https://data.cincinnati-oh.gov/resource/gexm-h6bt.csv"
    limit <- 1000
    offset <- 0
    all_new_rows <- list()
    
    # Get total count for progress bar
    count_url <- paste0(base_url, "?$select=count(*)&$where=", 
                        URLencode(paste0("create_time_incident>'", latest_date, "'"), reserved = TRUE))
    total_count <- tryCatch({
      count_result <- read.csv(count_url, stringsAsFactors = FALSE)
      as.numeric(count_result[1,1])
    }, error = function(e) {
      message("Could not get count, will fetch until no more records")
      NA
    })
    
    if (!is.na(total_count) && total_count > 0) {
      message("Found ", total_count, " new CFS records to fetch from API")
      pb <- progress::progress_bar$new(
        format = " Fetching CFS [:bar] :percent (:current/:total) ETA: :eta",
        total = ceiling(total_count / limit),
        clear = FALSE,
        width = 60
      )
    } else {
      message("Checking API for new records...")
      pb <- NULL
    }
    
    repeat {
      where_value <- paste0("create_time_incident>'", latest_date, "'")
      socrata_url <- paste0(base_url, "?$where=", URLencode(where_value, reserved = TRUE), 
                            "&$limit=", limit, "&$offset=", format(offset, scientific = FALSE))

      batch <- tryCatch(
        read.csv(socrata_url, stringsAsFactors = FALSE, na.strings = c("", "NA")),
        error = function(e) {
          message("Error fetching URL: ", e$message)
          return(data.frame())
        }
      )

      if (nrow(batch) == 0) {
        break
      }

      all_new_rows[[length(all_new_rows) + 1]] <- batch
      if (!is.null(pb)) pb$tick()
      
      offset <- offset + limit
      if (nrow(batch) < limit) break
    }
    
    if (!is.null(pb)) pb$terminate()

    # If new rows exist, combine with static data
    if (nrow(new_rows) > 0) {
      message("Found ", nrow(new_rows), " new CFS records from API")
      
      new_rows <- new_rows %>%
        clean_names() %>%
        mutate(
          address_x = as.character(address_x),
          agency = as.character(agency),
          disposition_text = as.character(disposition_text),
          event_number = as.character(event_number),
          incident_type_id = as.character(incident_type_id),
          incident_type_desc = as.character(incident_type_desc),
          priority = as.integer(priority),
          create_time_incident = as.POSIXct(create_time_incident, format = "%Y-%m-%dT%H:%M:%S", tz = "America/New_York"),
          arrival_time_primary_unit = as.POSIXct(arrival_time_primary_unit, format = "%Y-%m-%dT%H:%M:%S", tz = "America/New_York"),
          closed_time_incident = as.POSIXct(closed_time_incident, format = "%Y-%m-%dT%H:%M:%S", tz = "America/New_York"),
          dispatch_time_primary_unit = as.POSIXct(dispatch_time_primary_unit, format = "%Y-%m-%dT%H:%M:%S", tz = "America/New_York"),
          beat = as.character(beat),
          district = as.character(district),
          cpd_neighborhood = as.character(cpd_neighborhood),
          latitude_x = as.numeric(latitude_x),
          longitude_x = as.numeric(longitude_x)
        ) %>%
        select(names(cfs))
      
      cfs <- bind_rows(cfs, new_rows)
      message("✓ Combined with ", nrow(new_rows), " new records from API")
    } else {
      message("✓ No new CFS data available from API")
    }
    
    # Add calculated columns
    cfs <- cfs %>%
      mutate(
        time_to_close_mins = as.numeric(difftime(closed_time_incident, create_time_incident, units = "mins")),
        time_before_arrival = as.numeric(difftime(arrival_time_primary_unit, create_time_incident, units = "mins")),
        time_before_dispatch = as.numeric(difftime(dispatch_time_primary_unit, create_time_incident, units = "mins"))
      )
    
    # Filter to relevant calls since Skydio launch
    cfs_since_launch <- cfs %>%  
      filter(
        create_time_incident >= as.POSIXct("2025-07-28 00:00:00", tz = "America/New_York")
      ) %>%
      filter(
        !(disposition_text %in% c(
          "USED CLEAR BUTTON", "CAN:CANCEL", "DUP: DUPLICATE", 
          "TC:TRANSFERRED CALL", "26: AVAIL/DETAIL COMPLETED",
          "TEST: TEST", "ERR: ERROR INCIDENT", "NOT NOT A DISPOSITION",
          "UCPD DETAIL COMPLETE", "UCPD NO SERVICES RE", "NR:NO REPORT"
        ))
      ) %>%
      arrange(create_time_incident)
    
    # Convert to spatial
    cfs_sf <- st_as_sf(
      cfs_since_launch,
      coords = c("longitude_x", "latitude_x"),
      crs = 4326,
      remove = FALSE
    )
    cfs_sf <- st_transform(cfs_sf, 32616)  # UTM zone 16N
    
    message("Loading centerlines data...")
    centerlines <- st_read(county_centerlines_geojson, quiet = TRUE)
    
    centerlines_clean <- centerlines %>%
      janitor::clean_names() %>%
      mutate(
        street_full = case_when(
          !is.na(prefix) & prefix != "" ~ paste(toupper(trimws(prefix)), toupper(trimws(name)), toupper(trimws(suffix))),
          TRUE ~ paste(toupper(trimws(name)), toupper(trimws(suffix)))
        ),
        street_full = trimws(street_full),
        left_from = as.numeric(l_f_add),
        left_to = as.numeric(l_t_add),
        right_from = as.numeric(r_f_add),
        right_to = as.numeric(r_t_add),
        left_block_from = floor(left_from/100) * 100,
        left_block_to = floor(left_to/100) * 100,
        right_block_from = floor(right_from/100) * 100,
        right_block_to = floor(right_to/100) * 100
      ) %>%
      filter(!is.na(street_full), street_full != "",
             (!is.na(left_from) | !is.na(right_from)))
    
    ### MATCH CFS TO CENTERLINES
    
    cfs_with_centerlines <- match_cfs_to_centerlines(cfs_sf, centerlines_clean)
    
    # Save the processed CFS data with today's date
    message("💾 Saving processed CFS data to cache: ", cfs_cache_file)
    saveRDS(cfs_with_centerlines, cfs_cache_file)
    message("✓ Cache saved - future runs today will be much faster!")
    
    # Don't delete old caches here - do it at the end
  }
}

# Clean up old CFS caches (keep last 7 days) - MOVED TO AFTER cache is saved
all_cache_files <- list.files(
  path = "./whats-that-drone/data",
  pattern = "^cfs_with_centerlines_.*\\.rds$",
  full.names = TRUE
)

if (length(all_cache_files) > 7) {
  # Sort by date, keep newest 7
  file_dates <- as.Date(gsub(".*_(\\d{8})\\.rds$", "\\1", all_cache_files), "%Y%m%d")
  old_files <- all_cache_files[order(file_dates)][1:(length(all_cache_files) - 7)]
  file.remove(old_files)
  message("🗑️  Removed ", length(old_files), " old cache files")
}

### LOAD FLIGHT DATA - INCREMENTAL APPROACH
message("Loading flight data...")
all_flights <- st_read(flight_paths_geojson, quiet = TRUE) %>%
  clean_names() %>%
  mutate(
    takeoff = as.POSIXct(takeoff / 1000, origin = "1970-01-01", tz = "America/New_York"),
    landing = as.POSIXct(landing / 1000, origin = "1970-01-01", tz = "America/New_York")
  )

# Load previously matched flights
if (file.exists(previous_matched_file)) {
  message("✓ Found previous matched flights")
  previous_flights <- st_read(previous_matched_file, quiet = TRUE) %>%
    clean_names() %>%
    mutate(
      takeoff = as.POSIXct(takeoff / 1000, origin = "1970-01-01", tz = "America/New_York"),
      landing = as.POSIXct(landing / 1000, origin = "1970-01-01", tz = "America/New_York")
    )
  
  # Find NEW flights (not in previous matched file)
  new_flight_ids <- setdiff(all_flights$flight_id, previous_flights$flight_id)
  
  # Also check if we have NEW CFS data that might match OLD flights
  # Compare today's CFS count vs yesterday's
  yesterday_cfs_count <- if (exists("yesterday_cache") && file.exists(yesterday_cache)) {
    nrow(readRDS(yesterday_cache))
  } else {
    0
  }
  current_cfs_count <- nrow(cfs_with_centerlines)
  
  if (length(new_flight_ids) > 0) {
    message("✓ Found ", length(new_flight_ids), " NEW flights to match")
    
    new_flights <- all_flights %>%
      filter(flight_id %in% new_flight_ids) %>%
      select(flight_id, object_id, takeoff, landing, flight_purpose, geometry) %>%
      distinct(flight_id, .keep_all = TRUE) %>%
      st_transform(32616)
    
    # Match ONLY the new flights
    new_matches <- match_flights_to_cfs(cfs_with_centerlines, new_flights, 
                                        buffer_distance = 50, time_window_mins = 10)
    
    if (!is.null(new_matches) && nrow(new_matches) > 0) {
      new_flights <- new_flights %>%
        left_join(new_matches %>% select(flight_id, incident_type), 
                  by = c("object_id" = "flight_id")) %>%
        mutate(
          # Don't use coalesce - overwrite with match or reset to default
          flight_purpose = if_else(!is.na(incident_type), incident_type, "Call for Service")
        ) %>%
        select(-incident_type)
      
      message("✓ Matched ", nrow(new_matches), " new flights to CFS")
    } else {
      # No matches - just update flight_purpose to default
      new_flights <- new_flights %>%
        mutate(flight_purpose = "Call for Service")
      message("✓ No matches found for new flights - set to 'Call for Service'")
    }
    
    # Ensure both dataframes have same columns before rbind
    new_flights <- st_transform(new_flights, 4326)
    
    # Select same columns in both (in case previous_flights has extra columns)
    common_cols <- c("flight_id", "object_id", "takeoff", "landing", "flight_purpose", "geometry")
    
    # Make sure both have all needed columns
    for (col in common_cols) {
      if (!col %in% names(previous_flights)) {
        previous_flights[[col]] <- NA
      }
      if (!col %in% names(new_flights)) {
        new_flights[[col]] <- NA
      }
    }
    
    previous_flights <- previous_flights %>% select(all_of(common_cols))
    new_flights <- new_flights %>% select(all_of(common_cols))
    
    all_matched_flights <- rbind(previous_flights, new_flights)
    message("✓ Total flights: ", nrow(all_matched_flights))
    
  } else if (current_cfs_count > yesterday_cfs_count) {
    message("✓ No new flights, but ", current_cfs_count - yesterday_cfs_count, " new CFS records")
    message("   Re-matching ALL flights to capture new CFS matches...")
    
    # Re-match everything when new CFS data arrives
    flights_sf <- all_flights %>%
      select(flight_id, object_id, takeoff, landing, geometry) %>%
      mutate(flight_purpose = "Call for Service") %>%  # Reset all to default
      distinct(flight_id, .keep_all = TRUE) %>%
      st_transform(32616)
    
    matches <- match_flights_to_cfs(cfs_with_centerlines, flights_sf, 
                                    buffer_distance = 50, time_window_mins = 10)
    
    if (!is.null(matches) && nrow(matches) > 0) {
      flights_sf <- flights_sf %>%
        left_join(matches %>% select(flight_id, incident_type), 
                  by = c("object_id" = "flight_id")) %>%
        mutate(flight_purpose = if_else(!is.na(incident_type), incident_type, "Call for Service")) %>%
        select(-incident_type)
    }
    
    all_matched_flights <- st_transform(flights_sf, 4326)
    message("✓ Re-matched all flights")
    
  } else {
    message("✓ No new flights or CFS - using previous matched flights")
    all_matched_flights <- previous_flights
  }
  
} else {
  message("No previous matched flights - matching all flights")
  # Match all flights (first run)
  flights_sf <- all_flights %>%
    select(flight_id, object_id, takeoff, landing, flight_purpose, geometry) %>%
    distinct(flight_id, .keep_all = TRUE) %>%
    st_transform(32616)
  
  matches <- match_flights_to_cfs(cfs_with_centerlines, flights_sf, 
                                  buffer_distance = 50, time_window_mins = 10)
  
  if (!is.null(matches) && nrow(matches) > 0) {
    flights_sf <- flights_sf %>%
      left_join(matches %>% select(flight_id, incident_type), 
                by = c("object_id" = "flight_id")) %>%
      mutate(flight_purpose = coalesce(incident_type, flight_purpose)) %>%
      select(-incident_type)
  }
  
  all_matched_flights <- st_transform(flights_sf, 4326)
}

### SAVE UPDATED FLIGHT DATA
all_matched_flights <- all_matched_flights %>%
  mutate(
    takeoff = as.numeric(takeoff) * 1000,
    landing = as.numeric(landing) * 1000
  ) %>%
  select(flight_id, takeoff, landing, flight_purpose, geometry)

st_write(all_matched_flights, previous_matched_file, delete_dsn = TRUE)

message("✓ Saved ", nrow(all_matched_flights), " matched flights")
message("✓ Run gen_flight_geojson.R next")