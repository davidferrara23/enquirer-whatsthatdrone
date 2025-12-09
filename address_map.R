### ADDRESS SEARCH SHINY APP

library(shiny)
library(shinydashboard)
library(sf)
library(tidyverse)
library(leaflet)
library(tidygeocoder)
library(lubridate)

# Load flight data (assumes this is already loaded from your main script)
if (!exists("flights_sf")) {
    flights_sf <- st_read("./data/flight_paths.geojson")
    flights_sf$takeoff <- as.POSIXct(flights_sf$takeoff / 1000, origin = "1970-01-01", tz = "America/New_York")
    flights_sf$landing <- as.POSIXct(flights_sf$landing / 1000, origin = "1970-01-01", tz = "America/New_York")
    flights_sf <- st_transform(flights_sf, 32616)
    
    # Remove duplicate flight IDs, keeping the first occurrence
    flights_sf <- flights_sf %>% distinct(flight_id, .keep_all = TRUE)
}

# UI
ui <- dashboardPage(
  dashboardHeader(title = "What's that drone? - Address Search"),
  
  dashboardSidebar(
    h4("Search Parameters"),
    textInput("address", "Enter Address:", 
              placeholder = "e.g., 801 Plum St, Cincinnati, OH"),
    
    sliderInput("buffer_distance", "Search Radius (meters):", 
                min = 10, max = 100, value = 25, step = 5),
    
    dateRangeInput("date_range", "Date Range:",
                   start = min(as.Date(flights_sf$takeoff)), 
                   end = max(as.Date(flights_sf$takeoff)),
                   min = min(as.Date(flights_sf$takeoff)),
                   max = max(as.Date(flights_sf$takeoff))),
    
    actionButton("search_btn", "Search", class = "btn-primary", 
                 style = "width: 100%; margin-top: 10px;"),
    
    hr(),
    
    h5("Search Results:"),
    textOutput("total_flights"),
    textOutput("closest_flight"),
    textOutput("geocode_status"),
    
    hr(),
    
    downloadButton("download_data", "Download Results", 
                   style = "width: 100%;")
  ),
  
  dashboardBody(
    fluidRow(
      box(
        title = "Flight Paths Near Address", 
        status = "primary", 
        solidHeader = TRUE,
        width = 12, 
        height = "600px",
        leafletOutput("address_map", height = "550px")
      )
    ),
    
    fluidRow(
      box(
        title = "Flight Timeline", 
        status = "info", 
        solidHeader = TRUE,
        width = 12,
        plotOutput("timeline_plot", height = "200px")
      )
    ),
    
    fluidRow(
      box(
        title = "Flight Details", 
        status = "warning", 
        solidHeader = TRUE,
        width = 12,
        DT::dataTableOutput("flight_table")
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # Reactive values
  values <- reactiveValues(
    search_point = NULL,
    nearby_flights = NULL,
    geocode_result = NULL
  )
  
  # Geocode address when search button is clicked
  observeEvent(input$search_btn, {
    req(input$address)
    
    # Show notification
    showNotification("Geocoding address...", type = "message", duration = 2)
    
    # Geocode the address
    tryCatch({
      geocode_result <- tidygeocoder::geo(
        address = input$address,
        method = "osm",  # Use OpenStreetMap (free)
        full_results = TRUE
      )
      
      if (nrow(geocode_result) > 0 && !is.na(geocode_result$lat[1])) {
        # Create point
        search_point <- st_as_sf(
          data.frame(
            lon = geocode_result$long[1],
            lat = geocode_result$lat[1]
          ),
          coords = c("lon", "lat"),
          crs = 4326
        )
        
        # Transform to match flights CRS
        search_point <- st_transform(search_point, st_crs(flights_sf))
        
        values$search_point <- search_point
        values$geocode_result <- geocode_result
        
        # Find nearby flights
        search_buffer <- st_buffer(search_point, input$buffer_distance)
        
        # Filter by date range
        flights_in_range <- flights_sf %>%
          filter(
            as.Date(takeoff) >= input$date_range[1],
            as.Date(takeoff) <= input$date_range[2]
          )
        
        # Find intersecting flights
        intersects <- st_intersects(flights_in_range, search_buffer, sparse = FALSE)
        nearby_flights <- flights_in_range[intersects[,1], ]
        
        # Calculate distances
        if (nrow(nearby_flights) > 0) {
          nearby_flights <- nearby_flights %>%
            mutate(
              distance_m = as.numeric(st_distance(geometry, search_point)),
              flight_date = as.Date(takeoff),
              duration_mins = as.numeric(difftime(landing, takeoff, units = "mins"))
            ) %>%
            arrange(distance_m)
          
          values$nearby_flights <- nearby_flights
          
          showNotification(
            paste("Found", nrow(nearby_flights), "flights within", input$buffer_distance, "meters"),
            type = "message",
            duration = 5
          )
        } else {
          values$nearby_flights <- NULL
          showNotification(
            "No flights found within search radius",
            type = "warning",
            duration = 5
          )
        }
      } else {
        showNotification("Could not geocode address. Please try again.", 
                        type = "error", duration = 5)
        values$search_point <- NULL
        values$nearby_flights <- NULL
      }
    }, error = function(e) {
      showNotification(
        paste("Geocoding error:", e$message),
        type = "error",
        duration = 5
      )
      values$search_point <- NULL
      values$nearby_flights <- NULL
    })
  })
  
  # Status outputs
  output$total_flights <- renderText({
    if (is.null(values$nearby_flights)) {
      "No flights found"
    } else {
      paste("Total Flights:", nrow(values$nearby_flights))
    }
  })
  
  output$closest_flight <- renderText({
    if (is.null(values$nearby_flights) || nrow(values$nearby_flights) == 0) {
      ""
    } else {
      paste("Closest Flight:", round(values$nearby_flights$distance_m[1], 1), "meters")
    }
  })
  
  output$geocode_status <- renderText({
    if (!is.null(values$geocode_result)) {
      paste0("Coordinates: ", 
             round(values$geocode_result$lat[1], 5), ", ",
             round(values$geocode_result$long[1], 5))
    } else {
      ""
    }
  })
  
  # Map output
  output$address_map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(lng = -84.5, lat = 39.1, zoom = 12)
  })
  
  # Update map when search is performed
  observe({
    req(values$search_point)
    
    # Transform to lat/lon for leaflet
    search_point_ll <- st_transform(values$search_point, 4326)
    search_coords <- st_coordinates(search_point_ll)
    
    # Create buffer for display - FIX: transform the original point in UTM, buffer, then transform to WGS84
    search_buffer_ll <- st_buffer(values$search_point, input$buffer_distance) %>% 
      st_transform(4326)
    
    # Start with base map
    map <- leafletProxy("address_map") %>%
      clearShapes() %>%
      clearMarkers() %>%
      addCircleMarkers(
        lng = search_coords[1],
        lat = search_coords[2],
        radius = 8,
        color = "red",
        fillColor = "red",
        fillOpacity = 0.8,
        popup = paste0("<b>Search Address:</b><br>", input$address)
      ) %>%
      addPolygons(  # FIX: Use addPolygons instead of addCircles for the buffer
        data = search_buffer_ll,
        color = "red",
        fillColor = "red",
        fillOpacity = 0.1,
        weight = 2,
        popup = paste("Search radius:", input$buffer_distance, "meters")
      ) %>%
      setView(lng = search_coords[1], lat = search_coords[2], zoom = 16)
    
    # Add flights if any found
    if (!is.null(values$nearby_flights) && nrow(values$nearby_flights) > 0) {
      flights_ll <- st_transform(values$nearby_flights, 4326)
      
      map <- map %>%
        addPolylines(
          data = flights_ll,
          color = ~colorNumeric("YlOrRd", distance_m)(distance_m),
          weight = 3,
          opacity = 0.7,
          popup = ~paste0(
            "<b>Flight ID:</b> ", flight_id, "<br>",
            "<b>Date:</b> ", format(takeoff, "%Y-%m-%d"), "<br>",
            "<b>Time:</b> ", format(takeoff, "%H:%M"), " - ", format(landing, "%H:%M"), "<br>",
            "<b>Distance:</b> ", round(distance_m, 1), " meters<br>",
            "<b>Duration:</b> ", round(duration_mins, 1), " minutes"
          )
        ) %>%
        addLegend(
          "bottomright",
          pal = colorNumeric("YlOrRd", values$nearby_flights$distance_m),
          values = values$nearby_flights$distance_m,
          title = "Distance (m)",
          opacity = 0.7
        )
    }
    
    map
  })
  
  # Timeline plot
  output$timeline_plot <- renderPlot({
    if (is.null(values$nearby_flights) || nrow(values$nearby_flights) == 0) {
      ggplot() + 
        theme_void() + 
        labs(title = "No flights to display")
    } else {
      ggplot(values$nearby_flights, aes(x = flight_date)) +
        geom_histogram(binwidth = 1, fill = "steelblue", alpha = 0.7) +
        theme_minimal() +
        labs(
          title = "Flights by Date",
          x = "Date",
          y = "Number of Flights"
        ) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    }
  })
  
  # Flight details table
  output$flight_table <- DT::renderDataTable({
    if (is.null(values$nearby_flights) || nrow(values$nearby_flights) == 0) {
      data.frame(Message = "No flights found")
    } else {
      values$nearby_flights %>%
        st_drop_geometry() %>%
        select(
          `Flight ID` = flight_id,
          Date = flight_date,
          `Takeoff Time` = takeoff,
          `Landing Time` = landing,
          `Duration (min)` = duration_mins,
          `Distance (m)` = distance_m,
          `Purpose` = flight_purpose
        ) %>%
        mutate(
          `Takeoff Time` = format(`Takeoff Time`, "%H:%M:%S"),
          `Landing Time` = format(`Landing Time`, "%H:%M:%S"),
          `Duration (min)` = round(`Duration (min)`, 1),
          `Distance (m)` = round(`Distance (m)`, 1)
        )
    }
  }, options = list(pageLength = 10, scrollX = TRUE))
  
  # Download handler
  output$download_data <- downloadHandler(
    filename = function() {
      paste0("drone_flights_", gsub("[^0-9]", "", input$address), "_", 
             Sys.Date(), ".csv")
    },
    content = function(file) {
      if (!is.null(values$nearby_flights) && nrow(values$nearby_flights) > 0) {
        values$nearby_flights %>%
          st_drop_geometry() %>%
          select(
            flight_id, takeoff, landing, flight_date,
            duration_mins, distance_m, flight_purpose
          ) %>%
          write_csv(file)
      }
    }
  )
}

# Run app
shinyApp(ui = ui, server = server)