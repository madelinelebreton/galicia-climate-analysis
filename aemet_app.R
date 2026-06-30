# ==============================================================================
# Galicia Monthly AEMET Data Dashboard
# Author: Madeline Lebreton
# Last updated: 2026-06-30
#
# A Shiny app for fetching, filtering, and exporting AEMET monthly climate
# data for Galician municipalities. Designed to run on Posit Cloud.
# ==============================================================================

library(shiny)
library(bslib)
library(climaemet)
library(sf)
library(dplyr)
library(tidygeocoder)
library(purrr)
library(readxl)
library(tidyr)
library(writexl)
library(ggplot2)
library(DT)
library(leaflet) # Added for spatial rendering

# Comprehensive AEMET Monthly/Annual Metadata mapping
AEMET_VARS <- c(
  "Total precipitation (p_mes)"                                      = "p_mes",
  "Maximum daily precipitation (p_max)"                             = "p_max",
  "Average relative humidity (Hr)"                                  = "Hr",
  "Average monthly/yearly average temperature (tm_mes)"            = "tm_mes",
  "Average monthly/yearly maximum temperature (tm_max)"            = "tm_max",
  "Mean monthly/yearly minimum temperature (tm_min)"               = "tm_min",
  "Absolute maximum temperature (ta_max)"                           = "ta_max",
  "Highest minimum temperature (ts_min)"                            = "ts_min",
  "Lowest maximum temperature (ti_max)"                             = "ti_max",
  "No. days max temp ≥ 30°C (nt_30)"                                = "nt_30",
  "No. days min temp ≤ 0°C (nt_00)"                                 = "nt_00",
  "No. days appreciable precipitation ≥ 0.1 mm (np_001)"            = "np_001",
  "No. days precipitation ≥ 10 mm (np_100)"                        = "np_100",
  "No. days precipitation ≥ 30 mm (np_300)"                        = "np_300",
  "Monthly/yearly mean pressure at sea level (q_mar)"               = "q_mar",
  "Monthly/yearly mean pressure at station level (q_med)"           = "q_med",
  "Maximum absolute pressure (q_max)"                               = "q_max",
  "Minimum monthly/yearly maximum pressure (q_min)"                 = "q_min",
  "No. days wind speed ≥ 55 km/h (nw_55)"                           = "nw_55",
  "No. days wind speed ≥ 91 km/h (nw_91)"                           = "nw_91",
  "Direction/speed/date of max gust (w_racha)"                      = "w_racha",
  "Average daily wind speed 07-07 UTC (w_rec)"                      = "w_rec",
  "Monthly mean velocity from 07, 13, 18 UTC (w_med)"               = "w_med",
  "Mean monthly/yearly vapor tension (E)"                           = "E"
)

# ── Helper: fetch specific parameter for one station ─────────────────────────
fetch_station_data <- function(station_id, start_year, end_year, target_var) {
  tryCatch({
    df <- aemet_monthly_period(
      station = station_id,
      start   = start_year,
      end     = end_year
    )
    if (is.null(df) || nrow(df) == 0) return(NULL)
    
    if (!target_var %in% names(df)) {
      message("Variable ", target_var, " not found for station: ", station_id)
      return(NULL)
    }
    
    df %>%
      select(indicativo, fecha, value_col = all_of(target_var)) %>%
      mutate(
        year  = as.integer(substr(as.character(fecha), 1, 4)),
        month = as.integer(substr(as.character(fecha), 6, 7)),
        # Avoid forcing numeric types for mixed string payloads (like w_racha metadata)
        value_col = if(target_var == "w_racha") as.character(value_col) else suppressWarnings(as.numeric(value_col))
      ) %>%
      filter(year >= start_year, year <= end_year) %>%
      select(station_id = indicativo, year, month, value = value_col)
    
  }, error = function(e) {
    message("Failed for station: ", station_id, " — ", conditionMessage(e))
    NULL
  })
}

month_labels <- c("Jan","Feb","Mar","Apr","May","Jun",
                  "Jul","Aug","Sep","Oct","Nov","Dec")

# ══════════════════════════════════════════════════════════════════════════════
# UI
# ══════════════════════════════════════════════════════════════════════════════

ui <- page_navbar(
  title = "AEMET Advanced Data Dashboard",
  theme = bs_theme(bootswatch = "flatly", primary = "#2c6e8a"),
  bg    = "#2c6e8a",
  fillable = FALSE,
  
  # ── Tab 1: Setup ────────────────────────────────────────────────────────────
  nav_panel("① Setup",
            layout_columns(
              col_widths = c(5, 7),
              
              card(
                card_header("Configuration"),
                card_body(
                  p("Work through this app left to right, one tab at a time."),
                  hr(),
                  strong("Step 1 — AEMET API key"),
                  p(tags$small("Your personal key from ",
                               tags$a("opendata.aemet.es", href = "https://opendata.aemet.es/centrodedescargas/inicio", target = "_blank"),
                               ". Required to download data.")),
                  textInput("api_key", NULL, placeholder = "Paste your API key here"),
                  actionButton("btn_auth", "Connect to AEMET", class = "btn-primary w-100"),
                  uiOutput("auth_status"),
                  hr(),
                  strong("Step 2 — Climate Variable Selection"),
                  p(tags$small("Choose any valid dynamic parameter provided by the AEMET Monthly API.")),
                  selectInput("target_variable", NULL, choices = AEMET_VARS, selected = "p_mes"),
                  hr(),
                  strong("Step 3 — Municipality file"),
                  p(tags$small("Upload the Excel file listing your Galician municipalities ",
                               "(must have columns: NOMECAPITA, CONCELLO, PROVINCIA).")),
                  fileInput("muni_file", NULL,
                            accept = c(".xlsx", ".xls"),
                            buttonLabel = "Browse…",
                            placeholder = ".xlsx"),
                  uiOutput("file_status")
                )
              ),
              
              card(
                card_header("How this app works"),
                card_body(
                  tags$ol(
                    tags$li(strong("Setup:"), " Authenticate with AEMET, select your desired weather/climate parameter, and upload your spreadsheet configuration."),
                    tags$li(strong("Stations:"), " The app geocodes your municipalities via OpenStreetMap and matches them spatially to the nearest active tracking station."),
                    tags$li(strong("Data:"), " Define an index timeframe filter, scrape raw structural matrix elements, and visualize the output array mappings."),
                    tags$li(strong("Export:"), " Download structured multi-sheet Excel components containing matrix configurations.")
                  ),
                  hr(),
                  p(tags$small(tags$b("Data source: "), "AEMET Open Data (Agencia Estatal de Meteorología).")),
                  uiOutput("variable_info_note")
                )
              )
            )
  ),
  
  # ── Tab 2: Stations ──────────────────────────────────────────────────────────
  nav_panel("② Stations",
            layout_columns(
              col_widths = c(4, 8),
              
              card(
                card_header("Find nearest stations"),
                card_body(
                  p("Click the button below to geocode your municipalities and match each one to its nearest AEMET station."),
                  p(tags$small("This uses OpenStreetMap and may take 20–40 seconds depending on how many municipalities you have.")),
                  actionButton("btn_match", "Match municipalities to stations",
                               class = "btn-primary w-100"),
                  uiOutput("match_status"),
                  hr(),
                  uiOutput("station_selector_ui")
                )
              ),
              
              # Reorganized to stack the Map view nicely above the table matrix view
              layout_sidebar(
                sidebar = sidebar(
                  title = "Map Settings",
                  p(tags$small("The map displays lines connecting the geocoded Concello center (blue) to its assigned AEMET tracking station (orange).")),
                  open = FALSE
                ),
                card(
                  card_header("Spatial Link Mapping"),
                  card_body(
                    leafletOutput("match_map", height = "380px")
                  )
                ),
                card(
                  card_header("Municipality → Station matches"),
                  card_body(
                    p(tags$small("Sorted by distance. Large distances (> 40 km) may indicate a geocoding issue — check that municipality names are spelled correctly in your Excel file.")),
                    DTOutput("match_table")
                  )
                )
              )
            )
  ),
  
  # ── Tab 3: Data ──────────────────────────────────────────────────────────────
  nav_panel("③ Data",
            layout_columns(
              col_widths = c(3, 9),
              
              card(
                card_header("Fetch options"),
                card_body(
                  p("Select a date range and fetch data for your chosen stations."),
                  sliderInput("year_range", "Year range",
                              min = 1950, max = 2026,
                              value = c(1997, 2026),
                              step = 1, sep = ""),
                  uiOutput("fetch_button_ui"),
                  uiOutput("fetch_status"),
                  hr(),
                  uiOutput("fetch_progress_ui"),
                  p("Be patient - the AEMET API is limited to 50 requests per minute.")
                )
              ),
              
              card(
                card_header(uiOutput("plot_header_ui")),
                card_body(
                  uiOutput("plot_controls_ui"),
                  uiOutput("plot_or_message")
                )
              )
            )
  ),
  
  # ── Tab 4: Export ────────────────────────────────────────────────────────────
  nav_panel("④ Export",
            layout_columns(
              col_widths = c(4, 8),
              
              card(
                card_header("Download data"),
                card_body(
                  p("Download a multi-sheet Excel workbook containing:"),
                  tags$ul(
                    tags$li(tags$b("station_month_matrix"), " — rows = months, columns = stations (wide format for charting)"),
                    tags$li(tags$b("wide"), " — one row per station × year, with Jan–Dec columns and annual summaries"),
                    tags$li(tags$b("long"), " — one row per station × month (tidy format for analysis)")
                  ),
                  hr(),
                  downloadButton("btn_download_excel", "Download Excel workbook",
                                 class = "btn-success w-100"),
                  hr(),
                  downloadButton("btn_download_stations", "Download station matches (CSV)",
                                 class = "btn-outline-secondary w-100")
                )
              ),
              
              card(
                card_header("Data preview — long format"),
                card_body(
                  DTOutput("long_preview")
                )
              )
            )
  )
)

# ══════════════════════════════════════════════════════════════════════════════
# Server
# ══════════════════════════════════════════════════════════════════════════════

server <- function(input, output, session) {
  
  # ── Reactive state ──────────────────────────────────────────────────────────
  rv <- reactiveValues(
    auth_ok       = FALSE,
    stations      = NULL,
    stations_sf   = NULL,
    munis         = NULL,
    match_results = NULL,
    climate_long  = NULL,
    climate_wide  = NULL,
    climate_matrix= NULL,
    fetch_log     = character(0)
  )
  
  # Dynamic lookup helpers for titles and units based on string parsing metadata
  var_label <- reactive({
    names(AEMET_VARS)[AEMET_VARS == input$target_variable]
  })
  
  var_unit <- reactive({
    v <- input$target_variable
    if (v %in% c("p_mes", "p_max")) return("mm")
    if (v %in% c("tm_mes", "tm_max", "tm_min", "ta_max", "ts_min", "ti_max")) return("°C")
    if (v %in% c("Hr")) return("%")
    if (v %in% c("q_mar", "q_med", "q_max", "q_min")) return("hPa")
    if (v %in% c("w_med", "w_rec")) return("km/h")
    if (v %in% c("nt_30", "nt_00", "np_001", "np_100", "np_300", "nw_55", "nw_91")) return("days")
    return("units")
  })
  
  # Is the selected variable numeric or alphanumeric descriptor string?
  is_numeric_var <- reactive({
    input$target_variable != "w_racha"
  })
  
  # ── Dynamic Text Elements ───────────────────────────────────────────────────
  output$variable_info_note <- renderUI({
    tags$p(tags$small(tags$b("Selected Parameter: "), tags$code(input$target_variable), 
                      " = ", var_label(), " (", var_unit(), ")."))
  })
  
  output$fetch_button_ui <- renderUI({
    actionButton("btn_fetch", paste("Fetch", input$target_variable, "data"), class = "btn-primary w-100")
  })
  
  output$plot_header_ui <- renderUI({
    paste("Monthly Matrix Profile:", input$target_variable)
  })
  
  # ── Step 1a: Authenticate ───────────────────────────────────────────────────
  observeEvent(input$btn_auth, {
    req(nchar(trimws(input$api_key)) > 10)
    withProgress(message = "Connecting to AEMET…", {
      tryCatch({
        aemet_api_key(trimws(input$api_key), install = TRUE, overwrite = TRUE)
        stations <- aemet_stations()
        rv$stations <- stations
        rv$stations_sf <- st_as_sf(stations,
                                   coords = c("longitud", "latitud"),
                                   crs = 4326)
        Sys.sleep(0.3)
        rv$auth_ok <- TRUE
      }, error = function(e) {
        rv$auth_ok <- FALSE
        showNotification(paste("AEMET connection failed:", conditionMessage(e)),
                         type = "error", duration = 8)
      })
    })
  })
  
  output$auth_status <- renderUI({
    if (rv$auth_ok) {
      div(class = "alert alert-success mt-2 p-2",
          "✓ Connected — ", nrow(rv$stations), " AEMET stations loaded.")
    } else if (input$btn_auth > 0) {
      div(class = "alert alert-danger mt-2 p-2", "✗ Connection failed. Check your API key.")
    }
  })
  
  # ── Step 1b: Load municipality file ─────────────────────────────────────────
  observeEvent(input$muni_file, {
    tryCatch({
      rv$munis <- read_excel(input$muni_file$datapath)
    }, error = function(e) {
      showNotification(paste("Could not read file:", conditionMessage(e)),
                       type = "error", duration = 8)
    })
  })
  
  output$file_status <- renderUI({
    req(rv$munis)
    div(class = "alert alert-success mt-2 p-2",
        "✓ ", nrow(rv$munis), " municipalities loaded.")
  })
  
  # ── Step 2: Geocode & match ─────────────────────────────────────────────────
  observeEvent(input$btn_match, {
    req(rv$auth_ok, rv$munis, rv$stations_sf)
    
    withProgress(message = "Geocoding municipalities…", value = 0.2, {
      munis <- rv$munis
      munis$search <- paste(munis$NOMECAPITA, munis$CONCELLO,
                            munis$PROVINCIA, "Spain", sep = ", ")
      
      munis_geo <- tryCatch(
        geocode(munis, address = search, method = "osm",
                lat = latitude, long = longitude),
        error = function(e) {
          showNotification(paste("Geocoding failed:", conditionMessage(e)),
                           type = "error", duration = 8)
          NULL
        }
      )
      req(munis_geo)
      
      munis_geo <- munis_geo %>% filter(!is.na(latitude) & !is.na(longitude))
      
      setProgress(0.7, message = "Finding nearest stations…")
      
      munis_sf <- st_as_sf(munis_geo, coords = c("longitude", "latitude"), crs = 4326)
      nearest_idx <- st_nearest_feature(munis_sf, rv$stations_sf)
      
      # Extract structural spatial coordinates from matched targets to link with vectors
      matched_stations <- rv$stations[nearest_idx, ]
      
      results <- munis_geo %>%
        mutate(
          station_id   = matched_stations$indicativo,
          station_name = matched_stations$nombre,
          station_lat  = matched_stations$latitud,
          station_lon  = matched_stations$longitud
        )
      
      results$distance_km <- as.numeric(
        st_distance(munis_sf, rv$stations_sf[nearest_idx, ], by_element = TRUE)
      ) / 1000
      
      rv$match_results <- results
    })
  })
  
  output$match_status <- renderUI({
    req(rv$match_results)
    n_stations <- length(unique(rv$match_results$station_id))
    div(class = "alert alert-success mt-2 p-2",
        "✓ Matched ", nrow(rv$match_results), " municipalities to ",
        n_stations, " unique stations.")
  })
  
  # ── Interactive Geolocation Render Pipeline ────────────────────────────────
  output$match_map <- renderLeaflet({
    # Render static canvas container centered roughly on central Galicia
    leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(lng = -7.9, lat = 42.8, zoom = 8)
  })
  
  observe({
    req(rv$match_results)
    df <- rv$match_results
    
    # Isolate unique target nodes for crisp station overlays
    unique_stations <- df %>% 
      distinct(station_id, station_name, station_lat, station_lon)
    
    proxy <- leafletProxy("match_map") %>% clearMarkers() %>% clearShapes()
    
    # 1. Plot connector paths safely across matrix pairings
    for (i in seq_len(nrow(df))) {
      proxy <- proxy %>%
        addPolylines(
          lng = c(df$longitude[i], df$station_lon[i]),
          lat = c(df$latitude[i], df$station_lat[i]),
          color = "#7f8c8d", weight = 1.5, opacity = 0.7,
          dashArray = "4, 4"
        )
    }
    
    # 2. Add original geocoded municipality pins
    proxy %>%
      addCircleMarkers(
        data = df, lng = ~longitude, lat = ~latitude,
        radius = 4, color = "#2c6e8a", fillColor = "#3498db",
        fillOpacity = 0.8, weight = 1,
        popup = ~paste0("<strong>Concello:</strong> ", CONCELLO, 
                        "<br><strong>Matched to:</strong> ", station_name,
                        "<br><strong>Distance:</strong> ", round(distance_km, 1), " km")
      ) %>%
      # 3. Add distinctive target weather tracker base matrices
      addCircleMarkers(
        data = unique_stations, lng = ~station_lon, lat = ~station_lat,
        radius = 6, color = "#d35400", fillColor = "#e67e22",
        fillOpacity = 0.9, weight = 1.5,
        popup = ~paste0("<strong>AEMET Station:</strong> ", station_name, 
                        "<br><strong>ID:</strong> ", station_id)
      )
  })
  
  output$match_table <- renderDT({
    req(rv$match_results)
    rv$match_results %>%
      select(PROVINCIA, CONCELLO, NOMECAPITA,
             station_id, station_name,
             distance_km) %>%
      mutate(distance_km = round(distance_km, 1)) %>%
      arrange(distance_km) %>%
      datatable(
        rownames = FALSE,
        options  = list(pageLength = 10, scrollX = TRUE),
        colnames = c("Province", "Concello", "Capital",
                     "Station ID", "Station name", "Distance (km)")
      ) %>%
      formatStyle("distance_km",
                  backgroundColor = styleInterval(c(20, 40),
                                                  c("#d4edda", "#fff3cd", "#f8d7da")))
  })
  
  output$station_selector_ui <- renderUI({
    req(rv$match_results)
    unique_stations <- rv$match_results %>%
      distinct(station_id, station_name) %>%
      arrange(station_name)
    choices <- setNames(unique_stations$station_id,
                        paste0(unique_stations$station_name,
                               " (", unique_stations$station_id, ")"))
    tagList(
      strong("Select stations to fetch"),
      p(tags$small("All are selected by default. Deselect any you don't need.")),
      checkboxGroupInput("selected_stations", NULL,
                         choices  = choices,
                         selected = unique_stations$station_id)
    )
  })
  
  # ── Step 3: Fetch Custom Metadata Parameter ─────────────────────────────────
  observeEvent(input$btn_fetch, {
    req(rv$auth_ok, rv$match_results, input$selected_stations)
    
    stations_to_fetch <- input$selected_stations
    n        <- length(stations_to_fetch)
    start_yr <- input$year_range[1]
    end_yr   <- input$year_range[2]
    target_v <- input$target_variable
    rv$fetch_log <- character(0)
    
    withProgress(message = paste("Querying AEMET for", target_v, "…"), value = 0, {
      raw_list <- lapply(seq_along(stations_to_fetch), function(i) {
        sid <- stations_to_fetch[i]
        setProgress(i / n, detail = paste0("Station ", i, " of ", n, ": ", sid))
        Sys.sleep(0.3)
        result <- fetch_station_data(sid, start_yr, end_yr, target_v)
        if (is.null(result)) {
          rv$fetch_log <- c(rv$fetch_log, paste0("⚠ No valid entries returned for ", sid))
        }
        result
      })
      
      all_raw <- bind_rows(raw_list)
      
      if (nrow(all_raw) == 0) {
        showNotification("No records returned. The selected variable might not be tracked at these stations.",
                         type = "error", duration = 10)
        return()
      }
      
      # Long format creation
      all_long <- all_raw %>%
        filter(month != 13) %>%
        left_join(rv$stations %>% select(indicativo, nombre),
                  by = c("station_id" = "indicativo")) %>%
        mutate(
          date   = as.Date(paste0(year, "-", sprintf("%02d", month), "-01")),
          col_id = paste0(nombre, " (", station_id, ")")
        ) %>%
        select(col_id, station_id, nombre, year, month, date, value)
      
      rv$climate_long <- all_long
      
      # Wide format creation
      wide_base <- all_long %>%
        mutate(month_label = month_labels[month]) %>%
        pivot_wider(
          id_cols     = c(station_id, nombre, year),
          names_from  = month_label,
          values_from = value,
          names_sort  = FALSE
        )
      
      # Append aggregations safely only if dealing with purely numerical profiles
      if (is_numeric_var()) {
        wide_base <- wide_base %>%
          mutate(
            yearly_max  = apply(across(all_of(month_labels)), 1, function(x) if(all(is.na(x))) NA else max(x, na.rm=TRUE)),
            yearly_min  = apply(across(all_of(month_labels)), 1, function(x) if(all(is.na(x))) NA else min(x, na.rm=TRUE)),
            yearly_mean = apply(across(all_of(month_labels)), 1, function(x) if(all(is.na(x))) NA else mean(x, na.rm=TRUE)),
            yearly_sum  = apply(across(all_of(month_labels)), 1, function(x) if(all(is.na(x))) NA else sum(x, na.rm=TRUE))
          ) %>%
          select(station_id, nombre, year, all_of(month_labels),
                 yearly_max, yearly_min, yearly_mean, yearly_sum)
      } else {
        # String parameter type logic override (e.g. w_racha direction arrays)
        wide_base <- wide_base %>%
          select(station_id, nombre, year, any_of(month_labels))
      }
      
      rv$climate_wide <- wide_base %>% arrange(station_id, year)
      
      # Station Matrix Generation
      matrix_raw <- all_long %>%
        mutate(station_col = col_id) %>%
        pivot_wider(
          id_cols     = date,
          names_from  = station_col,
          values_from = value
        )
      
      all_months <- data.frame(
        date = seq(as.Date(paste0(start_yr, "-01-01")),
                   as.Date(paste0(end_yr,   "-12-01")),
                   by = "month")
      )
      
      rv$climate_matrix <- all_months %>%
        left_join(matrix_raw, by = "date") %>%
        mutate(Year  = format(date, "%Y"),
               Month = format(date, "%m")) %>%
        relocate(Year, Month, date)
    })
  })
  
  output$fetch_status <- renderUI({
    req(rv$climate_long)
    n_rows <- nrow(rv$climate_long)
    n_sta  = length(unique(rv$climate_long$station_id))
    msgs   = rv$fetch_log
    tagList(
      div(class = "alert alert-success mt-2 p-2",
          "✓ Fetched ", format(n_rows, big.mark = ","),
          " monthly instances across ", n_sta, " stations."),
      if (length(msgs) > 0)
        div(class = "alert alert-warning mt-1 p-2",
            tags$b("Station warnings encountered:"), br(),
            HTML(paste(msgs, collapse = "<br>")))
    )
  })
  
  output$plot_controls_ui <- renderUI({
    req(rv$climate_long)
    station_choices <- sort(unique(rv$climate_long$col_id))
    tagList(
      fluidRow(
        column(8,
               selectInput("plot_stations", "Show stations",
                           choices  = station_choices,
                           selected = station_choices,
                           multiple = TRUE,
                           width    = "100%")
        ),
        column(4,
               selectInput("plot_ncol", "Columns",
                           choices  = c("1", "2", "3", "4"),
                           selected = "3",
                           width    = "100%")
        )
      )
    )
  })
  
  # Handle whether to display the time-series line plot or a text note if variable is a composite string
  output$plot_or_message <- renderUI({
    if(is_numeric_var()) {
      plotOutput("climate_plot", height = "600px")
    } else {
      div(class = "alert alert-info mt-4", 
          "Plot rendering is disabled for alphanumeric composite strings (e.g., w_racha wind gust details). Please check the Data Preview or Export tabs for parsing full text parameters.")
    }
  })
  
  output$climate_plot <- renderPlot({
    req(rv$climate_long, input$plot_stations, is_numeric_var())
    
    plot_data <- rv$climate_long %>%
      filter(col_id %in% input$plot_stations)
    
    if (nrow(plot_data) == 0) return(NULL)
    
    ggplot(plot_data, aes(x = date, y = value, group = col_id)) +
      geom_line(color = "#2c6e8a", linewidth = 0.45, alpha = 0.85) +
      facet_wrap(~ col_id, ncol = as.integer(input$plot_ncol)) +
      theme_minimal(base_size = 11) +
      theme(
        strip.text       = element_text(size = 8, face = "bold"),
        panel.grid.minor = element_blank(),
        plot.title       = element_text(face = "bold")
      ) +
      labs(
        title = paste("Timeline analysis:", var_label()),
        x     = NULL,
        y     = paste0(input$target_variable, " (", var_unit(), ")")
      )
  })
  
  # ── Step 4: Export ───────────────────────────────────────────────────────────
  output$long_preview <- renderDT({
    req(rv$climate_long)
    
    col_name_dynamic <- paste0(var_label(), " (", var_unit(), ")")
    
    formatted_df <- rv$climate_long %>%
      select(col_id, year, month, date, value)
    
    if (is_numeric_var()) {
      formatted_df <- formatted_df %>% mutate(value = round(as.numeric(value), 1))
    }
    
    formatted_df %>%
      datatable(
        rownames = FALSE,
        options  = list(pageLength = 12, scrollX = TRUE),
        colnames = c("Station ID mapping", "Year", "Month", "Date", col_name_dynamic)
      )
  })
  
  output$btn_download_excel <- downloadHandler(
    filename = function() {
      paste0("galicia_", input$target_variable, "_",
             input$year_range[1], "_", input$year_range[2], ".xlsx")
    },
    content = function(file) {
      req(rv$climate_matrix, rv$climate_wide, rv$climate_long)
      write_xlsx(
        list(
          station_month_matrix = rv$climate_matrix,
          wide                 = rv$climate_wide,
          long                 = rv$climate_long %>%
            select(col_id, year, month, date, value)
        ),
        path = file
      )
    }
  )
  
  output$btn_download_stations <- downloadHandler(
    filename = function() "municipios_aemet_stations.csv",
    content  = function(file) {
      req(rv$match_results)
      readr::write_excel_csv(
        rv$match_results %>%
          select(PROVINCIA, CONCELLO, NOMECAPITA,
                 station_id, station_name, distance_km),
        file
      )
    }
  )
}

shinyApp(ui, server)