# ==============================================================================
# Galicia Monthly Precipitation Dashboard
# Author: Madeline Lebreton
# Last updated: 2026-06-22
#
# A Shiny app for fetching, filtering, and exporting AEMET monthly precipitation
# data for Galician municipalities. Designed to run on Posit Cloud.
#
# BEFORE RUNNING:
#   1. Upload your municipality Excel file (temp_precip_data.xlsx) to this project
#   2. Paste your AEMET API key in the box on the Setup tab
#   3. Work through the tabs left to right: Setup → Stations → Data → Export
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

# ── Helper: fetch precipitation for one station ────────────────────────────────
fetch_station_precip <- function(station_id, start_year, end_year) {
  tryCatch({
    df <- aemet_monthly_period(
      station = station_id,
      start   = start_year,
      end     = end_year
    )
    if (is.null(df) || nrow(df) == 0) return(NULL)

    df %>%
      select(indicativo, fecha, p_mes) %>%
      mutate(
        year  = as.integer(substr(as.character(fecha), 1, 4)),
        month = as.integer(substr(as.character(fecha), 6, 7)),
        p_mes = suppressWarnings(as.numeric(p_mes))
      ) %>%
      filter(year >= start_year, year <= end_year) %>%
      select(station_id = indicativo, year, month, precip_mm = p_mes)

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
  title = "Galicia Precipitation",
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
          strong("Step 2 — Municipality file"),
          p(tags$small("Upload the Excel file listing your Galician municipalities ",
            "(must have columns: NOMECAPITA, CONCELLO, PROVINCIA).")),
          fileInput("muni_file", NULL,
                    accept = c(".xlsx", ".xls"),
                    buttonLabel = "Browse…",
                    placeholder = "temp_precip_data.xlsx"),
          uiOutput("file_status")
        )
      ),

      card(
        card_header("How this app works"),
        card_body(
          tags$ol(
            tags$li(strong("Setup:"), " Authenticate with AEMET and upload your municipality list."),
            tags$li(strong("Stations:"), " The app geocodes your municipalities and finds the nearest AEMET weather station to each one. Review the matches and select which stations to fetch data for."),
            tags$li(strong("Data:"), " Choose a date range, fetch precipitation data, and explore the results as a S5sZWJyZXRvbkBnbWFpbC5jb20iLCJqdGkiOiJmZTg2ZmNjZC1hNTUwLTQzZmItOTc2ZC1mODk0MzA2NjIzYWYiLCJpc3MiOiJBRU1FVCIsImlhdCI6MTc4MjcyMDIyNywidXNlcklkIjoiZmU4NmZjY2QtYTU1MC00M2ZiLTk3NmQtZjg5NDMwNjYyM2FmIiwicm9sZSI6IiJ9.3BYn_SM55wmwxBw_yJ_TTK-Otime-series plot."),
            tags$li(strong("Export:"), " Download a multi-sheet Excel workbook with all three data formats.")
          ),
          hr(),
          p(tags$small(tags$b("Data source: "), "AEMET Open Data (Agencia Estatal de Meteorología). ",
            "Variable ", tags$code("p_mes"), " = total monthly precipitation in millimetres."))
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

      card(
        card_header("Municipality → Station matches"),
        card_body(
          p(tags$small("Sorted by distance. Large distances (> 40 km) may indicate a geocoding issue — check that municipality names are spelled correctly in your Excel file.")),
          DTOutput("match_table")
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
          p("Select a date range and fetch precipitation data for your chosen stations."),
          sliderInput("year_range", "Year range",
                      min = 1950, max = 2026,
                      value = c(1997, 2026),
                      step = 1, sep = ""),
          actionButton("btn_fetch", "Fetch precipitation data",
                       class = "btn-primary w-100"),
          uiOutput("fetch_status"),
          hr(),
          uiOutput("fetch_progress_ui")
        )
      ),

      card(
        card_header("Monthly precipitation by station"),
        card_body(
          uiOutput("plot_controls_ui"),
          plotOutput("precip_plot", height = "600px")
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
    auth_ok        = FALSE,
    stations       = NULL,
    stations_sf    = NULL,
    munis          = NULL,
    match_results  = NULL,
    precip_long    = NULL,
    precip_wide    = NULL,
    precip_matrix  = NULL,
    fetch_log      = character(0)
  )

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

      # Drop rows that failed to geocode
      munis_geo <- munis_geo %>% filter(!is.na(latitude) & !is.na(longitude))

      setProgress(0.7, message = "Finding nearest stations…")

      munis_sf <- st_as_sf(munis_geo,
                            coords = c("longitude", "latitude"),
                            crs = 4326)

      nearest_idx <- st_nearest_feature(munis_sf, rv$stations_sf)

      results <- munis_geo %>%
        mutate(
          station_id   = rv$stations$indicativo[nearest_idx],
          station_name = rv$stations$nombre[nearest_idx]
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
        options  = list(pageLength = 15, scrollX = TRUE),
        colnames = c("Province", "Concello", "Capital",
                     "Station ID", "Station name", "Distance (km)")
      ) %>%
      formatStyle("distance_km",
                  backgroundColor = styleInterval(c(20, 40),
                    c("#d4edda", "#fff3cd", "#f8d7da")))
  })

  # Station selector (shown after matching)
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

  # ── Step 3: Fetch precipitation data ────────────────────────────────────────
  observeEvent(input$btn_fetch, {
    req(rv$auth_ok, rv$match_results, input$selected_stations)

    stations_to_fetch <- input$selected_stations
    n <- length(stations_to_fetch)
    start_yr <- input$year_range[1]
    end_yr   <- input$year_range[2]
    rv$fetch_log <- character(0)

    withProgress(message = "Fetching precipitation data…", value = 0, {
      raw_list <- lapply(seq_along(stations_to_fetch), function(i) {
        sid <- stations_to_fetch[i]
        setProgress(i / n,
          detail = paste0("Station ", i, " of ", n, ": ", sid))
        Sys.sleep(0.3)
        result <- fetch_station_precip(sid, start_yr, end_yr)
        if (is.null(result)) {
          rv$fetch_log <- c(rv$fetch_log, paste0("⚠ No data returned for ", sid))
        }
        result
      })

      all_raw <- bind_rows(raw_list)

      if (nrow(all_raw) == 0) {
        showNotification("No data was returned. Check your API key and station selection.",
                         type = "error", duration = 10)
        return()
      }

      # Long format
      all_long <- all_raw %>%
        filter(month != 13) %>%
        left_join(rv$stations %>% select(indicativo, nombre),
                  by = c("station_id" = "indicativo")) %>%
        mutate(
          date   = as.Date(paste0(year, "-", sprintf("%02d", month), "-01")),
          col_id = paste0(nombre, " (", station_id, ")")
        ) %>%
        select(col_id, station_id, nombre, year, month, date, precip_mm)

      rv$precip_long <- all_long

      # Wide format
      rv$precip_wide <- all_long %>%
        mutate(month_label = month_labels[month]) %>%
        pivot_wider(
          id_cols     = c(station_id, nombre, year),
          names_from  = month_label,
          values_from = precip_mm,
          names_sort  = FALSE
        ) %>%
        mutate(
          yearly_max  = apply(across(all_of(month_labels)), 1, max,  na.rm = TRUE),
          yearly_min  = apply(across(all_of(month_labels)), 1, min,  na.rm = TRUE),
          yearly_mean = apply(across(all_of(month_labels)), 1, mean, na.rm = TRUE),
          yearly_sum  = apply(across(all_of(month_labels)), 1, sum,  na.rm = TRUE)
        ) %>%
        select(station_id, nombre, year, all_of(month_labels),
               yearly_max, yearly_min, yearly_mean, yearly_sum) %>%
        arrange(station_id, year)

      # Station × month matrix
      matrix_raw <- all_long %>%
        mutate(station_col = col_id) %>%
        pivot_wider(
          id_cols     = date,
          names_from  = station_col,
          values_from = precip_mm
        )

      all_months <- data.frame(
        date = seq(as.Date(paste0(start_yr, "-01-01")),
                   as.Date(paste0(end_yr,   "-12-01")),
                   by = "month")
      )

      rv$precip_matrix <- all_months %>%
        left_join(matrix_raw, by = "date") %>%
        mutate(Year  = format(date, "%Y"),
               Month = format(date, "%m")) %>%
        relocate(Year, Month, date)
    })
  })

  output$fetch_status <- renderUI({
    req(rv$precip_long)
    n_rows <- nrow(rv$precip_long)
    n_sta  <- length(unique(rv$precip_long$station_id))
    msgs   <- rv$fetch_log
    tagList(
      div(class = "alert alert-success mt-2 p-2",
          "✓ Fetched ", format(n_rows, big.mark = ","),
          " monthly records across ", n_sta, " stations."),
      if (length(msgs) > 0)
        div(class = "alert alert-warning mt-1 p-2",
            tags$b("Warnings:"), br(),
            HTML(paste(msgs, collapse = "<br>")))
    )
  })

  # Plot controls (shown after fetch)
  output$plot_controls_ui <- renderUI({
    req(rv$precip_long)
    station_choices <- sort(unique(rv$precip_long$col_id))
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

  output$precip_plot <- renderPlot({
    req(rv$precip_long, input$plot_stations)

    plot_data <- rv$precip_long %>%
      filter(col_id %in% input$plot_stations)

    if (nrow(plot_data) == 0) return(NULL)

    ggplot(plot_data, aes(x = date, y = precip_mm, group = col_id)) +
      geom_line(color = "#2c6e8a", linewidth = 0.45, alpha = 0.85) +
      facet_wrap(~ col_id, ncol = as.integer(input$plot_ncol)) +
      theme_minimal(base_size = 11) +
      theme(
        strip.text       = element_text(size = 8, face = "bold"),
        panel.grid.minor = element_blank(),
        plot.title       = element_text(face = "bold")
      ) +
      labs(
        title = "Monthly Precipitation by Station",
        x     = NULL,
        y     = "Precipitation (mm)"
      )
  })

  # ── Step 4: Export ───────────────────────────────────────────────────────────
  output$long_preview <- renderDT({
    req(rv$precip_long)
    rv$precip_long %>%
      select(col_id, year, month, date, precip_mm) %>%
      mutate(precip_mm = round(precip_mm, 1)) %>%
      datatable(
        rownames = FALSE,
        options  = list(pageLength = 12, scrollX = TRUE),
        colnames = c("Station", "Year", "Month", "Date", "Precip (mm)")
      )
  })

  output$btn_download_excel <- downloadHandler(
    filename = function() {
      paste0("galicia_precip_",
             input$year_range[1], "_", input$year_range[2], ".xlsx")
    },
    content = function(file) {
      req(rv$precip_matrix, rv$precip_wide, rv$precip_long)
      write_xlsx(
        list(
          station_month_matrix = rv$precip_matrix,
          wide                 = rv$precip_wide,
          long                 = rv$precip_long %>%
                                   select(col_id, year, month, date, precip_mm)
        ),
        path = file
      )
    }
  )

  output$btn_download_stations <- downloadHandler(
    filename = function() "municipios_aemet_stations.csv",
    content  = function(file) {
      req(rv$match_results)
      
      # FIX: Use readr::write_excel_csv to cleanly force Excel-compatible UTF-8
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
