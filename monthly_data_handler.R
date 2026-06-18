rm(list = ls()) # clear environment
gc() # clear memory
# ================================
# 0. SETUP
# ================================
library(climaemet)
library(dplyr)
library(lubridate)
library(purrr)


# API key set:
aemet_api_key("eyJhbGciOiJIUzI1NiJ9.eyJzdWIiOiJtYWRlbGluZS5sZWJyZXRvbkBnbWFpbC5jb20iLCJqdGkiOiJiMWEzODZjMC03ODc3LTQ4ZDktYmI5ZS05MWU1NDljYzQwNTYiLCJpc3MiOiJBRU1FVCIsImlhdCI6MTc3OTk2MjgwMCwidXNlcklkIjoiYjFhMzg2YzAtNzg3Ny00OGQ5LWJiOWUtOTFlNTQ5Y2M0MDU2Iiwicm9sZSI6IiJ9.dQrIldbr6UOQraZpyDnZR9p2obfU9GRHzuRCCBRs2B0")

dir.create("outputs/spreadsheets", recursive = TRUE, showWarnings = FALSE)

# ================================
# 1. PARAMETERS
# ================================
end_year <- year(Sys.Date()) # 2026
start_year <- end_year - 100
batch_size <- 100 # AEMET limits data per call, so we batch by decade

# Galicia station IDs 
# can also dynamically filter via aemet_stations()
galicia_stations <- c(
  "1505", # LUGO
  "1475X", # SANTIAGO
  "1387", # A CORUÑA
  "1690A", # ORENSE
  "1484C" # PONTEVEDRA
)

# ================================
# 2. GET STATION METADATA 
# ================================
stations_meta <- aemet_stations() # metadata for all stations in Spain

galicia_meta <- stations_meta %>%
  filter(indicativo %in% galicia_stations) # keep only desired stations

# ================================
# 3. DOWNLOAD MONTHLY DATA (100 YEARS)
# ================================
# aemet_monthly_period returns one station per call, need to loop

# for each station ID, download data and combine into one dataframe


# -----------------------------
# SAFE API FUNCTION
# -----------------------------
year_seq <- seq(start_year, end_year, by = batch_size)

safe_fetch <- function(st, y_start, y_end) {
  
  message("Station: ", st, " | ", y_start, "-", y_end)
  
  Sys.sleep(1.5)
  
  tryCatch({
    
    res <- aemet_monthly_period(
      station = st,
      start = y_start,
      end = y_end,
      verbose = FALSE
    )
    
    if (is.null(res) || nrow(res) == 0) return(NULL)
    
    res %>%
      mutate(
        station = st,
        start = y_start,
        end = y_end
      )
    
  }, error = function(e) {
    message("Failed station ", st, " | ", y_start, "-", y_end, ": ", e$message)
    return(NULL)
  })
}

# -----------------------------
# BATCHED LOOP
# -----------------------------
monthly_raw <- map_dfr(galicia_stations, function(st) {
  
  map_dfr(year_seq, function(y_start) {
    
    y_end <- min(y_start + batch_size - 1, end_year)
    
    safe_fetch(st, y_start, y_end)
    
  })
})

# ================================
# 4. CLEAN + STANDARDISE
# ================================
monthly_clean <- monthly_raw %>%
  
  # Ensure date parsing works (AEMET uses YYYY-MM format in many outputs)
  mutate(
    fecha = as.character(fecha),
    year = as.integer(substr(fecha, 1, 4)),
    month = as.integer(substr(fecha, 6, 7))
  ) %>%
  
  # Keep within range (AEMET may return partial early years)
  filter(year >= start_year, year <= end_year)

# ================================
# 5. SELECT CORE VARIABLES
# ================================
# Depending on API response, common fields include:
# prec (precipitation), tmed, tmax, tmin

monthly_final <- monthly_clean %>%
  select(
    station,
    year,
    month,
    everything()
  )

# ================================
# 6. SAVE TO CSV
# ================================
write.csv(
  monthly_final,
  "outputs/spreadsheets/galicia_monthly_100yr.csv",
  row.names = FALSE
)

# ================================
# 7. OPTIONAL: QUICK CHECK SUMMARY
# ================================
summary_check <- monthly_final %>%
  group_by(station, year) %>%
  summarise(
    mean_precip = mean(prec, na.rm = TRUE),
    total_precip = sum(prec, na.rm = TRUE),
    .groups = "drop"
  )

write.csv(
  summary_check,
  "outputs/spreadsheets/galicia_monthly_summary_check.csv",
  row.names = FALSE
)