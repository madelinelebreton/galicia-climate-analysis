# Get AEMET monthly precipitation data for Galicia municipalities
# author: Madeline Lebreton
# date: 6/22/2026

rm(list = ls())
gc()

library(climaemet)
library(sf)
library(dplyr)
library(tidygeocoder)
library(purrr)
library(readxl)
library(tidyr)
library(writexl)
aemet_api_key("eyJhbGciOiJIUzI1NiJ9.eyJzdWIiOiJtYWRlbGluZS5sZWJyZXRvbkBnbWFpbC5jb20iLCJqdGkiOiJiMWEzODZjMC03ODc3LTQ4ZDktYmI5ZS05MWU1NDljYzQwNTYiLCJpc3MiOiJBRU1FVCIsImlhdCI6MTc3OTk2MjgwMCwidXNlcklkIjoiYjFhMzg2YzAtNzg3Ny00OGQ5LWJiOWUtOTFlNTQ5Y2M0MDU2Iiwicm9sZSI6IiJ9.dQrIldbr6UOQraZpyDnZR9p2obfU9GRHzuRCCBRs2B0")

#------------------------------------
# Load AEMET stations
#------------------------------------
stations <- aemet_stations()

stations_sf <- st_as_sf(
  stations,
  coords = c("longitud", "latitud"),
  crs = 4326
)

#------------------------------------
# Read municipality list
#------------------------------------
munis <- read_excel(
  "C:/Users/Equipo/Documents/ss_ML_local/galicia-climate-analysis/temp_precip_data.xlsx"
)


#------------------------------------
# Create search string
#------------------------------------
munis$search <- paste(
  munis$NOMECAPITA,
  munis$CONCELLO,
  munis$PROVINCIA,
  "Spain",
  sep = ", "
)

#------------------------------------
# Geocode localities
#------------------------------------
munis_geo <- munis %>%
  geocode(
    address = search,
    method = "osm",
    lat = latitude,
    long = longitude
  )

# Convert to sf
munis_sf <- st_as_sf(
  munis_geo,
  coords = c("longitude", "latitude"),
  crs = 4326
)

#------------------------------------
# Find nearest station
#------------------------------------
nearest_idx <- st_nearest_feature(
  munis_sf,
  stations_sf
)

results <- munis_geo %>%
  mutate(
    station_id = stations$indicativo[nearest_idx],
    station_name = stations$nombre[nearest_idx]
  )

# Calculate distance
results$distance_km <- as.numeric(
  st_distance(
    munis_sf,
    stations_sf[nearest_idx, ],
    by_element = TRUE
  )
) / 1000

#------------------------------------
# Output
#------------------------------------
results %>%
  select(
    PROVINCIA,
    CONCELLO,
    NOMECAPITA,
    station_id,
    station_name,
    distance_km
  ) %>%
  arrange(distance_km)

write.csv(
  results,
  "C:\\Users\\Equipo\\Documents\\ss_ML_local\\galicia-climate-analysis\\outputs\\municipios_aemet_stations.csv",
  row.names = FALSE
)

unique_stations <- unique(results$station_id)
print(unique_stations)

unique_stations_df <- stations %>%
  filter(indicativo %in% unique_stations)
print(unique_stations_df, n=Inf)

write.table(
  unique_stations_df,
  "clipboard",
  sep = "\t",
  row.names = FALSE,
  quote = FALSE
)

#------------------------------------
# Fetch monthly precipitation (mm) for each station, 1997-2026
#------------------------------------

fetch_station_precip <- function(station_id, start_year = 1997, end_year = 2026) {
  
  tryCatch({
    df <- aemet_monthly_period(
      station = station_id,
      start = start_year,
      end = end_year
    )
    if (is.null(df) || nrow(df) == 0) return(NULL)
    
    df %>%
      select(indicativo, fecha, p_mes) %>%   # p_mes = total monthly precip (mm)
      mutate(
        year  = as.integer(substr(as.character(fecha), 1, 4)), # get year as substring of date
        month = as.integer(substr(as.character(fecha), 6, 7)), # get month as substring of date
        p_mes = suppressWarnings(as.numeric(p_mes))
      ) %>%
      filter(year >= start_year, year <= end_year) %>%
      select(station_id = indicativo, year, month, precip_mm = p_mes)
  }, error = function(e) {
    message("Failed for station: ", station_id, " — ", conditionMessage(e))
    NULL
  })
}

# Fetch for all 24 unique stations (with a small delay to respect rate limits)
all_precip_long <- map_dfr(unique_stations, function(sid) {
  Sys.sleep(0.3)
  fetch_station_precip(sid)
})

# Add station name from the stations table
all_precip_long <- all_precip_long %>%
  left_join( # keep all stations, even if some have no data
    stations %>% select(indicativo, nombre),
    by = c("station_id" = "indicativo")
  ) %>%
  relocate(station_id, nombre, year, month, precip_mm)

#------------------------------------
# Wide format: rows = station × year, cols = Jan–Dec
#------------------------------------
month_labels <- c("Jan","Feb","Mar","Apr","May","Jun",
                  "Jul","Aug","Sep","Oct","Nov","Dec")

all_precip_wide <- all_precip_long %>%
  mutate(month_label = month_labels[month]) %>%
  pivot_wider(
    id_cols     = c(station_id, nombre, year),
    names_from  = month_label,
    values_from = precip_mm,
    names_sort  = FALSE          # keep Jan–Dec order
  ) %>%
   mutate(
    yearly_max  = apply(across(all_of(month_labels)), 1, max,  na.rm = TRUE),
    yearly_min  = apply(across(all_of(month_labels)), 1, min,  na.rm = TRUE),
    yearly_mean = apply(across(all_of(month_labels)), 1, mean, na.rm = TRUE),
    yearly_sum  = apply(across(all_of(month_labels)), 1, sum,  na.rm = TRUE)
  ) %>%
  select(station_id, nombre, year, all_of(month_labels), yearly_max, yearly_min, yearly_mean, yearly_sum) %>%
  arrange(station_id, year) %>%
  arrange(station_id, year)



#------------------------------------
# Spreadsheet format for Ane:
# rows = months (1997-2026)
# cols = stations
#------------------------------------

all_precip_station_matrix <- all_precip_long %>%
  mutate(
    date = as.Date(paste0("01-", month, "-", year), format = "%d-%m-%Y"),
    station_col = paste0(nombre, "(", station_id, ")")
  )  %>%
  filter(!is.na(date)) %>%
  pivot_wider(
    id_cols     = date,
    names_from  = station_col,
    values_from = precip_mm
  )

# ensure every month is from 1997-01 to 2026-12 
all_months <- data.frame(
  date = seq(
    as.Date("1997-01-01"),
    as.Date("2026-12-01"),
    by = "month"
  )
)

all_precip_station_matrix <- all_months %>%
  left_join(all_precip_station_matrix, by = "date")

# Add Year and Month columns for readability
all_precip_station_matrix <- all_precip_station_matrix %>%
  mutate(
    Year = format(date, "%Y"),
    Month = format(date, "%m")
  ) %>%
  relocate(Year, Month, date) 




#------------------------------------
# Export to Excel
#------------------------------------
out_path <- "C:/Users/Equipo/Documents/ss_ML_local/galicia-climate-analysis/outputs/galicia_precip_monthly_1997_2026.xlsx"

write_xlsx(
  list(
    station_month_matrix = all_precip_station_matrix,
    wide = all_precip_wide,
    long = all_precip_long
  ),
  path = out_path
)

message("Exported: ", out_path)
