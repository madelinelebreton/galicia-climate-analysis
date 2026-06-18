# Get AEMET station codes for municipalities in Galicia, Spain
# author: Madeline Lebreton
# date: 6/18/2026

rm(list = ls())
gc()

library(climaemet)
library(sf)
library(dplyr)
library(tidygeocoder)
library(purrr)
library(readxl)
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
