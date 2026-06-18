# data handler for AEMET data, for visualization
# author: madeline lebreton with help from https://ropenspain.github.io/climaemet/articles 
# date: 6.03.2026

# =========================
# SETUP
# =========================
library(climaemet) # Meteorological data
library(mapSpain) # Base maps of Spain
library(classInt) # Classification
library(terra) # Raster handling
library(sf) # Spatial shape handling
library(gstat) # Spatial interpolation
library(geoR) # Spatial analysis
library(tidyverse) # Collection of R packages designed for data science
library(tidyterra) # Tidyverse methods for the terra package

# API key
aemet_api_key("eyJhbGciOiJIUzI1NiJ9.eyJzdWIiOiJtYWRlbGluZS5sZWJyZXRvbkBnbWFpbC5jb20iLCJqdGkiOiJiMWEzODZjMC03ODc3LTQ4ZDktYmI5ZS05MWU1NDljYzQwNTYiLCJpc3MiOiJBRU1FVCIsImlhdCI6MTc3OTk2MjgwMCwidXNlcklkIjoiYjFhMzg2YzAtNzg3Ny00OGQ5LWJiOWUtOTFlNTQ5Y2M0MDU2Iiwicm9sZSI6IiJ9.dQrIldbr6UOQraZpyDnZR9p2obfU9GRHzuRCCBRs2B0")

stations <- aemet_stations()

# =========================
# SELECT DATA
# =========================
start_date <- "2000-01-01"
end_date <- "2026-12-01"

clim_data <- aemet_daily_clim(
  start = date_select,
  end = date_select,
  return_sf = TRUE
)