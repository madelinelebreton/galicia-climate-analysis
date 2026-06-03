# precipitation_monthly_by_years.R
# purpose: plot the precipitation by month over a specified number of years using the AEMET API
# author: madeline lebreton
# date: 28.06.2026

# -----------------------------
# SETUP
# -----------------------------
# install.packages("climaemet")

## Get API key from AEMET.
# browseURL("https://opendata.aemet.es/centrodedescargas/altaUsuario")

## register API key (once)
# aemet_api_key("eyJhbGciOiJIUzI1NiJ9.eyJzdWIiOiJtYWRlbGluZS5sZWJyZXRvbkBnbWFpbC5jb20iLCJqdGkiOiJiMWEzODZjMC03ODc3LTQ4ZDktYmI5ZS05MWU1NDljYzQwNTYiLCJpc3MiOiJBRU1FVCIsImlhdCI6MTc3OTk2MjgwMCwidXNlcklkIjoiYjFhMzg2YzAtNzg3Ny00OGQ5LWJiOWUtOTFlNTQ5Y2M0MDU2Iiwicm9sZSI6IiJ9.dQrIldbr6UOQraZpyDnZR9p2obfU9GRHzuRCCBRs2B0")
rm(list = ls())
gc()


desired_years <- 30

# -----------------------------
# LIBRARIES
# -----------------------------
library(climaemet)
library(dplyr)
library(ggplot2)
library(lubridate)



# -----------------------------
# FIND STATION
# -----------------------------
stations <- aemet_stations()

santiago <- stations %>%
  filter(grepl("SANTIAGO DE COMPOSTELA", nombre, ignore.case = TRUE)) %>%
  slice(1)

station_id <- santiago$indicativo

# -----------------------------
# DOWNLOAD MONTHLY DATA
# -----------------------------
clim <- aemet_monthly_period(
  station = station_id,
  start = year(Sys.Date()) - desired_years,
  end = year(Sys.Date())
)

# -----------------------------
# CLEAN DATA
# -----------------------------
dataframe <- clim %>%
  mutate(
    date = as.Date(paste0(fecha, "-01")),
    year = year(date),
    month = month(date),
    precip = as.numeric(p_mes)
  ) %>%
  filter(!is.na(precip))

# -----------------------------
# PLOT
# -----------------------------
p <- ggplot(
  dataframe,
  aes(
    x = month,
    y = precip,
    group = year,
    colour = year
  )
) +
  geom_line(alpha = 0.9, linewidth = 0.8) +
  scale_colour_gradient(
    low = "grey85",
    high = "#2105c0"
  ) +
  scale_x_continuous(
    breaks = 1:12,
    labels = month.abb
  ) +

  scale_y_continuous(
    name = "Monthly Precipitation (mm)",
    limits = c(0, 1000),
    breaks = seq(0, 1000, 100)
  ) +

  labs(
    title = "Monthly precipitation – Santiago de Compostela",
    subtitle = paste0(
      "Last ", desired_years, " years (", year(Sys.Date()) - desired_years, "–", year(Sys.Date()), ")"
    ),
    x = "Month",
    y = "Precipitation (mm)",
    colour = "Year",
    caption = "Source: AEMET OpenData via climaemet"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

print(p)

# -----------------------------
# EXPORT
# -----------------------------
ggsave(
  "C:\\Users\\Equipo\\Documents\\ss_ML_local\\galicia-climate-analysis\\outputs\\plots\\santiago_precip_30y.png",
  p,
  width = 15,
  height = 4,
  dpi = 300
)