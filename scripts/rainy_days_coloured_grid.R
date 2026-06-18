# =====================================================
# GALICIA RAINY DAY ANOMALY HEATMAP
# =====================================================

## Get API key from AEMET.
# browseURL("https://opendata.aemet.es/centrodedescargas/altaUsuario")

## register API key (once)
# aemet_api_key("eyJhbGciOiJIUzI1NiJ9.eyJzdWIiOiJtYWRlbGluZS5sZWJyZXRvbkBnbWFpbC5jb20iLCJqdGkiOiJiMWEzODZjMC03ODc3LTQ4ZDktYmI5ZS05MWU1NDljYzQwNTYiLCJpc3MiOiJBRU1FVCIsImlhdCI6MTc3OTk2MjgwMCwidXNlcklkIjoiYjFhMzg2YzAtNzg3Ny00OGQ5LWJiOWUtOTFlNTQ5Y2M0MDU2Iiwicm9sZSI6IiJ9.dQrIldbr6UOQraZpyDnZR9p2obfU9GRHzuRCCBRs2B0")
rm(list = ls())
gc()



library(climaemet)
library(dplyr)
library(ggplot2)
library(lubridate)
library(stringr)

# -----------------------------
# USER INPUT
# -----------------------------
rain_threshold <- 20  # mm/day

start_year <- year(Sys.Date()) - 60
end_year <- year(Sys.Date())

# -----------------------------
# GET GALICIA STATIONS
# -----------------------------
stations <- aemet_stations()

galicia_stations <- stations %>%
  filter(
    provincia %in% c(
      "CORUNA",
      "LUGO",
      "PONTEVEDRA",
      "ORENSE"
    )
  )

# -----------------------------
# DOWNLOAD DAILY DATA
# -----------------------------
all_data <- list()

for (i in seq_len(nrow(galicia_stations))) {

  station_id <- galicia_stations$indicativo[i]
  station_name <- galicia_stations$nombre[i]

  cat("Downloading:", station_name, "\n")

  try({

    daily <- aemet_daily_clim(
      station = station_id,
      start = paste0(start_year, "-01-01"),
      end = paste0(end_year, "-12-31")
    )

    daily$station <- station_name

    all_data[[i]] <- daily

  }, silent = TRUE)
}

# -----------------------------
# COMBINE
# -----------------------------
df <- bind_rows(all_data)

# -----------------------------
# CLEAN DATA
# -----------------------------
df <- df %>%
  mutate(
    date = as.Date(fecha),
    year = year(date),

    # precipitation column
    precip = as.numeric(str_replace(prec, ",", "."))

  ) %>%
  filter(!is.na(precip))

# -----------------------------
# COUNT RAINY DAYS
# -----------------------------
yearly_counts <- df %>%
  group_by(station, year) %>%
  summarise(
    rainy_days = sum(precip > rain_threshold, na.rm = TRUE),
    .groups = "drop"
  )

# -----------------------------
# CALCULATE 60-YEAR AVERAGE
# -----------------------------
station_means <- yearly_counts %>%
  group_by(station) %>%
  summarise(
    mean_rainy_days = mean(rainy_days, na.rm = TRUE)
  )

# -----------------------------
# CALCULATE ANOMALY
# -----------------------------
plot_data <- yearly_counts %>%
  left_join(station_means, by = "station") %>%
  mutate(
    anomaly = rainy_days - mean_rainy_days
  )

# -----------------------------
# PLOT
# -----------------------------
p <- ggplot(
  plot_data,
  aes(
    x = year,
    y = reorder(station, mean_rainy_days),
    fill = anomaly
  )
) +
  geom_tile() +

  scale_fill_gradient2(
    low = "blue",
    mid = "grey85",
    high = "darkred",
    midpoint = 0,
    name = paste0(
      "Rainy-day anomaly\n(days > ",
      rain_threshold,
      " mm)"
    )
  ) +

  labs(
    title = "Rainy-day anomalies across Galicia",
    subtitle = paste0(
      "Number of days per year with precipitation > ",
      rain_threshold,
      " mm"
    ),
    x = "Year",
    y = "Weather station",
    caption = "Source: AEMET OpenData via climaemet"
  ) +

  theme_minimal() +

  theme(
    plot.title = element_text(face = "bold"),
    axis.text.y = element_text(size = 7)
  )

print(p)

# -----------------------------
# EXPORT
# -----------------------------
ggsave(
  "galicia_rainy_day_anomaly_heatmap.png",
  p,
  width = 14,
  height = 10,
  dpi = 300
)