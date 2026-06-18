# annual_precip_anomaly.R
# purpose: annual precipitation anomalies for A Coruña (station 1387)
# author: Madeline Lebreton

# -----------------------------
# SETUP
# -----------------------------
rm(list = ls())
gc()

library(climaemet)
library(dplyr)
library(ggplot2)
library(readr)
library(lubridate)

# -----------------------------
# USER INPUTS
# -----------------------------
station_id <- "1387"  # A Coruña station ID 
start_year <- 1976
end_year <- 2025  

# -----------------------------
# API KEY
# -----------------------------
aemet_api_key("eyJhbGciOiJIUzI1NiJ9.eyJzdWIiOiJtYWRlbGluZS5sZWJyZXRvbkBnbWFpbC5jb20iLCJqdGkiOiJiMWEzODZjMC03ODc3LTQ4ZDktYmI5ZS05MWU1NDljYzQwNTYiLCJpc3MiOiJBRU1FVCIsImlhdCI6MTc3OTk2MjgwMCwidXNlcklkIjoiYjFhMzg2YzAtNzg3Ny00OGQ5LWJiOWUtOTFlNTQ5Y2M0MDU2Iiwicm9sZSI6IiJ9.dQrIldbr6UOQraZpyDnZR9p2obfU9GRHzuRCCBRs2B0")


# -----------------------------
# DOWNLOAD MONTHLY DATA
# -----------------------------
precip_monthly <- aemet_monthly_period(
  station = station_id,
  start = start_year,
  end = end_year
)

# -----------------------------
# CLEAN DATA
# -----------------------------
precip_monthly <- precip_monthly %>%
  mutate(
    date = as.Date(paste0(fecha, "-01")),
    year = year(date),
    month = month(date),
    precip = as.numeric(p_mes),
  ) %>%
  filter(!is.na(year)) %>%
  select(indicativo, year, month, precip, everything())
  

# -----------------------------
# ANNUAL PRECIPITATION TOTALS
# -----------------------------
annual_precip <- precip_monthly %>%
  group_by(year) %>%
  summarise(
    annual_prec_mm = sum(precip),
    .groups = "drop"
  )

winter_precip <- precip_monthly %>%
  filter(month %in% c(12, 1, 2)) %>%
  group_by(year) %>%
  summarise(
    winter_prec_mm = sum(precip),
    mean_winter_prec_mm = mean(precip),
    .groups = "drop"
  )

summer_precip <- precip_monthly %>%
  filter(month %in% c(6, 7, 8)) %>%
  group_by(year) %>%
  summarise(
    summer_prec_mm = sum(precip),
    mean_summer_prec_mm = mean(precip),
    .groups = "drop"
  )

# -----------------------------
# LONG-TERM AVERAGE
# -----------------------------
mean_precip <- mean(
  annual_precip$annual_prec_mm,
  na.rm = TRUE
)

mean_winter_precip <- mean(
  winter_precip$winter_prec_mm,
  na.rm = TRUE
)

mean_summer_precip <- mean(
  summer_precip$summer_prec_mm,
  na.rm = TRUE
)

# -----------------------------
# ANOMALIES
# -----------------------------
annual_precip <- annual_precip %>%
  mutate(
    anomaly_mm = annual_prec_mm - mean_precip,
    anomaly_type = ifelse(
      anomaly_mm >= 0,
      "Above average",
      "Below average"
    )
  )

winter_precip <- winter_precip %>%
  mutate(
    anomaly_mm = winter_prec_mm - mean_winter_precip,
    anomaly_type = ifelse(
      anomaly_mm >= 0,
      "Above average",
      "Below average"
    )
  )

summer_precip <- summer_precip %>%
  mutate(
    anomaly_mm = summer_prec_mm - mean_summer_precip,
    anomaly_type = ifelse(
      anomaly_mm >= 0,
      "Above average",
      "Below average"
    )
  )

# -----------------------------
# SAVE DATA
# -----------------------------
write_csv(
  annual_precip,
  "C:\\Users\\Equipo\\Documents\\ss_ML_local\\galicia-climate-analysis\\outputs\\coruna_precipitation_anomalies_1976_2025.csv"
)

# -----------------------------
# PLOT
# -----------------------------
p <- ggplot(
  summer_precip,
  aes(
    x = year,
    y = anomaly_mm,
    fill = anomaly_type
  )
) +
  geom_col(width = 0.8) +
  geom_hline(
    yintercept = 0,
    linewidth = 0.8,
    colour = "black"
  ) +
  scale_fill_manual(
    values = c(
      "Above average" = "#2166AC",
      "Below average" = "#B2182B"
    )
  ) +
  scale_x_continuous(
    breaks = seq(1976, 2025, by = 5)
  ) +
  labs(
    title = "Summer Precipitation Anomalies",
    subtitle = paste0(
      "A Coruña (Station 1387)\n",
      "Reference period: 1976–2025\n",
      "Mean summer precipitation = ",
      round(mean_summer_precip, 1),
      " mm"
    ),
    x = "Year",
    y = "Difference from 1976–2025 average (mm)",
    fill = NULL
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "top"
  )

print(p)

# -----------------------------
# SAVE FIGURE
# -----------------------------
ggsave(
  "C:\\Users\\Equipo\\Documents\\ss_ML_local\\galicia-climate-analysis\\outputs\\coruna_precipitation_anomalies_1976_2025.png",
  plot = p,
  width = 12,
  height = 6,
  dpi = 300
)

cat(
  "\nMean summer precipitation (1976–2025):",
  round(mean_summer_precip, 1),
  "mm\n"
)