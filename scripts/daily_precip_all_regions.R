# =========================================================
# Galicia: Average Daily Precipitation by Province (2024)
# =========================================================

rm(list = ls())
gc()

library(climaemet)
library(dplyr)
library(ggplot2)
library(lubridate)

# -----------------------------
# API KEY
# -----------------------------
aemet_api_key("eyJhbGciOiJIUzI1NiJ9.eyJzdWIiOiJtYWRlbGluZS5sZWJyZXRvbkBnbWFpbC5jb20iLCJqdGkiOiJiMWEzODZjMC03ODc3LTQ4ZDktYmI5ZS05MWU1NDljYzQwNTYiLCJpc3MiOiJBRU1FVCIsImlhdCI6MTc3OTk2MjgwMCwidXNlcklkIjoiYjFhMzg2YzAtNzg3Ny00OGQ5LWJiOWUtOTFlNTQ5Y2M0MDU2Iiwicm9sZSI6IiJ9.dQrIldbr6UOQraZpyDnZR9p2obfU9GRHzuRCCBRs2B0")

# -----------------------------
# GALICIA STATIONS
# -----------------------------
stations <- data.frame(
  province = c("A Coruña", "Lugo", "Ourense", "Pontevedra"),
  station  = c("1387", "1505", "1690A", "1484C")
)

# -----------------------------
# DOWNLOAD DATA (2024 ONLY)
# -----------------------------
all_data <- list()

for (i in seq_len(nrow(stations))) {

  df <- get_data_aemet(
    api = "climatologias-mensuales-anuales",
    station = stations$station[i],
    start = 2024,
    end = 2024
  )

  df$province <- stations$province[i]
  all_data[[i]] <- df
}

df <- bind_rows(all_data)

# -----------------------------
# CLEAN + COMPUTE DAILY AVG
# -----------------------------

df <- df %>%
  mutate(
    # monthly precipitation (mm)
    p_mes = as.numeric(gsub(",", ".", p_mes)),

    # month number
    month = as.numeric(mes),

    # days in each month (2024 is leap year)
    days_in_month = days_in_month(ymd(paste0(2024, "-", month, "-01"))),

    # average daily precipitation (mm/day)
    prec_daily = p_mes / days_in_month
  )
  

# -----------------------------
# AVERAGE ACROSS YEAR
# -----------------------------
region_avg <- df %>%
  group_by(province) %>%
  summarise(
    avg_daily_precip = mean(prec_daily, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(avg_daily_precip)

# -----------------------------
# PLOT
# -----------------------------
ggplot(region_avg, aes(x = avg_daily_precip, y = reorder(province, avg_daily_precip))) +
  geom_col(fill = "steelblue") +
  labs(
    title = "Average Daily Precipitation in Galicia (2024)",
    x = "mm/day",
    y = "Province"
  ) +
  theme_minimal(base_size = 13)