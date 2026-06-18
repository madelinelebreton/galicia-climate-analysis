# precipitation_monthly_by_years.R
# purpose: plot the precipitation by month over a specified number of years using the AEMET API
# author: madeline lebreton
# date: 28.06.2026

# -----------------------------
# SETUP
# -----------------------------
rm(list = ls())
gc() 
## register API key (once)
aemet_api_key("eyJhbGciOiJIUzI1NiJ9.eyJzdWIiOiJtYWRlbGluZS5sZWJyZXRvbkBnbWFpbC5jb20iLCJqdGkiOiJiMWEzODZjMC03ODc3LTQ4ZDktYmI5ZS05MWU1NDljYzQwNTYiLCJpc3MiOiJBRU1FVCIsImlhdCI6MTc3OTk2MjgwMCwidXNlcklkIjoiYjFhMzg2YzAtNzg3Ny00OGQ5LWJiOWUtOTFlNTQ5Y2M0MDU2Iiwicm9sZSI6IiJ9.dQrIldbr6UOQraZpyDnZR9p2obfU9GRHzuRCCBRs2B0")


desired_years <- 60

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
  filter(indicativo == "1428")

# -----------------------------
# DOWNLOAD MONTHLY DATA
# -----------------------------
clim <- aemet_monthly_period(
  station = santiago$indicativo,
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
    precip = as.numeric(p_mes),
    precip_max = as.numeric(p_max),
    num_rainy_days = as.numeric(n_llu),
    num_thunder_days = as.numeric(n_tor)
  ) %>%
  filter(!is.na(precip)) %>%
  select(indicativo, year, month, precip, precip_max, num_rainy_days, num_thunder_days, everything())
  

# -----------------------------
# PERIOD AVERAGES
# -----------------------------
monthly_periods <- dataframe %>%
  mutate(
    period = case_when(
      year >= 1976 & year <= 2005 ~ "1976-2005",
      year >= 2006 & year <= 2015 ~ "2006-2015",
      year >= 2016 & year <= 2025 ~ "2016-2025",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(period)) %>%
  group_by(period, month) %>%
  summarise(
    avg_rainy_days = mean(num_rainy_days, na.rm = TRUE),
    .groups = "drop"
  )

# -----------------------------
# PLOT
# -----------------------------
p <- ggplot(
  monthly_periods,
  aes(
    x = month,
    y = avg_rainy_days,
    colour = period,
    group = period
  )
) +
  geom_line(linewidth = 1.4) +
  geom_point(size = 2.5) +
  
  scale_x_continuous(
    breaks = 1:12,
    labels = month.abb
  ) +
  
  scale_y_continuous(
    limits = c(0, 30)
  ) +
  
  scale_colour_manual(
    values = c(
      "1976-2005" = "grey60",
      "2006-2015" = "#6baed6",
      "2016-2025" = "#08306b"
    )
  ) +
  
  labs(
    title = "Average Monthly Rainy Days in Santiago de Compostela",
    subtitle = "Comparison of climate periods",
    x = "Month",
    y = "Average Number of Rainy Days",
    colour = "Period"
  ) +
  
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

print(p)