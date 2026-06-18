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

station_id <-  # desired station ID


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
    precip = as.numeric(p_mes),
    precip_max = as.numeric(p_max),
    num_rainy_days = as.numeric(n_llu),
    num_thunder_days = as.numeric(n_tor)
  ) %>%
  filter(!is.na(precip)) %>%
  select(indicativo, year, month, precip, precip_max, num_rainy_days, num_thunder_days, everything())
  

# -----------------------------
# PLOT
# -----------------------------
monthly_decades <- dataframe %>%
  mutate(
    decade = floor(year / 10) * 10
  ) %>%
  group_by(decade, month) %>%
  summarise(
    avg_rainy_days = mean(num_rainy_days, na.rm = TRUE),
    .groups = "drop"
  )

n_decades <- length(unique(monthly_decades$decade))

p <-ggplot(
  monthly_decades,
  aes(
    x      = month,
    y      = avg_rainy_days,
    colour = factor(decade),
    group  = decade
  )
) +
  geom_line(linewidth = 1.2) +
  geom_point() +
  scale_x_continuous(
    breaks = 1:12,
    labels = month.abb
  ) +
  scale_y_continuous(
    limits = c(0, 30),
   ) +
  scale_colour_manual(
    values = colorRampPalette(c("#c6dbef", "#08306b"))(n_decades)
  ) +
  guides(colour = guide_legend(reverse = TRUE)) +
  labs(
    title  = "Average Monthly Rainy Days by Decade",
    x      = "Month",
    y      = "Average Number of Rainy Days",
    colour = "Decade"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# -----------------------------
# EXPORT
# -----------------------------
ggsave(
  "C:\\Users\\Equipo\\Documents\\ss_ML_local\\galicia-climate-analysis\\outputs\\plots\\santiago_num_rainy_days_60y_decade_averages.png",
  p,
  dpi = 300
)

print("Done! Plot saved to outputs/plots/santiago_num_rainy_days_60y_decade_averages.png")