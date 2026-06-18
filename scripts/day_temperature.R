# day_temperature.R
# purpose: plot the temperature across spain on one day using the AEMET API
# author: madeline lebreton
# date: 28.06.2026

################
##    SETUP   ##
################
# install.packages("climaemet")

library(climaemet)
rm(list = ls()) # clear environment

## Get API key from AEMET.
# browseURL("https://opendata.aemet.es/centrodedescargas/altaUsuario")

## register API key (once)
# aemet_api_key("eyJhbGciOiJIUzI1NiJ9.eyJzdWIiOiJtYWRlbGluZS5sZWJyZXRvbkBnbWFpbC5jb20iLCJqdGkiOiJiMWEzODZjMC03ODc3LTQ4ZDktYmI5ZS05MWU1NDljYzQwNTYiLCJpc3MiOiJBRU1FVCIsImlhdCI6MTc3OTk2MjgwMCwidXNlcklkIjoiYjFhMzg2YzAtNzg3Ny00OGQ5LWJiOWUtOTFlNTQ5Y2M0MDU2Iiwicm9sZSI6IiJ9.dQrIldbr6UOQraZpyDnZR9p2obfU9GRHzuRCCBRs2B0")

#climaemet::aemet_api_key("eyJhbGciOiJIUzI1NiJ9.eyJzdWIiOiJtYWRlbGluZS5sZWJyZXRvbkBnbWFpbC5jb20iLCJqdGkiOiJiMWEzODZjMC03ODc3LTQ4ZDktYmI5ZS05MWU1NDljYzQwNTYiLCJpc3MiOiJBRU1FVCIsImlhdCI6MTc3OTk2MjgwMCwidXNlcklkIjoiYjFhMzg2YzAtNzg3Ny00OGQ5LWJiOWUtOTFlNTQ5Y2M0MDU2Iiwicm9sZSI6IiJ9.dQrIldbr6UOQraZpyDnZR9p2obfU9GRHzuRCCBRs2B0", install = TRUE)
# You need to install sf if it is not already installed.
# Run install.packages("sf") to install it.
library(ggplot2)
library(dplyr)


###################
##    GET DATA   ##
###################

# call AEMET API to get daily climate data for all stations in Spain on a specific date
all_stations <- aemet_daily_clim( 
  start = "2026-05-20",
  end = "2026-05-20",
  return_sf = TRUE
)

#####################
##    PLOT GRAPH   ##
#####################

png("C:\\Users\\Equipo\\Documents\\ss_ML_local\\galicia-climate-analysis\\outputs\\plots\\day_temperature.png", width = 1200, height = 800, res = 150)

ggplot(all_stations) +
  geom_sf(aes(colour = tmed), shape = 19, size = 2, alpha = 0.95) +
  labs(
    title = "Average temperature in Spain",
    subtitle = "20 May 2026",
    color = "Max temp.\n(celsius)",
    caption = "Source: AEMET"
  ) +
  scale_colour_gradientn(
    colours = hcl.colors(10, "RdBu", rev = TRUE),
    breaks = c(-10, -5, 0, 5, 10, 15, 20),
    guide = "legend"
  ) +
  theme_bw() +
  theme(
    panel.border = element_blank(),
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(face = "italic")
  )

dev.off()
