library(climaemet)
library(ggplot2)
library(dplyr)
library(tidyr)

# ── Configuration ──────────────────────────────────────────────────────────────
aemet_api_key("eyJhbGciOiJIUzI1NiJ9.eyJzdWIiOiJtYWRlbGluZS5sZWJyZXRvbkBnbWFpbC5jb20iLCJqdGkiOiJiMWEzODZjMC03ODc3LTQ4ZDktYmI5ZS05MWU1NDljYzQwNTYiLCJpc3MiOiJBRU1FVCIsImlhdCI6MTc3OTk2MjgwMCwidXNlcklkIjoiYjFhMzg2YzAtNzg3Ny00OGQ5LWJiOWUtOTFlNTQ5Y2M0MDU2Iiwicm9sZSI6IiJ9.dQrIldbr6UOQraZpyDnZR9p2obfU9GRHzuRCCBRs2B0")

START_YEAR <- 1976
END_YEAR   <- 2025

stations <- tibble::tribble(
  ~indicativo, ~name,
  "1387",  "A Coruña",
  "1354C", "Ferrol",
  "1484C", "Pontevedra",
  "1690A", "Ourense",
  "1495",  "Vigo Aeropuerto",
  "1505",  "Lugo Aeropuerto",
  "1631E", "A Pobra de Trives",
  "1735X", "Xinzo de Limia",
  "1700X", "O Carballiño",
  "1351",  "Estaca de Bares"
)

# ── Fetch data with tryCatch ───────────────────────────────────────────────────
fetch_station <- function(indicativo, name) {
  Sys.sleep(3)
  tryCatch(
    {
      df <- aemet_monthly_period(
        station = indicativo,
        start   = START_YEAR,
        end     = END_YEAR,
        verbose = FALSE
      )
      if (is.null(df) || nrow(df) == 0) {
        message("No data returned for: ", name, " (", indicativo, ")")
        return(NULL)
      }
      df$station_name <- name
      df
    },
    error = function(e) {
      message("Error fetching ", name, " (", indicativo, "): ", e$message)
      NULL
    }
  )
}

raw_list <- mapply(fetch_station,
                   stations$indicativo,
                   stations$name,
                   SIMPLIFY = FALSE)

all_data <- dplyr::bind_rows(Filter(Negate(is.null), raw_list))

# ── Clean & reshape ────────────────────────────────────────────────────────────
monthly_precip <- all_data |>
  mutate(
    year  = as.integer(substr(fecha, 1, 4)),
    month = as.integer(substr(fecha, 6, 7)),
    p_mes = suppressWarnings(as.numeric(p_mes))   # monthly precipitation (mm)
  ) %>%
  filter(!is.na(p_mes)) %>%
  filter(month < 13) %>%
  mutate(station_name = factor(station_name, levels = stations$name))


# ── Plot ───────────────────────────────────────────────────────────────────────
month_labels <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
                  "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")

ggplot(monthly_precip, aes(x = month, y = p_mes,
                            group = year, colour = year)) +
  geom_line(alpha = 0.6, linewidth = 0.4) +
  scale_x_continuous(
    breaks = 1:12,
    labels = month_labels
  ) +
  scale_y_continuous(
    limits = c(0, NA),   # fixed lower bound; upper determined by data
    expand = expansion(mult = c(0, 0.05))
  ) +
  scale_colour_gradient(
    low    = "#b3d4f0",   # light blue  → older years
    high   = "#08306b",   # dark blue   → recent years
    name   = "Year",
    breaks = c(START_YEAR, END_YEAR),
    labels = c(START_YEAR, END_YEAR)
  ) +
  facet_wrap(~ station_name, ncol = 2, scales = "fixed") +
  labs(
    title    = "Monthly Precipitation by Station (Galicia)",
    subtitle = paste0(START_YEAR, "–", END_YEAR),
    x        = NULL,
    y        = "Precipitation (mm)"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title       = element_text(face = "bold", size = 14),
    plot.subtitle    = element_text(colour = "grey40", margin = margin(b = 10)),
    strip.text       = element_text(face = "bold", size = 10),
    axis.text.x      = element_text(angle = 45, hjust = 1, size = 8),
    panel.grid.minor = element_blank(),
    legend.position  = "right"
  )