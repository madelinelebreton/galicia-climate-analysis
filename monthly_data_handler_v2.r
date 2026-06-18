rm(list = ls()) # clear environment
gc() # clear memory

library(climaemet)
library(dplyr)
library(purrr)
library(readr)

# API key:
aemet_api_key("eyJhbGciOiJIUzI1NiJ9.eyJzdWIiOiJtYWRlbGluZS5sZWJyZXRvbkBnbWFpbC5jb20iLCJqdGkiOiJiMWEzODZjMC03ODc3LTQ4ZDktYmI5ZS05MWU1NDljYzQwNTYiLCJpc3MiOiJBRU1FVCIsImlhdCI6MTc3OTk2MjgwMCwidXNlcklkIjoiYjFhMzg2YzAtNzg3Ny00OGQ5LWJiOWUtOTFlNTQ5Y2M0MDU2Iiwicm9sZSI6IiJ9.dQrIldbr6UOQraZpyDnZR9p2obfU9GRHzuRCCBRs2B0")


# =========================
# CONFIG
# =========================
start_year <- 1925
end_year   <- 2025
batch_size <- 100

dir.create("data/raw/monthly", recursive = TRUE, showWarnings = FALSE)
dir.create("data/logs", recursive = TRUE, showWarnings = FALSE)

log_file <- "data/logs/download_log.csv"

# Galicia station IDs 
# can also dynamically filter via aemet_stations()
galicia_stations <- c(
  "1505", # LUGO
  "1475X", # SANTIAGO
  "1387", # A CORUÑA
  "1690A", # ORENSE
  "1484C" # PONTEVEDRA
)

# =========================
# YEAR CHUNKS
# =========================
year_batches <- seq(start_year, end_year, by = batch_size)

# =========================
# LOAD LOG (for resume)
# =========================
if (file.exists(log_file)) {
  download_log <- read_csv(log_file, show_col_types = FALSE)
} else {
  download_log <- tibble(
    station = character(),
    start = integer(),
    end = integer(),
    status = character()
  )
}

# =========================
# SAFE DOWNLOAD FUNCTION
# =========================
download_chunk <- function(st, y_start, y_end) {
  
  file_name <- paste0("data/raw/monthly/", st, "_", y_start, "_", y_end, ".rds")
  
  # skip if already exists
  if (file.exists(file_name)) {
    message("Skipping (cached): ", file_name)
    return("skipped")
  }
  
  message("Downloading: ", st, " | ", y_start, "-", y_end)
  
  Sys.sleep(2)  # rate limit protection
  
  res <- tryCatch({
    
    aemet_monthly_period(
      station = st,
      start = y_start,
      end = y_end,
      verbose = FALSE
    )
    
  }, error = function(e) {
    message("ERROR: ", st, " | ", e$message)
    return(NULL)
  })
  
  # handle failure
  if (is.null(res) || nrow(res) == 0) {
    return("failed")
  }
  
  # save chunk
  saveRDS(res, file_name)
  
  return("success")
}

# =========================
# MAIN LOOP
# =========================
for (st in galicia_stations) {
  
  for (i in seq_along(year_batches)) {
    
    y_start <- year_batches[i]
    y_end <- min(y_start + batch_size - 1, end_year)
    
    status <- download_chunk(st, y_start, y_end)
    
    # update log
    download_log <- bind_rows(
      download_log,
      tibble(
        station = st,
        start = y_start,
        end = y_end,
        status = status
      )
    )
    
    write_csv(download_log, log_file)
  }
}