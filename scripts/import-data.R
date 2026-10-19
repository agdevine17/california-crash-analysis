library(tidyverse)
library(data.table)
library(lubridate)
library(janitor)

# Find annual crash files
crash_files <- list.files(
  "data/raw",
  pattern = "crashes_20(1[6-9]|2[0-5])\\.csv$",
  full.names = TRUE,
  ignore.case = TRUE
)

print(crash_files)
stopifnot(length(crash_files) == 10)

# Read one year's crash data
read_crashes <- function(file) {
  
  source_year <- as.integer(
    str_extract(basename(file), "\\d{4}")
  )
  
  df <- fread(
    file,
    colClasses = "character",
    showProgress = TRUE
  ) |>
    clean_names()
  
  columns <- c(
    "collision_id",
    "crash_date_time",
    "city_name",
    "county_code",
    "collision_type_description",
    "motor_vehicle_involved_with_desc",
    "number_injured",
    "number_killed",
    "weather_1",
    "weather_2",
    "lighting_description",
    "latitude",
    "longitude"
  )
  
  # Check which columns exist
  missing <- setdiff(columns, names(df))
  
  if (length(missing) > 0) {
    stop(
      "Missing columns in ", basename(file),
      ": ", paste(missing, collapse = ", ")
    )
  }
  
  df |>
    select(all_of(columns)) |>
    mutate(source_year = source_year)
}

# Combine all 10 years
crashes <- map_dfr(crash_files, read_crashes)

# Parse dates
crashes <- crashes |>
  mutate(
    crash_date_time = mdy_hms(
      crash_date_time,
      quiet = TRUE
    ),
    crash_date = as.Date(crash_date_time),
    year = year(crash_date),
    month = month(crash_date),
    month_name = month(
      crash_date,
      label = TRUE,
      abbr = TRUE
    )
  )

# Validate combined dataset
print(crashes |> count(source_year))
print(crashes |> count(year))
print(sum(is.na(crashes$crash_date)))
print(sum(duplicated(crashes$collision_id)))

# Save
saveRDS(
  crashes,
  "data/processed/crashes_combined.rds"
)

# Load
crashes <- readRDS(
  "data/processed/crashes_combined.rds"
)
