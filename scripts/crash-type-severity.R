library(tidyverse)
library(lubridate)
library(scales)

crashes <- readRDS(
  "data/processed/crashes_combined.rds"
) |>
  as_tibble() |>
  mutate(
    number_injured = as.numeric(number_injured),
    number_killed = as.numeric(number_killed)
  )

# Check relevant column names
names(crashes)[
  str_detect(
    names(crashes),
    "collision_type|motor_vehicle_involved|pedestrian|number_injured|number_killed"
  )
]

# Examine collision types
crashes |>
  count(collision_type_description, sort = TRUE) |>
  print(n = Inf)

# Examine what motor vehicles collided with
crashes |>
  count(motor_vehicle_involved_with_desc, sort = TRUE) |>
  print(n = Inf)

# Check original values that could not be converted
crashes_raw <- readRDS(
  "data/processed/crashes_combined.rds"
) |>
  as_tibble()

crashes_raw |>
  filter(
    !is.na(number_injured),
    number_injured != "",
    is.na(suppressWarnings(as.numeric(number_injured)))
  ) |>
  count(number_injured, sort = TRUE) |>
  print(n = Inf)

crashes_raw |>
  filter(
    !is.na(number_killed),
    number_killed != "",
    is.na(suppressWarnings(as.numeric(number_killed)))
  ) |>
  count(number_killed, sort = TRUE) |>
  print(n = Inf)

# Examine injury and fatality counts
crashes |>
  summarise(
    total_crashes = n(),
    injury_crashes = sum(number_injured > 0, na.rm = TRUE),
    fatal_crashes = sum(number_killed > 0, na.rm = TRUE),
    total_injured = sum(number_injured, na.rm = TRUE),
    total_killed = sum(number_killed, na.rm = TRUE)
  )



# Pedestrian crash classification


crashes <- crashes |>
  mutate(
    pedestrian_collision_type =
      collision_type_description == "VEHICLE/PEDESTRAIN",

    pedestrian_involved_with =
      motor_vehicle_involved_with_desc == "PEDESTRIAN"
  )

# Compare both pedestrian classifications
pedestrian_classification <- crashes |>
  count(
    pedestrian_collision_type,
    pedestrian_involved_with,
    name = "n_crashes"
  )

print(pedestrian_classification)

crashes |>
  summarise(
    missing_injured = sum(is.na(number_injured)),
    missing_killed = sum(is.na(number_killed)),
    missing_collision_type =
      sum(is.na(collision_type_description) |
            collision_type_description == ""),
    missing_involved_with =
      sum(is.na(motor_vehicle_involved_with_desc) |
            motor_vehicle_involved_with_desc == "")
  ) |>
  print()



# Combined pedestrian crash indicator


crashes <- crashes |>
  mutate(
    pedestrian_crash =
      coalesce(pedestrian_collision_type, FALSE) |
      coalesce(pedestrian_involved_with, FALSE)
  )

# Verify the total
crashes |>
  summarise(
    total_crashes = n(),
    pedestrian_crashes = sum(pedestrian_crash),
    pedestrian_percentage =
      100 * mean(pedestrian_crash)
  ) |>
  print()



# Monthly pedestrian crash analysis


monthly_pedestrian <- crashes |>
  filter(
    !is.na(crash_date),
    year >= 2016,
    year <= 2025
  ) |>
  group_by(month) |>
  summarise(
    total_crashes = n(),
    pedestrian_crashes = sum(pedestrian_crash),
    pedestrian_percentage =
      100 * pedestrian_crashes / total_crashes,
    .groups = "drop"
  ) |>
  mutate(
    month_name = factor(
      month.abb[month],
      levels = month.abb
    )
  ) |>
  arrange(month)

print(monthly_pedestrian)

# Missing values by month

missing_by_month <- crashes |>
  filter(
    !is.na(crash_date),
    year >= 2016,
    year <= 2025
  ) |>
  group_by(month) |>
  summarise(
    total_crashes = n(),

    missing_injured = sum(is.na(number_injured)),
    missing_killed = sum(is.na(number_killed)),

    missing_collision_type = sum(
      is.na(collision_type_description) |
        trimws(collision_type_description) == ""
    ),

    missing_involved_with = sum(
      is.na(motor_vehicle_involved_with_desc) |
        trimws(motor_vehicle_involved_with_desc) == ""
    ),

    .groups = "drop"
  ) |>
  mutate(
    month_name = month.abb[month],

    pct_missing_injured =
      100 * missing_injured / total_crashes,

    pct_missing_killed =
      100 * missing_killed / total_crashes,

    pct_missing_collision_type =
      100 * missing_collision_type / total_crashes,

    pct_missing_involved_with =
      100 * missing_involved_with / total_crashes
  ) |>
  select(
    month_name,
    starts_with("pct_missing")
  )

print(missing_by_month, n = Inf)

# Missing pedestrian classification information

pedestrian_missing <- crashes |>
  mutate(
    missing_collision_type =
      is.na(collision_type_description) |
      trimws(collision_type_description) == "",

    missing_involved_with =
      is.na(motor_vehicle_involved_with_desc) |
      trimws(motor_vehicle_involved_with_desc) == "",

    both_missing =
      missing_collision_type & missing_involved_with
  ) |>
  summarise(
    total_crashes = n(),
    n_both_missing = sum(both_missing),
    percent_both_missing =
      100 * n_both_missing / total_crashes
  )

print(pedestrian_missing)

# Missing severity data among pedestrian crashes

pedestrian_missing_severity <- crashes |>
  filter(pedestrian_crash) |>
  summarise(
    pedestrian_crashes = n(),

    missing_injured = sum(is.na(number_injured)),
    missing_killed = sum(is.na(number_killed)),

    pct_missing_injured =
      100 * mean(is.na(number_injured)),

    pct_missing_killed =
      100 * mean(is.na(number_killed))
  )

print(pedestrian_missing_severity)

# Missing severity data by year

missing_by_year <- crashes |>
  filter(
    !is.na(crash_date),
    year >= 2016,
    year <= 2025
  ) |>
  group_by(year) |>
  summarise(
    total_crashes = n(),
    missing_injured = sum(is.na(number_injured)),
    missing_killed = sum(is.na(number_killed)),
    pct_missing_injured =
      100 * missing_injured / total_crashes,
    pct_missing_killed =
      100 * missing_killed / total_crashes,
    .groups = "drop"
  )

print(missing_by_year, n = Inf)

# Missing severity data by month in 2025

missing_2025 <- crashes |>
  filter(
    year == 2025,
    !is.na(crash_date)
  ) |>
  group_by(month) |>
  summarise(
    total_crashes = n(),
    missing_injured = sum(is.na(number_injured)),
    missing_killed = sum(is.na(number_killed)),
    pct_missing_injured =
      100 * missing_injured / total_crashes,
    pct_missing_killed =
      100 * missing_killed / total_crashes,
    .groups = "drop"
  ) |>
  mutate(
    month_name = month.abb[month]
  ) |>
  select(
    month_name,
    total_crashes,
    missing_injured,
    missing_killed,
    pct_missing_injured,
    pct_missing_killed
  )

print(missing_2025, n = Inf)

# Missing severity data for pedestrian crashes by year

pedestrian_missing_by_year <- crashes |>
  filter(
    pedestrian_crash,
    year >= 2016,
    year <= 2025
  ) |>
  group_by(year) |>
  summarise(
    pedestrian_crashes = n(),
    missing_injured = sum(is.na(number_injured)),
    missing_killed = sum(is.na(number_killed)),
    pct_missing_injured =
      100 * missing_injured / pedestrian_crashes,
    pct_missing_killed =
      100 * missing_killed / pedestrian_crashes,
    .groups = "drop"
  )

print(pedestrian_missing_by_year, n = Inf)

# Save processed tables

write_csv(
  monthly_pedestrian,
  "outputs/tables/monthly-pedestrian.csv"
)

write_csv(
  pedestrian_classification,
  "outputs/tables/pedestrian-classification.csv"
)
