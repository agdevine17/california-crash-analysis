# Pedestrian Crash Severity Analysis

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

pedestrian_crashes <- crashes |>
  filter(
    !is.na(crash_date),
    year >= 2016,
    year <= 2025
  ) |>
  mutate(
    pedestrian_crash =
      coalesce(
        collision_type_description == "VEHICLE/PEDESTRAIN",
        FALSE
      ) |
      coalesce(
        motor_vehicle_involved_with_desc == "PEDESTRIAN",
        FALSE
      )
  ) |>
  filter(pedestrian_crash)

cat(
  "Total pedestrian involved crashes:",
  nrow(pedestrian_crashes),
  "\n"
)

pedestrian_crashes <- pedestrian_crashes |>
  mutate(
    injury_crash = case_when(
      is.na(number_injured) ~ NA,
      number_injured > 0 ~ TRUE,
      TRUE ~ FALSE
    ),

    fatal_crash = case_when(
      is.na(number_killed) ~ NA,
      number_killed > 0 ~ TRUE,
      TRUE ~ FALSE
    )
  )

# Monthly pedestrian crash severity

monthly_severity <- pedestrian_crashes |>
  group_by(month) |>
  summarise(
    total_pedestrian_crashes = n(),

    injury_crashes = sum(
      injury_crash,
      na.rm = TRUE
    ),

    fatal_crashes = sum(
      fatal_crash,
      na.rm = TRUE
    ),

    known_injury = sum(!is.na(injury_crash)),
    known_fatal = sum(!is.na(fatal_crash)),

    missing_injury = sum(is.na(injury_crash)),
    missing_fatal = sum(is.na(fatal_crash)),

    .groups = "drop"
  ) |>
  mutate(
    month_name = month.abb[month],

    injury_percentage =
      100 * injury_crashes / known_injury,

    fatal_percentage =
      100 * fatal_crashes / known_fatal
  )

print(monthly_severity, n = Inf)
monthly_severity |>
  select(
    month_name,
    total_pedestrian_crashes,
    injury_crashes,
    fatal_crashes,
    injury_percentage,
    fatal_percentage
  ) |>
  print(n = Inf, width = Inf)

# October vs. other months

october_severity <- pedestrian_crashes |>
  mutate(
    period = if_else(
      month == 10,
      "October",
      "Other Months"
    )
  ) |>
  group_by(period) |>
  summarise(
    total_crashes = n(),

    injury_crashes = sum(injury_crash, na.rm = TRUE),
    fatal_crashes = sum(fatal_crash, na.rm = TRUE),

    known_injury = sum(!is.na(injury_crash)),
    known_fatal = sum(!is.na(fatal_crash)),

    .groups = "drop"
  ) |>
  mutate(
    injury_percentage =
      100 * injury_crashes / known_injury,

    fatal_percentage =
      100 * fatal_crashes / known_fatal
  )

print(october_severity)
october_severity |>
  select(
    period,
    injury_percentage,
    fatal_percentage
  ) |>
  print(width = Inf)

# Yearly pedestrian crash severity

yearly_severity <- pedestrian_crashes |>
  group_by(year) |>
  summarise(
    total_pedestrian_crashes = n(),

    injury_crashes = sum(injury_crash, na.rm = TRUE),
    fatal_crashes = sum(fatal_crash, na.rm = TRUE),

    known_injury = sum(!is.na(injury_crash)),
    known_fatal = sum(!is.na(fatal_crash)),

    missing_injury = sum(is.na(injury_crash)),
    missing_fatal = sum(is.na(fatal_crash)),

    .groups = "drop"
  ) |>
  mutate(
    injury_percentage =
      100 * injury_crashes / known_injury,

    fatal_percentage =
      100 * fatal_crashes / known_fatal
  )

print(yearly_severity, n = Inf)
yearly_severity |>
  select(
    year,
    total_pedestrian_crashes,
    injury_percentage,
    fatal_percentage,
    missing_injury,
    missing_fatal
  ) |>
  print(n = Inf, width = Inf)

write_csv(
  monthly_severity,
  "outputs/tables/pedestrian-severity-monthly.csv"
)

write_csv(
  october_severity,
  "outputs/tables/pedestrian-severity-october.csv"
)

write_csv(
  yearly_severity,
  "outputs/tables/pedestrian-severity-yearly.csv"
)

# Exclude 2025

october_severity_2016_2024 <- pedestrian_crashes |>
  filter(year <= 2024) |>
  mutate(
    period = if_else(
      month == 10,
      "October",
      "Other Months"
    )
  ) |>
  group_by(period) |>
  summarise(
    total_crashes = n(),

    injury_crashes = sum(injury_crash, na.rm = TRUE),
    fatal_crashes = sum(fatal_crash, na.rm = TRUE),

    known_injury = sum(!is.na(injury_crash)),
    known_fatal = sum(!is.na(fatal_crash)),

    .groups = "drop"
  ) |>
  mutate(
    injury_percentage =
      100 * injury_crashes / known_injury,

    fatal_percentage =
      100 * fatal_crashes / known_fatal
  )

print(october_severity_2016_2024, width = Inf)

write_csv(
  october_severity_2016_2024,
  "outputs/tables/pedestrian-severity-october-2016-2024.csv"
)
