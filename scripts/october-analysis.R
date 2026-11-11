
library(tidyverse)
library(lubridate)
library(scales)

crashes <- readRDS(
  "data/processed/crashes_combined.rds"
)

# Count crashes for each day of October in each year
october_daily <- crashes |>
  filter(
    !is.na(crash_date),
    year >= 2016,
    year <= 2025,
    month == 10
  ) |>
  mutate(
    day = day(crash_date)
  ) |>
  count(year, day, name = "n_crashes") |>
  complete(
    year = 2016:2025,
    day = 1:31,
    fill = list(n_crashes = 0)
  )

# Calculate average crashes per day of October
october_average <- october_daily |>
  group_by(day) |>
  summarise(
    avg_crashes = mean(n_crashes),
    sd_crashes = sd(n_crashes),
    .groups = "drop"
  )

print(october_average)

# Plot average crashes per day
p1 <- ggplot(
  october_average,
  aes(x = day, y = avg_crashes)
) +
  geom_col(
    aes(fill = day >= 16),
    width = 0.8
  ) +
  scale_fill_manual(
    values = c(
      "FALSE" = "steelblue",
      "TRUE" = "darkorange"
    ),
    guide = "none"
  ) +
  scale_x_continuous(
    breaks = 1:31
  ) +
  scale_y_continuous(
    labels = comma,
    expand = expansion(mult = c(0, 0.05))
  ) +
  labs(
    title = "Average Daily California Crashes in October",
    subtitle = "2016-2025",
    x = "Day of October",
    y = "Average Number of Crashes",
    caption = "Source: California Highway Patrol CCRS"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.major.x = element_blank()
  )

print(p1)

ggsave(
  "outputs/october-daily-average.png",
  plot = p1,
  width = 12,
  height = 6,
  dpi = 300
)


p2 <- ggplot(
  october_daily,
  aes(
    x = day,
    y = n_crashes,
    fill = day >= 16
  )
) +
  geom_col(width = 0.8) +
  scale_fill_manual(
    values = c(
      "FALSE" = "steelblue",
      "TRUE" = "darkorange"
    ),
    guide = "none"
  ) +
  facet_wrap(
    ~ year,
    ncol = 2
  ) +
  scale_x_continuous(
    breaks = seq(1, 31, by = 5)
  ) +
  scale_y_continuous(
    labels = comma
  ) +
  labs(
    title = "Daily October Crashes by Year",
    subtitle = "California, 2016-2025",
    x = "Day of October",
    y = "Number of Crashes",
    caption = "Source: California Highway Patrol CCRS"
  ) +
  theme_minimal(base_size = 12)

print(p2)

ggsave(
  "outputs/october-daily-by-year.png",
  plot = p2,
  width = 12,
  height = 14,
  dpi = 300
)

october_halves <- october_daily |>
  mutate(
    half = if_else(
      day <= 15,
      "First Half",
      "Second Half"
    )
  ) |>
  group_by(year, half) |>
  summarise(
    total_crashes = sum(n_crashes),
    days = n(),
    avg_daily_crashes = total_crashes / days,
    .groups = "drop"
  )

print(october_halves)

october_half_comparison <- october_halves |>
  select(year, half, avg_daily_crashes) |>
  pivot_wider(
    names_from = half,
    values_from = avg_daily_crashes
  ) |>
  mutate(
    percent_difference = 100 *
      (`Second Half` - `First Half`) /
      `First Half`
  )

print(october_half_comparison)

# Compare Halloween with other October days
halloween_comparison <- october_daily |>
  mutate(
    period = if_else(
      day == 31,
      "Halloween",
      "Other October Days"
    )
  ) |>
  group_by(period) |>
  summarise(
    total_crashes = sum(n_crashes),
    days = n(),
    avg_daily_crashes = mean(n_crashes),
    .groups = "drop"
  )

print(halloween_comparison)

halloween_difference <- halloween_comparison |>
  select(period, avg_daily_crashes) |>
  pivot_wider(
    names_from = period,
    values_from = avg_daily_crashes
  ) |>
  mutate(
    percent_difference = 100 *
      (Halloween - `Other October Days`) /
      `Other October Days`
  )

print(halloween_difference)

# Create daily crash counts for October
october_weekdays <- crashes |>
  filter(
    !is.na(crash_date),
    year >= 2016,
    year <= 2025,
    month == 10
  ) |>
  count(crash_date, name = "n_crashes") |>
  mutate(
    year = year(crash_date),
    day = day(crash_date),
    weekday = wday(
      crash_date,
      label = TRUE,
      abbr = TRUE
    ),
    halloween = day == 31
  )

print(october_weekdays)

# Identify the weekday of Halloween each year
halloween_weekdays <- october_weekdays |>
  filter(halloween) |>
  select(
    year,
    halloween_weekday = weekday,
    halloween_crashes = n_crashes
  )

# Compare with matching weekdays
halloween_weekday_comparison <- october_weekdays |>
  left_join(
    halloween_weekdays,
    by = "year"
  ) |>
  filter(
    weekday == halloween_weekday,
    !halloween
  ) |>
  group_by(
    year,
    halloween_weekday,
    halloween_crashes
  ) |>
  summarise(
    comparison_avg = mean(n_crashes),
    .groups = "drop"
  ) |>
  mutate(
    percent_difference = 100 *
      (halloween_crashes - comparison_avg) /
      comparison_avg
  )

print(halloween_weekday_comparison)

p3 <- ggplot(
  halloween_weekday_comparison,
  aes(
    x = factor(year),
    y = percent_difference,
    fill = percent_difference > 0
  )
) +
  geom_col() +
  geom_hline(
    yintercept = 0,
    linetype = "dashed"
  ) +
  scale_fill_manual(
    values = c(
      "FALSE" = "steelblue",
      "TRUE" = "darkorange"
    ),
    guide = "none"
  ) +
  labs(
    title = "Halloween Crash Counts vs. Comparable Weekdays",
    subtitle = "Percentage difference by year, 2016-2025",
    x = "Year",
    y = "Percentage Difference (%)",
    caption = "Source: California Highway Patrol CCRS"
  ) +
  theme_minimal(base_size = 12)

print(p3)

ggsave(
  "outputs/halloween-weekday-comparison.png",
  plot = p3,
  width = 10,
  height = 6,
  dpi = 300
)

# Extract the hour of each crash
crashes_hourly <- crashes |>
  filter(
    !is.na(crash_date_time),
    year >= 2016,
    year <= 2025,
    month == 10
  ) |>
  mutate(
    hour = hour(crash_date_time),
    day = day(crash_date),
    period = case_when(
      hour >= 18 | hour < 6 ~ "Nighttime",
      TRUE ~ "Daytime"
    ),
    halloween = day == 31
  )

hourly_comparison <- crashes_hourly |>
  count(halloween, period, name = "n_crashes") |>
  mutate(
    n_dates = if_else(halloween, 10, 300),
    avg_crashes = n_crashes / n_dates
  )

print(hourly_comparison)

p4 <- ggplot(
  hourly_comparison,
  aes(
    x = period,
    y = avg_crashes,
    fill = halloween
  )
) +
  geom_col(
    position = "dodge"
  ) +
  scale_fill_manual(
    values = c(
      "FALSE" = "steelblue",
      "TRUE" = "darkorange"
    ),
    labels = c(
      "Other October Days",
      "Halloween"
    ),
    name = "Date"
  ) +
  labs(
    title = "Daytime and Nighttime Crashes in October",
    subtitle = "Halloween vs. other October dates, 2016-2025",
    x = "Time of Day",
    y = "Average Crashes per Date",
    caption = "Source: California Highway Patrol CCRS"
  ) +
  theme_minimal(base_size = 12)

print(p4)

ggsave(
  "outputs/halloween-day-night.png",
  plot = p4,
  width = 10,
  height = 6,
  dpi = 300
)


# Compare Halloween and other October dates
# separately for daytime and nighttime

halloween_time_comparison <- hourly_comparison |>
  select(halloween, period, avg_crashes) |>
  pivot_wider(
    names_from = halloween,
    values_from = avg_crashes,
    names_prefix = "halloween_"
  ) |>
  mutate(
    percent_difference = 100 *
      (halloween_TRUE - halloween_FALSE) /
      halloween_FALSE
  )

print(halloween_time_comparison)

# Save results
dir.create(
  "outputs/tables",
  recursive = TRUE,
  showWarnings = FALSE
)

write_csv(
  halloween_time_comparison,
  "outputs/tables/halloween-time-comparison.csv"
)

write_csv(
  halloween_weekday_comparison,
  "outputs/tables/halloween-weekday-comparison.csv"
)

write_csv(
  october_half_comparison,
  "outputs/tables/october-half-comparison.csv"
)

# Pedestrian involved crashes on Halloween
pedestrian_october <- crashes |>
  filter(
    !is.na(crash_date),
    year >= 2016,
    year <= 2025,
    month == 10
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
      ),
    halloween = day(crash_date) == 31
  ) |>
  filter(pedestrian_crash)

pedestrian_halloween <- pedestrian_october |>
  group_by(halloween) |>
  summarise(
    pedestrian_crashes = n(),
    .groups = "drop"
  ) |>
  mutate(
    n_days = if_else(halloween, 10, 300),
    avg_daily_crashes = pedestrian_crashes / n_days
  )

print(pedestrian_halloween)

# Pedestrian crashes Halloween vs matching weekdays

pedestrian_daily <- pedestrian_october |>
  count(crash_date, name = "n_crashes") |>
  complete(
    crash_date = seq(
      as.Date("2016-10-01"),
      as.Date("2025-10-31"),
      by = "day"
    ),
    fill = list(n_crashes = 0)
  ) |>
  filter(month(crash_date) == 10) |>
  mutate(
    year = year(crash_date),
    weekday = wday(
      crash_date,
      label = TRUE,
      abbr = TRUE
    ),
    halloween = day(crash_date) == 31
  )

pedestrian_halloween_weekdays <- pedestrian_daily |>
  filter(halloween) |>
  select(
    year,
    halloween_weekday = weekday,
    halloween_crashes = n_crashes
  )

pedestrian_weekday_comparison <- pedestrian_daily |>
  left_join(
    pedestrian_halloween_weekdays,
    by = "year"
  ) |>
  filter(
    weekday == halloween_weekday,
    !halloween
  ) |>
  group_by(
    year,
    halloween_weekday,
    halloween_crashes
  ) |>
  summarise(
    comparison_avg = mean(n_crashes),
    .groups = "drop"
  ) |>
  mutate(
    percent_difference = 100 *
      (halloween_crashes - comparison_avg) /
      comparison_avg
  )

print(pedestrian_weekday_comparison)

write_csv(
  pedestrian_weekday_comparison,
  "outputs/tables/pedestrian-halloween-weekday-comparison.csv"
)

write_csv(
  pedestrian_halloween,
  "outputs/tables/pedestrian-halloween-crashes-comparison.csv"
)
