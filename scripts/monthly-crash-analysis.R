library(tidyverse)
library(lubridate)
library(scales)

crashes <- readRDS("data/processed/crashes_combined.rds")

# Create a table with monthly crash counts by year
monthly_counts <- crashes |>
  filter(
    !is.na(crash_date),
    year >= 2016,
    year <= 2025
  ) |>
  count(year, month, month_name, name = "n_crashes") |>
  arrange(year, month)

# Check that all 120 months are represented
print(nrow(monthly_counts))
print(monthly_counts)

# Identify the month with the most crashes each year
highest_month_by_year <- monthly_counts |>
  group_by(year) |>
  slice_max(
    order_by = n_crashes,
    n = 1,
    with_ties = TRUE
  ) |>
  ungroup() |>
  arrange(year)

print(highest_month_by_year)

october_highest <- highest_month_by_year |>
  group_by(year) |>
  summarise(
    october_highest = any(month == 10),
    .groups = "drop"
  ) |>
  summarise(
    years_october_highest = sum(october_highest),
    total_years = n(),
    percentage = 100 * mean(october_highest)
  )

print(october_highest)

# Rank months by crash count within each year
monthly_rankings <- monthly_counts |>
  group_by(year) |>
  mutate(
    rank = min_rank(desc(n_crashes))
  ) |>
  ungroup() |>
  arrange(year, rank)

# View October's ranking each year
october_rankings <- monthly_rankings |>
  filter(month == 10) |>
  select(year, month_name, n_crashes, rank)

print(october_rankings)

october_rank_summary <- october_rankings |>
  summarise(
    average_rank = mean(rank),
    median_rank = median(rank),
    best_rank = min(rank),
    worst_rank = max(rank)
  )

print(october_rank_summary)

# Calculate average daily crash counts
monthly_daily <- monthly_counts |>
  mutate(
    days_in_month = days_in_month(
      make_date(year, month, 1)
    ),
    crashes_per_day = n_crashes / days_in_month
  )

print(monthly_daily)

daily_by_month <- monthly_daily |>
  group_by(month, month_name) |>
  summarise(
    total_crashes = sum(n_crashes),
    total_days = sum(days_in_month),
    crashes_per_day = total_crashes / total_days,
    .groups = "drop"
  ) |>
  arrange(desc(crashes_per_day))

print(daily_by_month)

highest_daily_by_year <- monthly_daily |>
  group_by(year) |>
  slice_max(
    order_by = crashes_per_day,
    n = 1,
    with_ties = TRUE
  ) |>
  ungroup()

print(highest_daily_by_year)

# Compare October to the average of other months
october_comparison <- monthly_counts |>
  group_by(year) |>
  summarise(
    october_crashes = n_crashes[month == 10],
    other_month_avg = mean(n_crashes[month != 10]),
    percent_difference = 100 *
      (october_crashes - other_month_avg) /
      other_month_avg,
    .groups = "drop"
  )

print(october_comparison)

october_share <- monthly_counts |>
  group_by(year) |>
  mutate(
    annual_total = sum(n_crashes),
    monthly_share = 100 * n_crashes / annual_total
  ) |>
  ungroup() |>
  filter(month == 10) |>
  mutate(
    expected_share = 100 * 31 /
      if_else(leap_year(year), 366, 365),
    excess_percentage_points =
      monthly_share - expected_share
  ) |>
  select(
    year,
    n_crashes,
    annual_total,
    monthly_share,
    expected_share,
    excess_percentage_points
  )

print(october_share)

dir.create(
  "outputs/tables",
  recursive = TRUE,
  showWarnings = FALSE
)

# Save monthly crash counts
write_csv(
  monthly_counts,
  "outputs/tables/monthly-counts.csv"
)

# Save highest crash month for each year
write_csv(
  highest_month_by_year,
  "outputs/tables/highest-month-by-year.csv"
)

# Save October highest month summary
write_csv(
  october_highest,
  "outputs/tables/october-highest-summary.csv"
)

# Save monthly rankings
write_csv(
  monthly_rankings,
  "outputs/tables/monthly-rankings.csv"
)

# Save October rankings
write_csv(
  october_rankings,
  "outputs/tables/october-rankings.csv"
)

# Save daily-adjusted monthly counts
write_csv(
  monthly_daily,
  "outputs/tables/monthly-daily-counts.csv"
)

# Save aggregated daily crash counts
write_csv(
  daily_by_month,
  "outputs/tables/daily-crashes-by-month.csv"
)

# Save highest daily crash month by year
write_csv(
  highest_daily_by_year,
  "outputs/tables/highest-daily-month-by-year.csv"
)

# Save October comparisons
write_csv(
  october_comparison,
  "outputs/tables/october-comparison.csv"
)

# Save October annual shares
write_csv(
  october_share,
  "outputs/tables/october-annual-share.csv"
)

write_csv(
  october_rank_summary,
  "outputs/tables/october-rank-summary.csv"
)

print("All analysis tables saved successfully!")