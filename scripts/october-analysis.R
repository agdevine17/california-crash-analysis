
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
