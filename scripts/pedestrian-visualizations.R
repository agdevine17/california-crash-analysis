# Pedestrian Crash Visualizations

library(tidyverse)
library(lubridate)
library(scales)

monthly_pedestrian <- read_csv(
  "outputs/tables/monthly-pedestrian.csv",
  show_col_types = FALSE
) |>
  mutate(
    month_name = factor(
      month.abb[month],
      levels = month.abb
    )
  )

print(monthly_pedestrian)

p1 <- ggplot(
  monthly_pedestrian,
  aes(
    x = month_name,
    y = pedestrian_crashes,
    fill = month == 10
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
  scale_y_continuous(
    labels = comma,
    expand = expansion(mult = c(0, 0.05))
  ) +
  labs(
    title = "Pedestrian-Involved Crashes by Month",
    subtitle = "California, 2016-2025",
    x = "Month",
    y = "Number of Crashes",
    caption = "Source: California Highway Patrol CCRS"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.major.x = element_blank()
  )

ggsave(
  "outputs/pedestrian-crashes-monthly.png",
  plot = p1,
  width = 10,
  height = 6,
  dpi = 300
)

p2 <- ggplot(
  monthly_pedestrian,
  aes(
    x = month_name,
    y = pedestrian_percentage,
    fill = month == 10
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
  scale_y_continuous(
    labels = label_number(suffix = "%"),
    expand = expansion(mult = c(0, 0.05))
  ) +
  labs(
    title = "Percentage of Crashes Involving Pedestrians",
    subtitle = "California, 2016-2025",
    x = "Month",
    y = "Pedestrian-Involved Crashes (%)",
    caption = "Source: California Highway Patrol CCRS"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.major.x = element_blank()
  )

ggsave(
  "outputs/pedestrian-crash-percentage.png",
  plot = p2,
  width = 10,
  height = 6,
  dpi = 300
)

# Calculate average daily pedestrian crashes

days_by_month <- expand_grid(
  year = 2016:2025,
  month = 1:12
) |>
  mutate(
    days = days_in_month(
      make_date(year, month, 1)
    )
  ) |>
  group_by(month) |>
  summarise(
    total_days = sum(days),
    .groups = "drop"
  )

monthly_pedestrian <- monthly_pedestrian |>
  left_join(
    days_by_month,
    by = "month"
  ) |>
  mutate(
    avg_daily_pedestrian =
      pedestrian_crashes / total_days
  )

print(
  monthly_pedestrian |>
    select(
      month_name,
      pedestrian_crashes,
      total_days,
      avg_daily_pedestrian
    ),
  n = Inf
)

# Average Daily Pedestrian-Involved Crashes

p3 <- ggplot(
  monthly_pedestrian,
  aes(
    x = month_name,
    y = avg_daily_pedestrian,
    fill = month == 10
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
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.05))
  ) +
  labs(
    title = "Average Daily Pedestrian-Involved Crashes",
    subtitle = "California, 2016-2025",
    x = "Month",
    y = "Average Number of Crashes per Day",
    caption = "Source: California Highway Patrol CCRS"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.major.x = element_blank()
  )

ggsave(
  "outputs/pedestrian-crashes-daily-average.png",
  plot = p3,
  width = 10,
  height = 6,
  dpi = 300
)

pedestrian_halloween <- read_csv(
  "outputs/tables/pedestrian-halloween-crashes-comparison.csv",
  show_col_types = FALSE
)

pedestrian_weekday_comparison <- read_csv(
  "outputs/tables/pedestrian-halloween-weekday-comparison.csv",
  show_col_types = FALSE
)

# Graph 4: Halloween vs. other October dates

p4 <- pedestrian_halloween |>
  mutate(
    period = if_else(
      halloween,
      "Halloween",
      "Other October Days"
    ),
    period = factor(
      period,
      levels = c("Other October Days", "Halloween")
    )
  ) |>
  ggplot(
    aes(
      x = period,
      y = avg_daily_crashes,
      fill = period
    )
  ) +
  geom_col(width = 0.6) +
  geom_text(
    aes(
      label = round(avg_daily_crashes, 1)
    ),
    vjust = -0.5,
    size = 4
  ) +
  scale_fill_manual(
    values = c(
      "Other October Days" = "steelblue",
      "Halloween" = "darkorange"
    ),
    guide = "none"
  ) +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.12))
  ) +
  labs(
    title = "Average Daily Pedestrian-Involved Crashes",
    subtitle = "Halloween vs. Other October Dates, 2016-2025",
    x = NULL,
    y = "Average Number of Crashes per Day",
    caption = "Source: California Highway Patrol CCRS"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.major.x = element_blank()
  )

ggsave(
  "outputs/pedestrian-halloween-average.png",
  plot = p4,
  width = 9,
  height = 6,
  dpi = 300
)

# Halloween vs. comparable weekdays

p5 <- ggplot(
  pedestrian_weekday_comparison,
  aes(
    x = factor(year),
    y = percent_difference,
    fill = percent_difference > 0
  )
) +
  geom_col(width = 0.7) +
  geom_hline(
    yintercept = 0,
    linetype = "dashed"
  ) +
  geom_text(
    aes(
      label = paste0(
        round(percent_difference, 1),
        "%"
      )
    ),
    vjust = if_else(
      pedestrian_weekday_comparison$percent_difference > 0,
      -0.5,
      1.5
    ),
    size = 3.5
  ) +
  scale_fill_manual(
    values = c(
      "FALSE" = "steelblue",
      "TRUE" = "darkorange"
    ),
    guide = "none"
  ) +
  scale_y_continuous(
    labels = label_number(suffix = "%"),
    expand = expansion(mult = c(0.15, 0.15))
  ) +
  labs(
    title = "Halloween Pedestrian Crashes vs. Comparable Weekdays",
    subtitle = "Percentage Difference by Year, 2016-2025",
    x = "Year",
    y = "Percentage Difference (%)",
    caption = "Source: California Highway Patrol CCRS"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.major.x = element_blank()
  )

ggsave(
  "outputs/pedestrian-halloween-weekday-comparison.png",
  plot = p5,
  width = 11,
  height = 6,
  dpi = 300
)
