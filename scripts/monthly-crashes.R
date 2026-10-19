library(tidyverse)
library(lubridate)
library(scales)

crashes <- readRDS("data/processed/crashes_combined.rds")

monthly_crashes <- crashes |>
  filter(
    !is.na(crash_date),
    year >= 2016,
    year <= 2025
  ) |>
  count(month, month_name, name = "n_crashes") |>
  arrange(month)

print(monthly_crashes)

p1 <- ggplot(
  monthly_crashes,
  aes(x = month_name, y = n_crashes)
) +
  geom_col(fill = "steelblue") +
  scale_y_continuous(
    labels = comma,
    expand = expansion(mult = c(0, 0.05))
  ) +
  labs(
    title = "California Traffic Crashes by Month",
    subtitle = "Aggregated across 2016-2025",
    x = "Month",
    y = "Number of Crashes",
    caption = "Source: California Highway Patrol CCRS"
  ) +
  theme_minimal(base_size = 13)

ggsave(
  filename = "outputs/monthly-crashes.png",
  plot = p1,
  width = 10,
  height = 6,
  dpi = 300
)


# Count crashes for each year-month
crashes_by_year_month <- crashes |>
  filter(
    !is.na(crash_date),
    year >= 2016,
    year <= 2025
  ) |>
  mutate(
    year_month = floor_date(crash_date, "month")
  ) |>
  count(year_month, name = "n_crashes") |>
  arrange(year_month)

# Create bar chart
p2 <- ggplot(
  crashes_by_year_month,
  aes(
    x = year_month,
    y = n_crashes,
    fill = month(year_month) == 10
  )
) +
  geom_col(width = 25) +
  scale_fill_manual(
    values = c("FALSE" = "steelblue",
               "TRUE" = "darkorange"),
    guide = "none"
  ) +
  geom_vline(
    xintercept = as.Date(
      paste0(2017:2025, "-01-01")
    ),
    linetype = "dashed",
    color = "gray60",
    linewidth = 0.3
  ) +
  scale_x_date(
    date_breaks = "1 month",
    date_labels = "%b",
    expand = expansion(add = 15)
  ) +
  scale_y_continuous(
    labels = comma,
    expand = expansion(mult = c(0, 0.05))
  ) +
  labs(
    title = "Monthly California Traffic Crashes",
    subtitle = "January 2016 - December 2025",
    x = "Month",
    y = "Number of Crashes",
    caption = "Source: California Highway Patrol CCRS"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(
      angle = 90,
      hjust = 1,
      vjust = 0.5,
      size = 6
    ),
    panel.grid.major.x = element_blank()
  )

# Save figure
ggsave(
  "outputs/crashes-timeline.png",
  plot = p2,
  width = 20,
  height = 7,
  dpi = 300
)

# Count crashes by month within each year
crashes_by_year_month_facet <- crashes |>
  filter(
    !is.na(crash_date),
    year >= 2016,
    year <= 2025
  ) |>
  count(year, month, month_name, name = "n_crashes") |>
  arrange(year, month)

# Check the dataframe
print(crashes_by_year_month_facet)


# Create faceted monthly crash bar chart
p3 <- ggplot(
  crashes_by_year_month_facet,
  aes(
    x = month_name,
    y = n_crashes,
    fill = month == 10
  )
) +
  geom_col() +
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
  scale_y_continuous(
    labels = comma,
    expand = expansion(mult = c(0, 0.05))
  ) +
  labs(
    title = "Monthly California Traffic Crashes by Year",
    subtitle = "2016-2025",
    x = "Month",
    y = "Number of Crashes",
    caption = "Source: California Highway Patrol CCRS"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(
      angle = 45,
      hjust = 1,
      size = 8
    ),
    strip.text = element_text(
      face = "bold",
      size = 12
    ),
    panel.grid.major.x = element_blank()
  )

ggsave(
  filename = "outputs/monthly-crashes-by-year-facet.png",
  plot = p3,
  width = 12,
  height = 14,
  dpi = 300
)
