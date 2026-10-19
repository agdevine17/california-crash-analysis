
library(tidyverse)
library(data.table)
library(janitor)

# Read the first crash file
crashes_2016 <- fread(
  "data/raw/crashes_2016.csv",
  nrows = 1000
) |>
  clean_names()

# Inspect structure
glimpse(crashes_2016)

# View column names
names(crashes_2016)

# Preview observations
head(crashes_2016)

# Check missing values
colSums(is.na(crashes_2016))
