# simulate_two_cities_cases.R
#
# Author: Bob Verity
# Date: 2025-10-08
#
# Inputs:
#   - district_weekly_list (from the RCodingSupport package)
#
# Outputs:
#   - R_ignore/non_package_data/two_cities_cases.rds
#
# Purpose:
#   Combine and reformat weekly case data from two simulated districts
#   ("A" and "B") to create a realistic dataset of case counts from two
#   cities ("Riverton" and "Stonebridge"). Generates a continuous time
#   series, filters to recent years, and saves the combined dataset for
#   plotting exercises in Week 4.
#
# ------------------------------------------------------------------

library(tidyverse)
library(lubridate)
library(dplyr)
library(here)

# read in existing district data and rename cities
df_A <- district_weekly_list$A |>
  rename(tested = tested_A,
         cases = cases_A) |>
  mutate(city = "Riverton")

df_B <- district_weekly_list$B |>
  rename(tested = tested_B,
         cases = cases_B) |>
  mutate(city = "Stonebridge")

# combine and filter
df_combined <- df_A |>
  bind_rows(df_B) |>
  mutate(date = as.Date(sprintf("%s-01-01", year)) + (week - 1)*7,
         city = factor(city, levels = c("Riverton", "Stonebridge"))) |>
  select(city, date, tested, cases) |>
  filter(date > as.Date("2016-01-01"))

# quick plot
df_combined |>
  ggplot() + theme_bw() +
  geom_line(aes(x = date, y = cases / tested, color = city))

# save to file
saveRDS(df_combined, file = here("R_ignore/non_package_data", "two_cities_cases.rds"))
