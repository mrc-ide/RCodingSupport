# plot_positivity_temp.R
#
# Author: Bob Verity
# Date: 2025-11-17
#
# Inputs:
#   - data_raw/two_cities_cases.rds
#   - data_raw/two_cities_temp.rds
#
# Outputs:
#   (none; this script produces exploratory plots only)
#
# Purpose:
#   Explore two related datasets describing respiratory testing activity
#   and seasonal temperature patterns in two cities.
#
# ------------------------------------------------------------------

library(tidyverse)
library(here)
library(lubridate)

# read in data
dat_cases <- readRDS(here("data_raw", "two_cities_cases.rds"))
dat_temp <- readRDS(here("data_raw", "two_cities_temp.rds"))

# plot positivity over time
dat_cases |>
  ggplot() + theme_bw() +
  geom_point(aes(x = date, y = 100 * cases / tested, col = city)) +
  labs(x = "Time", y = "Test Positivity (%)", colour = "City")

# plot temperature each month
dat_temp |>
  ggplot() + theme_bw() +
  geom_line(aes(x = Month, y = Temp_celcius, group = Year, col = as.factor(Year))) +
  facet_wrap(~City) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "Month", y = "Temperature (Celcius)", colour = "Year")

# basic calculations from case data
dat_cases <- dat_cases |>
  mutate(Positivity = 100 * cases / tested,
         Year = year(date),
         Month = months(date))

# merge datasets
dat_combined <- dat_cases |>
  left_join(dat_temp)

nrow(dat_combined)

# fix names and try again
dat_combined <- dat_cases |>
  rename(City = city) |>
  left_join(dat_temp)

nrow(dat_combined)

# plot positivity against temperature
dat_combined |>
  ggplot() + theme_bw() +
  geom_point(aes(x = Temp_celcius, y = Positivity, col = City)) +
  labs(x = "Temperature (Celcius)", y = "Positivity (%)")

