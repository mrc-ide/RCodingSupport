# simulate_regions_incidence.R
#
# Author: Bob Verity
# Date: 2025-11-24
#
# Purpose:
#   Simulate a simple dataset containing:
#     - 10 regions
#     - population size for each region
#     - weekly case counts
#
# ------------------------------------------------------------------

library(tidyverse)

set.seed(123)

# ---- 1. Define regions ----
regions <- c(
  "Northshore",
  "Eastmoor",
  "Riverside",
  "Hillcrest",
  "Southwick",
  "Brookfield",
  "Kingswell",
  "Marston Vale",
  "Westford",
  "Greystone"
)

# ---- 2. Simulate population sizes ----
# Mix of small and large populations (e.g., rural vs urban)
population <- round(runif(10, min = 40000, max = 300000))

# ---- 3. Simulate cases per region ----
# Introduce slight correlation: more populous regions tend to have more cases
baseline_rate <- runif(10, min = 0.0005, max = 0.002)  # incidence rates
cases <- rpois(10, lambda = population * baseline_rate)

# ---- 4. Make final dataset ----
dat_regions <- tibble(
  region = regions,
  population = population,
  cases = cases
)

# ---- 5. Optional: save for problem sets ----
saveRDS(dat_regions, here("R_ignore", "non_package_data", "regions_incidence.rds"))

dat_regions
