# simulate_CFR_debugging.R
#
# Author: Bob Verity
# Date: 2025-11-25
#
# Purpose:
#   Simulate three small datasets for debugging exercises in Week 8.
#   These datasets are stored together in a single .RData object:
#
#   - study_A: clean, valid data (equal lengths, no NAs)
#   - study_B: mismatched lengths (to trigger length checks)
#   - study_C: contains NAs (to encourage warnings and safe behaviour)
#
# Output:
#   - data_raw/CFR_debugging.RData
#
# ------------------------------------------------------------------

library(tidyverse)
library(here)

# ---------------------------
# study_A: clean data
# ---------------------------

study_A <- tibble(
  deaths = c(4, 5, 0, 3),
  cases  = c(200, 150, 120, 50)
)

# ---------------------------
# study_B: mismatched lengths
# ---------------------------

study_B <- list(
  deaths = c(6, 7),                     # length 2
  cases  = c(100, 120, 80, 60)             # length 4  → mismatch
)

# ---------------------------
# study_C: contains NAs
# ---------------------------

study_C <- tibble(
  deaths = c(4, NA, 1, 0),
  cases  = c(200, 150, NA, 80)
)

# ---------------------------
# save to a single RData file
# ---------------------------

save(study_A, study_B, study_C,
     file = here("R_ignore/non_package_data", "CFR_debugging.RData"))

