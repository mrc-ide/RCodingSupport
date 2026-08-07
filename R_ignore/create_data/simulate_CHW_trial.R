# simulate_CHW_trial.R
#
# Author: Bob Verity
# Date: 2025-10-29
#
# Inputs: (none)
#
# Saved files:
#   - R_ignore/non_package_data/CHW_trial.rds
#
# Purpose:
#   Generate a synthetic dataset for a Community Health Worker (CHW) trial,
#   used for teaching ggplot data visualisation and factor handling.
#
#   The trial compares an Intervention group vs. a Control group across two
#   settings with differing levels of service availability:
#     - "Baseline services (lower)"
#     - "Enhanced services (higher)"
#
#   Data are simulated at the individual level and summarised to group means
#   with 95% confidence intervals. Intended learning objectives:
#     - Demonstrate how ggplot handles character vs. factor ordering
#     - Show the use of dodging and error bars in grouped bar plots
#     - Provide a clean, minimal example for intermediate plotting exercises
#
# ------------------------------------------------------------------

# --- Setup ----
library(tidyverse)
set.seed(1)

# Two conditions (character): alphabetical order will put "Baseline..." before "Enhanced..."
conditions_chr <- c("Baseline services (lower)", "Enhanced services (higher)")
groups <- c("Intervention group", "Control group")

# --- 1) Generate synthetic RAW data (no missing cells) ----
raw <- crossing(condition = conditions_chr, group = groups) %>%
  mutate(n_per_cell = sample(c(20, 28, 36), size = n(), replace = TRUE)) %>%
  rowwise() %>%
  mutate(
    # Make the Enhanced condition higher than Baseline for both groups
    base_mu = case_when(
      group == "Intervention group" ~ 3.2,
      TRUE                          ~ 2.8
    ),
    # Add a bump for the Enhanced condition
    mu = base_mu + if_else(condition == "Enhanced services (higher)", 1.0, 0.0),
    sd = 0.6,
    y  = list(rnorm(n_per_cell, mu, sd))
  ) %>%
  unnest(y) %>%
  ungroup()

# --- 2) Summarise to MEAN + 95% CI, keep a minimal plotting frame ----
df_plot <- raw %>%
  group_by(condition, group) %>%
  summarise(
    mean = mean(y),
    n = n(),
    sd = sd(y),
    se = sd / sqrt(n),
    tcrit = qt(0.975, df = pmax(n - 1, 1)),
    ci_lower = mean - tcrit * se,
    ci_upper = mean + tcrit * se,
    .groups = "drop"
  ) %>%
  dplyr::select(condition, group, mean, ci_lower, ci_upper)

# Keep group ordering stable (left/right slot)
df_plot <- df_plot %>%
  mutate(group = factor(group, levels = c("Intervention group", "Control group")))

# --- 3) Save to file ----
saveRDS(df_plot, file = here::here("R_ignore", "non_package_data", "CHW_trial.rds"))
