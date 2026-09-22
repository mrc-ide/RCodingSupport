
# plot_resistance_trajectories.R
#
# Author: Bob Verity
# Date: 2025-10-29 (updated 2026-09-22 for the Republic of Amani malaria data)
#
# Inputs:
#   - data_raw/resistance_trajectories.rds
#
# Outputs:
#   - none (plot displayed in RStudio)
#
# Purpose:
#   Load simulated data on molecular marker prevalence (kelch13 C469Y and dhps
#   K540E) across the three sentinel sites of the Republic of Amani, and
#   visualise both observed estimates and modelled trends. The figure
#   demonstrates:
#     - use of geom_ribbon() to show model uncertainty bands
#     - geom_pointrange() for observed estimates with 95% CIs
#     - facet_grid() for two-dimensional faceting by site and marker
#     - fixed y-axis limits and basic title/label formatting
#
# ------------------------------------------------------------------

library(here)
library(ggplot2)

# read in data: a list with $data (observed prevalence at sampling dates) and
# $model (credible bands on a weekly grid)
dat <- readRDS(here("data_raw", "resistance_trajectories.rds"))
str(dat, max.level = 1)

# make plot
dat$data |>
  ggplot() + theme_bw() +
  geom_ribbon(aes(x = t, ymin = Q2.5, ymax = Q97.5, fill = marker), data = dat$model, alpha = 0.4) +
  geom_ribbon(aes(x = t, ymin = Q25, ymax = Q75, fill = marker), data = dat$model, alpha = 0.4) +
  geom_pointrange(aes(x = t, y = p_est, ymin = CI_lower, ymax = CI_upper)) +
  geom_hline(yintercept = 10, linetype = "dashed") +
  facet_grid(marker ~ site) +
  xlab("Time") + ylab("Marker prevalence (%)") +
  ggtitle("Drug-resistance marker prevalence at sentinel sites, Republic of Amani") +
  scale_y_continuous(limits = c(0, 100))
