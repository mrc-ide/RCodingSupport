
# plot_DR_frequency.R
#
# Author: Bob Verity
# Date: 2025-10-29
#
# Inputs:
#   - R_ignore/non_package_data/DR_frequency.rds
#
# Outputs:
#   - none (plot displayed in RStudio)
#
# Purpose:
#   Load simulated data on molecular marker prevalence (k13 A675V and dhps K540E)
#   across multiple Ugandan sites, and visualise both observed estimates and
#   modelled posterior trends. The figure demonstrates:
#     - use of geom_ribbon() to show model uncertainty bands
#     - geom_pointrange() for observed estimates with 95% CIs
#     - facet_grid() for two-dimensional faceting by site and mutation
#     - fixed y-axis limits and basic title/label formatting
#
# ------------------------------------------------------------------

# read in data
dat <- readRDS(here("R_ignore", "non_package_data", "DR_frequency.rds"))

# make plot
dat$data |>
  ggplot() + theme_bw() +
  geom_ribbon(aes(x = t, ymin = Q2.5, ymax = Q97.5, fill = mutation), data = dat$model, alpha = 0.4) +
  geom_ribbon(aes(x = t, ymin = Q25, ymax = Q75, fill = mutation), data = dat$model, alpha = 0.4) +
  geom_pointrange(aes(x = t, y = p_est, ymin = CI_lower, ymax = CI_upper)) +
  geom_hline(yintercept = 10, linetype = "dashed") +
  facet_grid(mutation ~ pop) +
  xlab("Time") + ylab("Allele Frequency") +
  ggtitle("Estimated Allele Frequencies in Ugandan Field Sites") +
  scale_y_continuous(limits = c(0, 100))
