# solutions_4.A.2.R
#
# Author: Bob Verity
# Date: 2025-10-14
#
# Inputs:
#   - posterior_density.rds
#
# Outputs:
#   (none)
#
# Purpose:
#   Load posterior draws, produce a base image plot and equivalent ggplot raster plot.
# --------------------------------------------------------------------

library(here)

# -------------------------------
# Base R plot (your original code)
# -------------------------------

# load data on posterior draws
df_draws <- readRDS(here("R_ignore", "non_package_data", "posterior_density.rds"))

# make an image plot
image(
  df_draws$mu, df_draws$sigma, df_draws$posterior_density,
  xlab  = "mu",
  ylab  = "sigma",
  main  = "Posterior density"
)

# add contour lines
contour(
  df_draws$mu, df_draws$sigma, df_draws$posterior_density,
  add = TRUE,
  nlevels = 10,
  drawlabels = FALSE
)

# -------------------------------
# ggplot2 version (solution)
# -------------------------------
library(ggplot2)

# Convert the regular grid (x-by-y matrix) to a long data.frame for ggplot
grid_df <- expand.grid(mu = df_draws$mu, sigma = df_draws$sigma)
grid_df$posterior_density <- as.vector(df_draws$posterior_density)

# Heatmap with contours, colorblind-friendly scale
ggplot(grid_df, aes(x = mu, y = sigma)) +
  geom_raster(aes(fill = posterior_density)) +
  geom_contour(aes(z = posterior_density), color = "white", linewidth = 0.3, bins = 10, alpha = 0.7) +
  #scale_fill_viridis_c(option = "magma") +
  #scale_fill_viridis_c(option = "turbo") +
  #scale_fill_distiller(palette = "Spectral") +
  scale_fill_gradient(low = "white", high = "darkred") +
  xlab("mu") + ylab("sigma") + ggtitle("Posterior density") +
  scale_x_continuous(limits = c(-5, 5), expand = c(0, 0)) +
  scale_y_continuous(limits = c(-5, 5), expand = c(0, 0)) +
  theme_minimal()

# explore appearance to people with colour blindness
colorBlindness::cvdPlot()
