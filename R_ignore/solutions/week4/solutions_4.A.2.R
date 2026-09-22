# solutions_4.A.2.R
#
# Author: Bob Verity
# Date: 2025-10-14 (updated 2026-09-22 for the Republic of Amani malaria data)
#
# Inputs:
#   - data_raw/posterior_density.rds
#
# Outputs:
#   (none)
#
# Purpose:
#   Load a 2D posterior density surface over two transmission-model parameters
#   (R0 and the case reporting rate), produce a base image plot and an
#   equivalent ggplot raster plot.
# --------------------------------------------------------------------

library(here)

# -------------------------------
# Base R plot (your original code)
# -------------------------------

# load the posterior density surface: a list with the x grid (R0), the y grid
# (reporting_rate) and a matrix of densities
post <- readRDS(here("data_raw", "posterior_density.rds"))
str(post)

# make an image plot
image(
  post$R0, post$reporting_rate, post$density,
  xlab  = "R0",
  ylab  = "Reporting rate",
  main  = "Posterior density"
)

# add contour lines
contour(
  post$R0, post$reporting_rate, post$density,
  add = TRUE,
  nlevels = 10,
  drawlabels = FALSE
)

# -------------------------------
# ggplot2 version (solution)
# -------------------------------
library(ggplot2)

# Convert the regular grid (x-by-y matrix) to a long data.frame for ggplot.
# expand.grid() varies its first argument fastest, which matches how R stores
# a matrix column-by-column, so as.vector() lines the densities up correctly.
grid_df <- expand.grid(R0 = post$R0, reporting_rate = post$reporting_rate)
grid_df$density <- as.vector(post$density)

# Heatmap with contours, colorblind-friendly scale
ggplot(grid_df, aes(x = R0, y = reporting_rate)) +
  geom_raster(aes(fill = density)) +
  geom_contour(aes(z = density), color = "white", linewidth = 0.3, bins = 10, alpha = 0.7) +
  #scale_fill_viridis_c(option = "magma") +
  #scale_fill_viridis_c(option = "turbo") +
  #scale_fill_distiller(palette = "Spectral") +
  scale_fill_gradient(low = "white", high = "darkred") +
  xlab("R0") + ylab("Reporting rate") + ggtitle("Posterior density") +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme_minimal()

# explore appearance to people with colour blindness
colorBlindness::cvdPlot()
