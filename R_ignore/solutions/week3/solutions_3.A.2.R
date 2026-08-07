# 3A2_solution_sourcing_script.R
# ------------------------------------------------------------
# Purpose: Source an external script, use its function on a
#          weekly incidence vector, and visualise results.
# Based on: Problem set "3.A.2 Sourcing an R Script"
# ------------------------------------------------------------

## 1) Load the teaching package -----------------------------------------------
# remotes::install_github("mrc-ide/RCodingSupport@v1.0.0")
library(RCodingSupport)   # provides 'incidence_weekly' vector

## 2) Source the helper script ------------------------------------------------
# The file should live at: source_scripts/moving_average.R
source("source_scripts/moving_average.R")

## 3) Apply the function with several window sizes ----------------------------
ma_k3 <- calc_moving_average(incidence_weekly, window_size = 3)
ma_k5 <- calc_moving_average(incidence_weekly, window_size = 5)
ma_k7 <- calc_moving_average(incidence_weekly, window_size = 7)

## 4) Plot raw vs smoothed series ---------------------------------------------
plot(
  incidence_weekly, type = "l",
  xlab = "Week", ylab = "Incidence (cases)",
  main = "Weekly incidence: raw vs moving averages"
)
lines(ma_k3, col = "red",   lwd = 2)
lines(ma_k5, col = "blue",  lwd = 2)
lines(ma_k7, col = "green", lwd = 2)
legend(
  "topleft",
  legend = c("Raw", "MA k=3", "MA k=5", "MA k=7"),
  col    = c("black", "red", "blue", "green"),
  lty    = 1, lwd = c(1,2,2,2), bty = "n"
)

