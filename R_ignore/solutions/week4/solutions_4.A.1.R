# solutions_4.A.1.R
#
# Author: Bob Verity
# Date: 2025-10-14
#
# Inputs:
#   - R_ignore/non_package_data/two_cities_cases.rds
#
# Outputs:
#   - outputs/positivity_two_cities.png  (image)
#   - outputs/positivity_two_cities_plot.rds  (saved ggplot object)
#
# Purpose:
#   Load two-city testing data, compute test-positivity, build a basic ggplot
#   (lines + points), improve presentation, and save both an image and the
#   plot object.
# --------------------------------------------------------------------

library(here)
library(ggplot2)
library(dplyr)

# ---- Task 1: Load & explore ----
dat <- readRDS(here("R_ignore", "non_package_data", "two_cities_cases.rds"))

str(dat)
head(dat)

# Create prevalence = cases / tested
dat$prevalence <- dat$cases / dat$tested

# Quick range check (should be within [0, 1])
range(dat$prevalence)

# ---- Task 2: First ggplot (lines + points, colour by city) ----
p <- ggplot(dat, aes(x = date, y = prevalence)) +
  geom_line(aes(colour = city)) +
  geom_point(aes(colour = city), size = 1.4)

# Print to preview
p

# ---- Task 3: Improve presentation (labels, limits, theme) ----
p_final <- p +
  xlab("Date") +
  ylab("Prevalence") +
  ggtitle("Test positivity over time by city") +
  ylim(0, 0.25) +
  theme_bw()

p_final

# ---- Task 4: Save as image AND as an R object ----

# Check class of the saved object in-memory
class(p_final)

# Save high-resolution PNG
# ggsave(
#   filename = here("outputs", "positivity_two_cities.png"),
#   plot     = p_final,
#   width    = 8, height = 4.5, dpi = 300
# )

# Save the ggplot object to RDS
# saveRDS(p_final, file = here("outputs", "positivity_two_cities_plot.rds"))

