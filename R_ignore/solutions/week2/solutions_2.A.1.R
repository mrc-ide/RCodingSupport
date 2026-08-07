## ------------------------------------------------------------
## R Coding Support Sessions – Solutions
## 2.A.1 Working with Vectors and Matrices
## ------------------------------------------------------------

# remotes::install_github("mrc-ide/RCodingSupport@v1.0.0")
library(RCodingSupport)

## ------------------------------------------------------------
## Part 1: Working with vectors
## ------------------------------------------------------------

## Task 1: Inspect the weekly incidence data
# Vector of weekly totals over 5 years:
n_weeks <- length(incidence_weekly)
n_weeks

# Create observation-day vector: 1, 8, 15, ... (increments of 7)
obs_day <- seq(from = 1, by = 7, length.out = n_weeks)

# Quick plot
plot(obs_day, incidence_weekly, type = "o", ylim = c(0, 350),
     xlab = "Observation day (weekly)", ylab = "Weekly cases",
     main = "Weekly incidence over 5 years")

## Task 2: Look for outliers relative to first 4 years
weeks_per_year <- 52L
first4_idx <- 1:(4 * weeks_per_year)

mu4 <- mean(incidence_weekly[first4_idx])
sd4 <- sd(incidence_weekly[first4_idx])

lo <- mu4 - 1.96 * sd4
hi <- mu4 + 1.96 * sd4
c(lo = lo, hi = hi)

# Does year 5 exceed these limits?
abline(h = c(lo, hi), col = "red")

## ------------------------------------------------------------
## Part 2: Working with matrices
## ------------------------------------------------------------

## Task 1: Explore the age-disaggregated matrix
dim(incidence_weekly_age)
nrow(incidence_weekly_age)
ncol(incidence_weekly_age)

# Oldest age group (row name of the last row)
oldest_age_group <- tail(rownames(incidence_weekly_age), 1)
oldest_age_group

# Do column sums match weekly totals?
totals_by_col <- colSums(incidence_weekly_age)
all(totals_by_col == incidence_weekly)  # should be TRUE

## Task 2: Plot the age-disaggregated results

# Extract the time series for 20–24-year-olds
# (Row name is "20-24"; if unsure, check rownames())
age_20_24 <- incidence_weekly_age["20-24", ]

# Plot alongside the overall series
plot(obs_day, incidence_weekly, type = "l", ylim = c(0, 350),
     xlab = "Observation day (weekly)", ylab = "Cases",
     main = "All cases vs 20–24 age group")
lines(obs_day, age_20_24, col = 2, lwd = 2)

# Extract cases across ages on week 52
week_idx <- 52L
by_age_week52 <- incidence_weekly_age[, week_idx]

# Plot distribution across ages for week 52
plot(seq_along(by_age_week52), by_age_week52, type = "o", pch = 19,
     xlab = "Age-group index (rows)", ylab = "Cases (week 52)",
     main = "Age distribution of cases (week 52)")
# (Optional: label axes more richly with rownames if desired)

# Image plot of the full matrix (ages x weeks)
# Flip rows for a top-to-bottom youngest->oldest display
image(t(incidence_weekly_age),
      axes = FALSE,
      main = "Cases by age (rows) and week (cols)")

# Add crude axes with labels (may be dense; adjust cex.axis as needed)
axis(1,
     at = seq(0, 1, length.out = ncol(incidence_weekly_age)),
     labels = colnames(incidence_weekly_age),
     las = 2, cex.axis = 0.5)
axis(2,
     at = seq(0, 1, length.out = nrow(incidence_weekly_age)),
     labels = rownames(incidence_weekly_age),
     las = 2, cex.axis = 0.5)

## Prompt for discussion (no code):
## - Do 20–24-year-olds track the overall pattern closely?
## - How concentrated are cases by age in week 52?
## - What visualisation would you choose for a policymaker?
