# 00_setting.R
#
# SHARED SETTING for the single malaria teaching scenario.
#
# Every generator (masters, genetics, analysis objects, trial) sources this file so
# that all datasets describe the SAME fictional world: geography, population, seasons
# and pathogen are defined once here.
#
# >>> ALL PLACE NAMES ARE FICTIONAL, invented for this teaching module. Any
# >>> resemblance to real places is coincidental. The data are entirely simulated.
#
# Scenario: endemic, seasonal malaria in the fictional Republic of Amani, a
# sub-Saharan setting. Transmission is rainfall-driven (with a lag); disease burden
# and severe outcomes fall most heavily on young children (acquired immunity with
# age); insecticide-treated bednet (ITN) coverage varies by district and reduces
# transmission. Surveillance runs across 12 districts over 5 years (2020-2025).
# ------------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(dplyr); library(tidyr)
})

# ---- global constants --------------------------------------------------------
SCENARIO_SEED  <- 2026L
n_years        <- 5L
weeks_per_year <- 52L
n_weeks        <- n_years * weeks_per_year          # 260
start_date     <- as.Date("2020-01-06")             # a Monday
scenario_t         <- seq_len(n_weeks)
scenario_week_start<- start_date + (scenario_t - 1) * 7
scenario_year      <- 2020L + (scenario_t - 1L) %/% weeks_per_year
scenario_week      <- (scenario_t - 1L) %% weeks_per_year + 1L

# ---- districts (2 are cities/focus sites) ------------------------------------
# region grouping: North, Central, Lakeside, Highlands
.set_seed <- function() set.seed(SCENARIO_SEED)
.set_seed()

districts <- tibble(
  district = c("Lusenga", "Mbarako", "Kanyeru", "Tendaji", "Nyalako", "Rusoma",
               "Kabendo", "Simwera", "Bunyeru", "Kilomba", "Matonga", "Wakisa"),
  region   = c("Central", "Lakeside", "Central", "North", "Lakeside", "Highlands",
               "North", "Central", "Lakeside", "Highlands", "North", "Central"),
  is_city  = c(TRUE, TRUE, rep(FALSE, 10))
) %>%
  mutate(
    population       = round(runif(n(), 40000, 320000)),
    base_rate        = runif(n(), 0.010, 0.030),  # annual base malaria attack scaling
    bednet_coverage  = round(runif(n(), 0.25, 0.80), 2),  # ITN coverage (0-1)
    rain_baseline    = runif(n(), 40, 70),        # mm/week baseline (tropical)
    temp_baseline    = runif(n(), 22, 27)         # deg C annual mean
  )
n_dist <- nrow(districts)

# resistance-monitoring sentinel sites (subset of districts)
sentinel_sites <- c("Lusenga", "Mbarako", "Tendaji")

# ---- health facilities per district (IDs link to the line-list & maps) -------
facilities <- districts %>%
  mutate(n_fac = pmax(2, round(population / 60000))) %>%
  rowwise() %>%
  mutate(fac = list(sprintf("%s_%02d", toupper(substr(district, 1, 3)), seq_len(n_fac)))) %>%
  ungroup() %>%
  select(district, fac) %>%
  unnest(fac) %>%
  rename(facility_id = fac)

# ---- shared helpers ----------------------------------------------------------

# Age distribution of malaria CASES: strong pediatric peak (mode ~4y), adult tail.
sample_case_ages <- function(n) {
  a <- round(rgamma(n, shape = 1.7, scale = 6))
  a[a > 90] <- sample(70:90, sum(a > 90), replace = TRUE)
  pmin(pmax(a, 0), 99)
}

# Severity that DECREASES with age (young children most at risk from malaria).
p_hospitalised <- function(age) plogis(-1.9 - 0.02 * age)
p_icu_if_hosp  <- function(age) plogis(-0.6 - 0.015 * age)
p_death        <- function(age, icu) plogis(-4.7 - 0.03 * age + 1.3 * icu)

message("Setting loaded: Republic of Amani | ", n_dist, " districts | ",
        nrow(facilities), " facilities | ", n_weeks, " weeks")
