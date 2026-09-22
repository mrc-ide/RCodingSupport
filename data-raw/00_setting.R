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
# sub-Saharan setting. Transmission is rainfall-driven (with a six-week lag); disease burden
# and severe outcomes fall most heavily on young children (acquired immunity with
# age); insecticide-treated bednet (ITN) coverage varies by district and reduces
# transmission. Surveillance runs across 12 districts over 5 years (2020-2025).
#
# Paths go through here::here(), so every script runs from anywhere in the project.
#
# Every script carries interleaved SANITY CHECKS (plots + summaries) so the
# scenario can be eyeballed as it is built. They are for the person running the
# script; nothing they produce is saved. They never touch the RNG, so the seeded
# outputs are byte-identical with or without them.
# ------------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(ggplot2); library(patchwork); library(here)
})

# ---- sanity-check helpers ----------------------------------------------------
# no stray Rplots.pdf when run via Rscript -- but never clobber a device the
# caller already opened (e.g. pdf("sanity.pdf") to capture every plot to a file)
if (!interactive() && is.null(dev.list())) pdf(NULL)
theme_set(theme_bw(base_size = 9))
peek <- function(title) cat("\n---", title, "---\n")
region_cols <- c(North = "#1b9e77", Central = "#d95f02", Lakeside = "#7570b3", Highlands = "#e7298a")

# the staging area every generator writes to
stage_dir <- Sys.getenv("STAGE_DIR", unset = here("data-raw", "staged_data"))

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
) |>
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

# ---- SANITY: the districts ---------------------------------------------------
peek("districts")
print(as.data.frame(districts), digits = 3)

p_pop <- districts |>
  mutate(district = reorder(district, -population)) |>
  ggplot(aes(district, population / 1000, fill = region)) +
  geom_col() +
  scale_fill_manual(values = region_cols) +
  labs(x = NULL, y = "Population (thousands)", title = "Population by district") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "bottom")

p_knobs <- ggplot(districts, aes(bednet_coverage, base_rate, colour = region)) +
  geom_point(aes(shape = is_city), size = 3) +
  geom_text(aes(label = district), vjust = -1, size = 2.5, show.legend = FALSE) +
  scale_colour_manual(values = region_cols) +
  scale_shape_manual(values = c(`FALSE` = 16, `TRUE` = 17)) +
  expand_limits(y = max(districts$base_rate) * 1.1) +
  labs(x = "ITN coverage", y = "Base attack rate (annual)", shape = "Focus city",
       title = "District risk knobs") +
  theme(legend.position = "bottom")

print(p_pop + p_knobs + plot_annotation(title = "Republic of Amani: the 12 districts"))

# ---- health facilities per district (IDs link to the line-list & maps) -------
facilities <- districts |>
  mutate(n_fac = pmax(2, round(population / 60000))) |>
  rowwise() |>
  mutate(fac = list(sprintf("%s_%02d", toupper(substr(district, 1, 3)), seq_len(n_fac)))) |>
  ungroup() |>
  select(district, fac) |>
  unnest(fac) |>
  rename(facility_id = fac)

peek("facilities per district")
print(table(facilities$district)[districts$district])

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

# ---- SANITY: the age and severity assumptions (deterministic curves) --------
peek("age / severity assumptions")
ages <- 0:90

p_age <- ggplot(data.frame(age = ages), aes(age)) +
  stat_function(fun = dgamma, args = list(shape = 1.7, scale = 6), linewidth = 0.8) +
  geom_vline(xintercept = c(5, 15), linetype = 3) +
  labs(x = "Age (years)", y = "Density", title = "Case age distribution (target)")

p_sev <- tibble(age = ages) |>
  mutate(`P(hospitalised)`  = p_hospitalised(age),
         `P(ICU | hosp)`    = p_icu_if_hosp(age),
         `P(death | no ICU)`= p_death(age, 0),
         `P(death | ICU)`   = p_death(age, 1)) |>
  pivot_longer(-age, names_to = "quantity", values_to = "p") |>
  ggplot(aes(age, 100 * p, colour = quantity)) +
  geom_line(linewidth = 0.8) +
  scale_colour_brewer(palette = "Dark2") +
  labs(x = "Age (years)", y = "Probability (%)", colour = NULL,
       title = "Severity by age") +
  theme(legend.position = "bottom")

print(p_age + p_sev)
cat(sprintf("Implied crude CFR at age 2 (no ICU): %.2f%%; at age 30: %.2f%%; ICU at age 2: %.1f%%\n",
            100 * p_death(2, 0), 100 * p_death(30, 0), 100 * p_death(2, 1)))

message("Setting loaded: Republic of Amani | ", n_dist, " districts | ",
        nrow(facilities), " facilities | ", n_weeks, " weeks")

