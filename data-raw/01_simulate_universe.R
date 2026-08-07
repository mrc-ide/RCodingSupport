# 01_simulate_universe.R
#
# MASTER GENERATOR for the malaria scenario. Sources 00_setting.R and produces the
# objects that (almost) every other teaching dataset is derived from:
#
#   case_linelist       individual-level line-list of reported malaria cases (master)
#   environment_weekly  weekly rainfall + temperature per district (the driver)
#   testing_weekly      weekly tests + confirmed cases per district (denominator)
#
# Malaria specifics: rainfall drives transmission with a ~3-week lag; ITN (bednet)
# coverage reduces transmission; cases peak in young children; severe disease, ICU
# admission and death are concentrated in the young.
#
# Seeded. Writes .rds to STAGE_DIR (falls back to ./staged_data). Does not touch
# live package data — integration is a separate, explicit step.
# ------------------------------------------------------------------------------

source("00_setting.R")   # run from the data-raw/ directory

stage_dir <- Sys.getenv("STAGE_DIR", unset = file.path(getwd(), "staged_data"))
dir.create(stage_dir, showWarnings = FALSE, recursive = TRUE)
message("Writing master objects to: ", stage_dir)

.set_seed()

# ------------------------------------------------------------------------------
# 1) Environmental series per district-week: rainfall (seasonal) + temperature
# ------------------------------------------------------------------------------
environment_weekly <- districts %>%
  select(district, rain_baseline, temp_baseline) %>%
  crossing(tibble(t = scenario_t, year = scenario_year,
                  week = scenario_week, week_start = scenario_week_start)) %>%
  arrange(district, t) %>%
  mutate(
    rainfall_mm = pmax(0, rain_baseline +
                         34 * sin(2 * pi * (t - 10) / weeks_per_year) +
                         rnorm(n(), 0, 12)),
    temp_c = temp_baseline + 3.5 * sin(2 * pi * (t - 30) / weeks_per_year) +
      rnorm(n(), 0, 1.0)
  )

# ------------------------------------------------------------------------------
# 2) Expected weekly cases: lagged rainfall x seasonality x (1 - bednet effect)
# ------------------------------------------------------------------------------
true_lag <- 3L
bednet_effect <- 0.6   # proportional reduction in transmission at full ITN coverage

env_model <- environment_weekly %>%
  left_join(select(districts, district, population, base_rate, bednet_coverage),
            by = "district") %>%
  group_by(district) %>%
  arrange(t, .by_group = TRUE) %>%
  mutate(
    rain_lag = lag(rainfall_mm, n = true_lag, default = 0),
    seasonal = 1 + 0.6 * sin(2 * pi * (t - 6) / weeks_per_year),
    lambda   = population * (base_rate / weeks_per_year) * seasonal *
      exp(0.006 * (rain_lag - mean(rainfall_mm))) *
      (1 - bednet_effect * bednet_coverage),
    cases_expected = pmax(lambda, 0.1),
    cases          = rpois(n(), cases_expected)
  ) %>%
  ungroup()

cat(sprintf("Simulated %d malaria cases over %d district-weeks (mean %.0f/week nationally)\n",
            sum(env_model$cases), nrow(env_model), sum(env_model$cases) / n_weeks))

# ------------------------------------------------------------------------------
# 3) Testing denominator per district-week (tested >= cases)
# ------------------------------------------------------------------------------
testing_weekly <- env_model %>%
  transmute(
    district, year, week, week_start, cases,
    positivity = pmin(pmax(rbeta(n(), 5, 20), 0.05), 0.8),
    tested     = pmax(cases, round(cases / positivity))
  ) %>%
  select(district, year, week, week_start, tested, cases)

# ------------------------------------------------------------------------------
# 4) Expand weekly district counts into an individual-level line-list
# ------------------------------------------------------------------------------
base <- env_model %>%
  filter(cases > 0) %>%
  select(district, t, year, week, week_start, cases) %>%
  tidyr::uncount(cases)
N <- nrow(base)

# --- dates: onset within the ISO week, report after a short delay ---
onset_date  <- base$week_start + sample(0:6, N, replace = TRUE)
report_date <- onset_date + pmin(rpois(N, 3), 21)

# --- age: pediatric peak (malaria) ---
age <- sample_case_ages(N)
age_group <- cut(age, breaks = seq(0, 100, by = 5), right = FALSE,
                 labels = paste0(seq(0, 95, 5), "-", seq(4, 99, 5)))

# --- sex ---
sex <- sample(c("Male", "Female"), N, replace = TRUE, prob = c(0.50, 0.50))

# --- facility within the case's district (vectorised) ---
fac_by_dist <- split(facilities$facility_id, facilities$district)
facility_id <- character(N)
for (d in names(fac_by_dist)) {
  idx <- which(base$district == d)
  facility_id[idx] <- sample(fac_by_dist[[d]], length(idx), replace = TRUE)
}

# --- demographics for the cross-sectional patient_records view ---
# Height: adults ~ Normal by sex; children follow a saturating growth curve that
# rises from ~52 cm at birth towards the adult mean (approx WHO-shaped).
adult_mean_ht <- ifelse(sex == "Male", 176, 164)
height_cm <- ifelse(age >= 18,
                    round(rnorm(N, adult_mean_ht, 7)),
                    round(adult_mean_ht - (adult_mean_ht - 52) * exp(-age / 7) + rnorm(N, 0, 2)))
# BMI: lower in children than adults; weight derived from height and BMI.
bmi_mean <- ifelse(age < 18, 16.5, 22)
bmi <- pmin(pmax(rnorm(N, bmi_mean, 2.5), 12), 38)
weight_kg <- round(bmi * (height_cm / 100)^2)
# Ethnicity: fictional group names invented for the Republic of Amani (a categorical
# demographic variable for teaching; not intended to represent any real group).
ethnicity <- sample(c("Kesi", "Roha", "Nuwa", "Saru", "Other"),
                    N, replace = TRUE, prob = c(0.45, 0.25, 0.15, 0.10, 0.05))
# "slept under a bednet last night" — individual-level, correlated with district ITN
dist_itn <- districts$bednet_coverage[match(base$district, districts$district)]
bednet <- rbinom(N, 1, prob = pmin(pmax(dist_itn, 0.05), 0.95)) == 1

# --- severity cascade: hospitalised -> ICU -> died (highest in young children) ---
hospitalised <- rbinom(N, 1, p_hospitalised(age)) == 1
icu <- hospitalised & (rbinom(N, 1, p_icu_if_hosp(age)) == 1)

los <- rep(NA_real_, N)
los[hospitalised & !icu] <- pmax(1, round(rlnorm(sum(hospitalised & !icu), log(4), 0.5)))
los[icu]                 <- pmax(1, round(rlnorm(sum(icu), log(9), 0.6)))
admission_date <- as.Date(rep(NA, N), origin = "1970-01-01")
admission_date[hospitalised] <- report_date[hospitalised] + sample(0:2, sum(hospitalised), replace = TRUE)
discharge_date <- admission_date + los

died <- rbinom(N, 1, p_death(age, as.integer(icu))) == 1
outcome <- ifelse(died, "died", "recovered")
death_date <- as.Date(rep(NA, N), origin = "1970-01-01")
death_date[died & hospitalised]  <- discharge_date[died & hospitalised]
death_date[died & !hospitalised] <- report_date[died & !hospitalised] +
  sample(2:10, sum(died & !hospitalised), replace = TRUE)

case_linelist <- tibble(
  case_id = sprintf("M%06d", seq_len(N)),
  onset_date, report_date,
  epi_year = base$year, epi_week = base$week,   # surveillance week (delay-free)
  age, age_group, sex,
  district = base$district, facility_id,
  height_cm, weight_kg, ethnicity, bednet,
  hospitalised, icu, admission_date, discharge_date, outcome, death_date
) %>%
  arrange(report_date) %>%
  mutate(case_id  = sprintf("M%06d", seq_len(n())),
         sex      = factor(sex, levels = c("Male", "Female")),
         district = factor(district, levels = districts$district),
         outcome  = factor(outcome, levels = c("recovered", "died")))

# ------------------------------------------------------------------------------
# 5) Save master objects
# ------------------------------------------------------------------------------
saveRDS(case_linelist,      file.path(stage_dir, "case_linelist.rds"))
saveRDS(environment_weekly, file.path(stage_dir, "environment_weekly.rds"))
saveRDS(testing_weekly,     file.path(stage_dir, "testing_weekly.rds"))
saveRDS(districts,          file.path(stage_dir, "districts.rds"))
saveRDS(facilities,         file.path(stage_dir, "facilities.rds"))

cat("\n--- case_linelist summary ---\n")
cat(sprintf("rows: %d | cols: %d | dates %s to %s\n",
            nrow(case_linelist), ncol(case_linelist),
            min(case_linelist$report_date), max(case_linelist$report_date)))
cat(sprintf("median age: %.0f (IQR %.0f-%.0f) | under-5: %.1f%%\n",
            median(case_linelist$age),
            quantile(case_linelist$age, .25), quantile(case_linelist$age, .75),
            100 * mean(case_linelist$age < 5)))
cat(sprintf("hospitalised: %.1f%% | icu: %d | died: %d (CFR %.2f%%)\n",
            100*mean(case_linelist$hospitalised), sum(case_linelist$icu),
            sum(case_linelist$outcome == "died"), 100*mean(case_linelist$outcome == "died")))
