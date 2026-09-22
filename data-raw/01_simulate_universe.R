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
# Seeded. Writes .rds to STAGE_DIR (default: data-raw/staged_data). Does not touch
# live package data — integration is a separate, explicit step.
#
# SANITY blocks (plots/summaries) are interleaved after each stage. They do not
# touch the RNG, so outputs are identical with or without them.
# ------------------------------------------------------------------------------

source(here::here("data-raw", "00_setting.R"))

dir.create(stage_dir, showWarnings = FALSE, recursive = TRUE)
message("Writing master objects to: ", stage_dir)

.set_seed()

# ------------------------------------------------------------------------------
# 1) Environmental series per district-week: rainfall (seasonal) + temperature
# ------------------------------------------------------------------------------
environment_weekly <- districts |>
  select(district, rain_baseline, temp_baseline) |>
  crossing(tibble(t = scenario_t, year = scenario_year,
                  week = scenario_week, week_start = scenario_week_start)) |>
  arrange(district, t) |>
  mutate(
    rainfall_mm = pmax(0, rain_baseline +
                         34 * sin(2 * pi * (t - 10) / weeks_per_year) +
                         rnorm(n(), 0, 12)),
    temp_c = temp_baseline + 3.5 * sin(2 * pi * (t - 30) / weeks_per_year) +
      rnorm(n(), 0, 1.0)
  )

# ---- SANITY: environment ------------------------------------------------------
peek("environment_weekly")
print(summary(environment_weekly[, c("rainfall_mm", "temp_c")]))

print(
  environment_weekly |>
    filter(district %in% c("Lusenga", "Rusoma")) |>
    select(district, week_start, Rainfall = rainfall_mm, Temperature = temp_c) |>
    pivot_longer(c(Rainfall, Temperature), names_to = "series", values_to = "value") |>
    ggplot(aes(week_start, value, colour = series)) +
    geom_line(linewidth = 0.4, show.legend = FALSE) +
    facet_grid(series ~ district, scales = "free_y", switch = "y") +
    scale_colour_manual(values = c(Rainfall = "steelblue", Temperature = "firebrick")) +
    labs(x = NULL, y = NULL, title = "Weekly environment, two contrasting districts",
         subtitle = "Rainfall mm/week; temperature deg C")
)

p_season <- environment_weekly |>
  group_by(district, week) |>
  summarise(rain = mean(rainfall_mm), .groups = "drop") |>
  left_join(select(districts, district, region), by = "district") |>
  ggplot(aes(week, rain, colour = region, group = district)) +
  geom_line(linewidth = 0.5) +
  scale_colour_manual(values = region_cols) +
  labs(x = "Week of year", y = "Mean rainfall (mm)",
       title = "Seasonal rainfall profile", subtitle = "one line per district") +
  theme(legend.position = "bottom")

p_temp <- environment_weekly |>
  mutate(district = reorder(district, temp_c)) |>
  ggplot(aes(district, temp_c)) +
  geom_boxplot(outlier.size = 0.3, fill = "grey85") +
  labs(x = NULL, y = "Temperature (C)", title = "Temperature spread by district") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(p_season + p_temp)

# ------------------------------------------------------------------------------
# 2) Expected weekly cases: lagged rainfall x seasonality x (1 - bednet effect)
# ------------------------------------------------------------------------------
true_lag <- 3L
bednet_effect <- 0.6   # proportional reduction in transmission at full ITN coverage

env_model <- environment_weekly |>
  left_join(select(districts, district, population, base_rate, bednet_coverage),
            by = "district") |>
  group_by(district) |>
  arrange(t, .by_group = TRUE) |>
  mutate(
    rain_lag = lag(rainfall_mm, n = true_lag, default = 0),
    seasonal = 1 + 0.6 * sin(2 * pi * (t - 6) / weeks_per_year),
    lambda   = population * (base_rate / weeks_per_year) * seasonal *
      exp(0.006 * (rain_lag - mean(rainfall_mm))) *
      (1 - bednet_effect * bednet_coverage),
    cases_expected = pmax(lambda, 0.1),
    cases          = rpois(n(), cases_expected)
  ) |>
  ungroup()

cat(sprintf("Simulated %d malaria cases over %d district-weeks (mean %.0f/week nationally)\n",
            sum(env_model$cases), nrow(env_model), sum(env_model$cases) / n_weeks))

# ---- SANITY: transmission model ----------------------------------------------
peek("env_model: cases vs drivers")
by_dist <- env_model |>
  group_by(district) |>
  summarise(population = first(population), bednet = first(bednet_coverage),
            base_rate = first(base_rate), cases = sum(cases),
            per_1000_yr = 1000 * cases / population / n_years, .groups = "drop") |>
  left_join(select(districts, district, region), by = "district") |>
  arrange(desc(per_1000_yr))
print(as.data.frame(select(by_dist, -region)), digits = 3)

nat <- env_model |>
  group_by(t, week_start) |>
  summarise(cases = sum(cases), rain = mean(rainfall_mm),
            expected = sum(cases_expected), .groups = "drop")

p_nat <- ggplot(nat, aes(week_start)) +
  geom_col(aes(y = cases), fill = "grey70", width = 6) +
  geom_line(aes(y = expected), colour = "red", linewidth = 0.6) +
  labs(x = NULL, y = "Cases / week", title = "National weekly cases",
       subtitle = "bars = simulated, red = expectation")

p_rain <- ggplot(nat, aes(week_start, rain)) +
  geom_line(colour = "steelblue", linewidth = 0.5) +
  labs(x = NULL, y = "Mean rainfall (mm)", title = "National mean rainfall")

# Cross-correlation of rainfall against cases. On the RAW series the shared annual
# cycle dominates and hides the lag: cases carry their own `seasonal` term (peaking
# ~4 weeks before rainfall does), so the raw peak sits at a small POSITIVE lag.
# Removing the week-of-year mean from both series strips that common cycle out, and
# what remains is the rainfall noise driving cases -- which recovers true_lag.
ccf_df <- function(x, y, label) {
  cc <- ccf(x, y, lag.max = 12, plot = FALSE)
  tibble(lag = as.numeric(cc$lag), acf = as.numeric(cc$acf), series = label)
}
nat_res <- nat |>
  mutate(woy = (t - 1) %% weeks_per_year + 1) |>
  group_by(woy) |>
  mutate(rain_res = rain - mean(rain), cases_res = cases - mean(cases)) |>
  ungroup()

cc_all <- bind_rows(
  ccf_df(nat$rain, nat$cases, "Raw series"),
  ccf_df(nat_res$rain_res, nat_res$cases_res, "Week-of-year mean removed")
) |>
  mutate(series = factor(series, levels = c("Raw series", "Week-of-year mean removed")))

peak_lags <- cc_all |> group_by(series) |> slice_max(acf, n = 1) |> ungroup()
cat(sprintf("ccf peak lag: raw %+d | deseasonalised %+d (planted lag is -%d)\n",
            peak_lags$lag[1], peak_lags$lag[2], true_lag))

p_ccf <- ggplot(cc_all, aes(lag, acf)) +
  geom_col(aes(fill = lag == -true_lag), width = 0.6, show.legend = FALSE) +
  facet_wrap(~ series, scales = "free_y") +
  scale_fill_manual(values = c(`FALSE` = "grey70", `TRUE` = "red")) +
  scale_x_continuous(breaks = seq(-12, 12, 6)) +
  labs(x = "Lag (weeks)", y = "Cross-correlation",
       title = "Rainfall leads cases",
       subtitle = sprintf("red = the planted lag of -%d; seasonality hides it in the raw series", true_lag))

p_itn <- ggplot(by_dist, aes(bednet, per_1000_yr, colour = region)) +
  geom_point(size = 2.5) +
  geom_text(aes(label = district), vjust = -1, size = 2.5, show.legend = FALSE) +
  scale_colour_manual(values = region_cols) +
  expand_limits(y = max(by_dist$per_1000_yr) * 1.12) +
  labs(x = "ITN coverage", y = "Cases per 1,000 per year",
       title = "District incidence vs ITN coverage",
       subtitle = sprintf("r = %.2f (base_rate also varies)", cor(by_dist$bednet, by_dist$per_1000_yr))) +
  theme(legend.position = "bottom")

print((p_nat + p_rain) / (p_ccf + p_itn))

# ------------------------------------------------------------------------------
# 3) Testing denominator per district-week (tested >= cases)
# ------------------------------------------------------------------------------
testing_weekly <- env_model |>
  transmute(
    district, year, week, week_start, cases,
    positivity = pmin(pmax(rbeta(n(), 5, 20), 0.05), 0.8),
    tested     = pmax(cases, round(cases / positivity))
  ) |>
  select(district, year, week, week_start, tested, cases)

# ---- SANITY: testing ----------------------------------------------------------
peek("testing_weekly")
tw_pos <- testing_weekly$cases / testing_weekly$tested   # NaN where a district-week had no tests
cat(sprintf("district-weeks with zero tests: %d (%.1f%%); zero cases: %d\n",
            sum(testing_weekly$tested == 0), 100 * mean(testing_weekly$tested == 0),
            sum(testing_weekly$cases == 0)))
cat(sprintf("positivity: median %.2f, IQR %.2f-%.2f, range %.2f-%.2f | tested >= cases: %s\n",
            median(tw_pos, na.rm = TRUE), quantile(tw_pos, .25, na.rm = TRUE),
            quantile(tw_pos, .75, na.rm = TRUE), min(tw_pos, na.rm = TRUE),
            max(tw_pos, na.rm = TRUE), all(testing_weekly$tested >= testing_weekly$cases)))

p_pos <- data.frame(pos = tw_pos) |>
  filter(is.finite(pos)) |>          # drop the zero-test district-weeks (0/0)
  ggplot(aes(pos)) +
  geom_histogram(bins = 40, fill = "grey70", colour = "white", linewidth = 0.2) +
  labs(x = "cases / tested", y = "District-weeks", title = "Weekly test positivity",
       subtitle = sprintf("%d district-weeks with no tests excluded", sum(!is.finite(tw_pos))))

p_ct <- ggplot(testing_weekly, aes(tested + 1, cases + 1)) +
  geom_point(alpha = 0.15, size = 0.4) +
  geom_abline(slope = 1, intercept = 0, colour = "red") +
  scale_x_log10() + scale_y_log10() +
  labs(x = "Tested + 1", y = "Cases + 1", title = "Cases vs tests (district-weeks)",
       subtitle = "red = equality; all points must sit below")

print(p_pos + p_ct)

# ------------------------------------------------------------------------------
# 4) Expand weekly district counts into an individual-level line-list
# ------------------------------------------------------------------------------
base <- env_model |>
  filter(cases > 0) |>
  select(district, t, year, week, week_start, cases) |>
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
) |>
  arrange(report_date) |>
  mutate(case_id  = sprintf("M%06d", seq_len(n())),
         sex      = factor(sex, levels = c("Male", "Female")),
         district = factor(district, levels = districts$district),
         outcome  = factor(outcome, levels = c("recovered", "died")))

# ---- SANITY: the line-list ----------------------------------------------------
# (all RNG is finished by this point, so anything goes below)
peek("case_linelist: demographics")
print(summary(case_linelist[, c("age", "height_cm", "weight_kg")]))
print(round(100 * prop.table(table(case_linelist$ethnicity)), 1))
print(table(sex = case_linelist$sex))

thin <- case_linelist[seq(1, N, by = 25), ]   # deterministic thinning for scatterplots

p_ages <- ggplot(case_linelist, aes(age)) +
  geom_histogram(binwidth = 1, fill = "grey70") +
  labs(x = "Age (years)", y = "Cases",
       title = "Case ages",
       subtitle = sprintf("median %d, %.0f%% under 5", median(case_linelist$age),
                          100 * mean(case_linelist$age < 5)))

p_ht <- ggplot(thin, aes(age, height_cm, colour = sex)) +
  geom_point(size = 0.4, alpha = 0.4) +
  scale_colour_manual(values = c(Male = "steelblue", Female = "firebrick")) +
  labs(x = "Age (years)", y = "Height (cm)", title = "Height vs age") +
  theme(legend.position = "bottom")

p_wt <- ggplot(thin, aes(height_cm, weight_kg)) +
  geom_point(size = 0.4, alpha = 0.3) +
  labs(x = "Height (cm)", y = "Weight (kg)", title = "Weight vs height")

p_delay <- case_linelist |>
  mutate(delay = as.integer(report_date - onset_date)) |>
  count(delay) |>
  ggplot(aes(delay, n)) +
  geom_col(fill = "grey70") +
  labs(x = "Onset -> report delay (days)", y = "Cases", title = "Reporting delay")

print((p_ages + p_ht) / (p_wt + p_delay))

peek("case_linelist: severity cascade by age")
sev <- case_linelist |>
  group_by(age_group) |>
  summarise(n = n(), Hospitalised = mean(hospitalised), ICU = mean(icu),
            `Died (CFR)` = mean(outcome == "died"), .groups = "drop") |>
  filter(n >= 50)
print(as.data.frame(sev |> mutate(across(Hospitalised:`Died (CFR)`, ~ round(100 * .x, 2)))))
cat(sprintf("overall: hospitalised %.1f%% | ICU %.2f%% | CFR %.2f%% | deaths %d\n",
            100 * mean(case_linelist$hospitalised), 100 * mean(case_linelist$icu),
            100 * mean(case_linelist$outcome == "died"), sum(case_linelist$outcome == "died")))

p_sev <- sev |>
  pivot_longer(c(Hospitalised, ICU, `Died (CFR)`), names_to = "outcome", values_to = "p") |>
  mutate(outcome = factor(outcome, levels = c("Hospitalised", "ICU", "Died (CFR)"))) |>
  ggplot(aes(age_group, 100 * p, fill = outcome)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ outcome, scales = "free_y") +
  scale_fill_manual(values = c(Hospitalised = "grey60", ICU = "orange", `Died (CFR)` = "firebrick")) +
  labs(x = "Age group", y = "Percent of cases", title = "Severity falls with age") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))

p_los <- tibble(los = los, type = ifelse(icu, "ICU", "Ward")) |>
  filter(!is.na(los)) |>
  ggplot(aes(los, fill = type)) +
  geom_histogram(binwidth = 1, show.legend = FALSE) +
  facet_wrap(~ type, scales = "free_y") +
  scale_fill_manual(values = c(Ward = "grey60", ICU = "orange")) +
  labs(x = "Length of stay (days)", y = "Admissions", title = "Length of stay")

print(p_sev / p_los)

peek("case_linelist: place")
place <- case_linelist |>
  count(district) |>
  left_join(select(districts, district, population, bednet_coverage), by = "district") |>
  mutate(per_1000_yr = round(1000 * n / population / n_years, 1),
         bednet_use = round(100 * tapply(case_linelist$bednet, case_linelist$district, mean)[as.character(district)], 1))
print(as.data.frame(place))
cat("cases per facility (first 12):\n")
print(head(sort(table(case_linelist$facility_id), decreasing = TRUE), 12))

peek("case_linelist: date integrity")
chk <- function(label, ok) cat(sprintf("  [%s] %s\n", if (ok) "OK" else "FAIL", label))
chk("report_date >= onset_date", all(case_linelist$report_date >= case_linelist$onset_date))
chk("admission_date only when hospitalised", all(is.na(case_linelist$admission_date) == !case_linelist$hospitalised))
chk("discharge_date > admission_date", all(case_linelist$discharge_date > case_linelist$admission_date, na.rm = TRUE))
chk("death_date only when died", all(is.na(case_linelist$death_date) == (case_linelist$outcome == "recovered")))
chk("every ICU case is hospitalised", all(case_linelist$hospitalised[case_linelist$icu]))
chk("case_id unique", !anyDuplicated(case_linelist$case_id))
chk("facility belongs to case's district",
    all(facilities$district[match(case_linelist$facility_id, facilities$facility_id)] == as.character(case_linelist$district)))

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
