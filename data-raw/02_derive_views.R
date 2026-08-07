# 02_derive_views.R
#
# Derives every legacy teaching dataset from the two MASTER objects produced by
# 01_simulate_universe.R. This is the proof that the ~20 old datasets collapse
# onto a small canonical core. Each view is a pure function of the masters.
#
# Run 01_simulate_universe.R first. Reads masters from STAGE_DIR; prints, for each
# legacy dataset, the reproduced object's shape so it can be compared to the original.
# ------------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(lubridate)
})

stage_dir      <- Sys.getenv("STAGE_DIR", unset = file.path(getwd(), "staged_data"))
case_linelist  <- readRDS(file.path(stage_dir, "case_linelist.rds"))
environment_weekly <- readRDS(file.path(stage_dir, "environment_weekly.rds"))
testing_weekly <- readRDS(file.path(stage_dir, "testing_weekly.rds"))
districts      <- readRDS(file.path(stage_dir, "districts.rds"))

report <- function(name, x) {
  d <- if (is.data.frame(x)) paste(nrow(x), "x", ncol(x)) else
    if (is.matrix(x)) paste(nrow(x), "x", ncol(x)) else
      if (is.list(x)) paste0("list[", length(x), "]") else paste0("len ", length(x))
  cat(sprintf("  %-24s -> %-14s %s\n", name, class(x)[1], d))
}

cat("Deriving legacy datasets from the master line-list + environment series\n")
cat(sprintf("MASTER case_linelist: %d rows; environment_weekly: %d rows\n\n",
            nrow(case_linelist), nrow(environment_weekly)))

# ============================ from case_linelist ==============================
cat("== from case_linelist ==\n")

# 1) patient_records: 150-row demographic cross-section (messy names kept on purpose)
patient_records <- case_linelist %>%
  slice_sample(n = 150) %>%
  transmute(ids = case_id, age, sex, ht_cm = height_cm, wt_kg = weight_kg,
            ethnicity, bednet)
report("patient_records", patient_records)

# 2) incidence_weekly_age: 20 age-bands x 260 weeks matrix (from the surveillance
#    week carried in the line-list, so every case is counted exactly once)
wk_id <- (case_linelist$epi_year - min(case_linelist$epi_year)) * 52L +
  case_linelist$epi_week
incidence_weekly_age <- matrix(
  as.integer(table(factor(case_linelist$age_group,
                          levels = levels(case_linelist$age_group)),
                   factor(wk_id, levels = 1:260))),
  nrow = nlevels(case_linelist$age_group),
  dimnames = list(levels(case_linelist$age_group), sprintf("Week_%03d", 1:260)))
report("incidence_weekly_age", incidence_weekly_age)

# 3) incidence_weekly: national weekly totals (vector length 260)
incidence_weekly <- colSums(incidence_weekly_age)
names(incidence_weekly) <- NULL
report("incidence_weekly", incidence_weekly)

# 4) weekly_cases / weekly_tests: national weekly counts as data.frame
weekly_national <- testing_weekly %>%
  group_by(year, week) %>% summarise(cases = sum(cases), tested = sum(tested), .groups = "drop")
weekly_cases <- weekly_national %>% select(year, week, cases)
weekly_tests <- weekly_national %>% select(year, week, tested)
report("weekly_cases", weekly_cases); report("weekly_tests", weekly_tests)

# 5) district_weekly_list: two focus cities, district-specific column names
mk_district <- function(dname, suffix) {
  testing_weekly %>% filter(district == dname) %>%
    transmute(year, week, !!paste0("tested_", suffix) := tested,
              !!paste0("cases_", suffix) := cases)
}
district_weekly_list <- list(A = mk_district("Lusenga", "A"),
                             B = mk_district("Mbarako", "B"))
report("district_weekly_list$A", district_weekly_list$A)

# 6) two_cities_cases: long form for the two cities, with a Date column
two_cities_cases <- testing_weekly %>%
  filter(district %in% c("Lusenga", "Mbarako")) %>%
  transmute(city = factor(district, levels = c("Lusenga", "Mbarako")),
            date = week_start, tested, cases)
report("two_cities_cases", two_cities_cases)

# 7) regions_incidence: population + total cases per district (= region)
regions_incidence <- case_linelist %>% count(district, name = "cases") %>%
  left_join(select(districts, district, population), by = "district") %>%
  transmute(region = district, population, cases)
report("regions_incidence", regions_incidence)

# 8) hospital_admissions: 250 hospitalised cases (messiness injected at export)
hospital_admissions <- case_linelist %>% filter(hospitalised) %>% slice_sample(n = 250) %>%
  transmute(patient_id = case_id, hospital = district,
            admit_date = admission_date, discharge_date, age,
            icu_flag = ifelse(icu, "Y", "N"), outcome)
report("hospital_admissions", hospital_admissions)

# 9) CFR_debugging: study_A clean, study_B mismatched lengths, study_C with NAs
agg <- case_linelist %>% filter(district %in% c("Lusenga","Mbarako","Kanyeru","Tendaji")) %>%
  group_by(district) %>% summarise(deaths = sum(outcome == "died"), cases = n(), .groups = "drop")
study_A <- tibble(deaths = agg$deaths, cases = agg$cases)                      # clean
study_B <- list(deaths = agg$deaths[1:2], cases = agg$cases)                   # length mismatch
study_C <- study_A; study_C$deaths[2] <- NA; study_C$cases[3] <- NA            # NAs
report("CFR_debugging (A)", study_A)

# 10) malaria_linelist: the master itself, renamed to a surveillance-style schema
malaria_linelist <- case_linelist %>%
  transmute(Patient_ID = case_id, Health_facility = facility_id, Age = age, Sex = sex,
            District = district, admission_date,
            icu_admission_date = as.Date(ifelse(icu, admission_date, NA), origin = "1970-01-01"),
            discharge_date, death_date)
report("malaria_linelist", malaria_linelist)

# ============================ from environment_weekly =========================
cat("\n== from environment_weekly ==\n")

env_cases <- environment_weekly %>%
  left_join(select(testing_weekly, district, year, week, cases), by = c("district","year","week"))

# 11) rainfall_cases: one district's weekly rainfall + malaria cases
rainfall_cases <- env_cases %>% filter(district == "Kanyeru") %>%
  transmute(year, week, t, rainfall_mm = round(rainfall_mm, 1), malaria_cases = cases)
report("rainfall_cases", rainfall_cases)

# 12) two_cities_temp: monthly mean temperature for the two cities (messy names)
two_cities_temp <- environment_weekly %>%
  filter(district %in% c("Lusenga", "Mbarako")) %>%
  mutate(month = month(week_start, label = TRUE, abbr = FALSE)) %>%
  group_by(City = district, Year = year, Month = month) %>%
  summarise(Temp_celcius = round(mean(temp_c), 1), .groups = "drop")
report("two_cities_temp", two_cities_temp)

cat("\nAll legacy dataset shapes reproduced from the two master objects.\n")

# ============================ internal consistency ============================
cat("\n== internal consistency checks ==\n")
national_weekly <- weekly_national %>% arrange(year, week) %>% pull(cases)
chk <- function(label, ok) cat(sprintf("  [%s] %s\n", if (ok) "OK" else "FAIL", label))

chk("sum(incidence_weekly_age) == nrow(case_linelist)",
    sum(incidence_weekly_age) == nrow(case_linelist))
chk("incidence_weekly == colSums(incidence_weekly_age)",
    all(incidence_weekly == colSums(incidence_weekly_age)))
chk("incidence_weekly == national weekly cases (testing_weekly)",
    all(incidence_weekly == national_weekly))
chk("district_weekly_list$A cases == line-list Lusenga count",
    sum(district_weekly_list$A$cases_A) == sum(case_linelist$district == "Lusenga"))
chk("regions_incidence cases sum == nrow(case_linelist)",
    sum(regions_incidence$cases) == nrow(case_linelist))
chk("tested >= cases in testing_weekly",
    all(testing_weekly$tested >= testing_weekly$cases))
chk("no hospitalised case missing an admission_date",
    all(!is.na(case_linelist$admission_date[case_linelist$hospitalised])))
chk("every ICU case is hospitalised",
    all(case_linelist$hospitalised[case_linelist$icu]))
chk("every death has outcome == 'died'",
    all(case_linelist$outcome[!is.na(case_linelist$death_date)] == "died"))
