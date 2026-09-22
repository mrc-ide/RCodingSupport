# 02_derive_views.R
#
# Derives every teaching view from the two MASTER objects produced by
# 01_simulate_universe.R. Each view is a pure function of the masters: the
# package data objects (built for real in 03) and the file copies written by
# export_examples() all trace back to the same simulated world.
#
# This script saves nothing. It exists so the views can be inspected and the
# internal consistency of the scenario checked before anything is built.
#
# Run 01_simulate_universe.R first; masters are read from STAGE_DIR.
# ------------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(lubridate)
  library(ggplot2); library(patchwork); library(here)
})
# no stray Rplots.pdf when run via Rscript -- but never clobber a device the
# caller already opened (e.g. pdf("sanity.pdf") to capture every plot to a file)
if (!interactive() && is.null(dev.list())) pdf(NULL)
theme_set(theme_bw(base_size = 9))
peek <- function(title) cat("\n---", title, "---\n")

stage_dir <- Sys.getenv("STAGE_DIR", unset = here("data-raw", "staged_data"))
case_linelist      <- readRDS(file.path(stage_dir, "case_linelist.rds"))
environment_weekly <- readRDS(file.path(stage_dir, "environment_weekly.rds"))
testing_weekly     <- readRDS(file.path(stage_dir, "testing_weekly.rds"))
districts          <- readRDS(file.path(stage_dir, "districts.rds"))

describe <- function(name, x) {
  d <- if (is.data.frame(x) || is.matrix(x)) paste(nrow(x), "x", ncol(x)) else
    if (is.list(x)) paste0("list[", length(x), "]") else paste0("len ", length(x))
  cat(sprintf("  %-24s %-14s %s\n", name, class(x)[1], d))
}

cat("Deriving the teaching views from the master line-list + environment series\n")
cat(sprintf("MASTER case_linelist: %d rows; environment_weekly: %d rows\n\n",
            nrow(case_linelist), nrow(environment_weekly)))

# ============================ from case_linelist ==============================
cat("== from case_linelist ==\n")

# 1) patient_records: 150-row demographic cross-section (messy names kept on purpose)
patient_records <- case_linelist |>
  slice_sample(n = 150) |>
  transmute(ids = case_id, age, sex, ht_cm = height_cm, wt_kg = weight_kg,
            ethnicity, bednet)
describe("patient_records", patient_records)
peek("patient_records")
print(head(patient_records))
print(summary(patient_records[, -1]))

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
describe("incidence_weekly_age", incidence_weekly_age)

# 3) incidence_weekly: national weekly totals (vector length 260)
incidence_weekly <- colSums(incidence_weekly_age)
names(incidence_weekly) <- NULL
describe("incidence_weekly", incidence_weekly)

peek("incidence_weekly / incidence_weekly_age")
print(rowSums(incidence_weekly_age))   # should fall steeply with age

p_inc <- data.frame(week = 1:260, cases = incidence_weekly) |>
  ggplot(aes(week, cases)) +
  geom_line(linewidth = 0.5) +
  geom_vline(xintercept = seq(52, 208, 52), linetype = 3, colour = "grey50") +
  labs(x = "Week (1-260)", y = "Cases",
       title = "National weekly cases",
       subtitle = sprintf("range %d-%d", min(incidence_weekly), max(incidence_weekly)))

p_mat <- as.data.frame.table(incidence_weekly_age, responseName = "cases") |>
  setNames(c("age_group", "week", "cases")) |>
  mutate(week = as.integer(sub("Week_", "", week))) |>
  ggplot(aes(week, age_group, fill = cases)) +
  geom_raster() +
  scale_fill_viridis_c(option = "inferno", direction = -1) +
  labs(x = "Week", y = "Age band", title = "Cases by age band x week") +
  theme(legend.position = "bottom", legend.key.height = unit(3, "mm"))

print(p_inc + p_mat)

# 4) weekly_cases / weekly_tests: national weekly counts as data.frame
weekly_national <- testing_weekly |>
  group_by(year, week) |>
  summarise(cases = sum(cases), tested = sum(tested), .groups = "drop")
weekly_cases <- weekly_national |> select(year, week, cases)
weekly_tests <- weekly_national |> select(year, week, tested)
describe("weekly_cases", weekly_cases); describe("weekly_tests", weekly_tests)

# 5) district_weekly_list: two focus cities, district-specific column names
mk_district <- function(dname, suffix) {
  testing_weekly |> filter(district == dname) |>
    transmute(year, week, !!paste0("tested_", suffix) := tested,
              !!paste0("cases_", suffix) := cases)
}
district_weekly_list <- list(A = mk_district("Lusenga", "A"),
                             B = mk_district("Mbarako", "B"))
describe("district_weekly_list$A", district_weekly_list$A)

# 6) two_cities_cases: long form for the two cities, with a Date column
two_cities_cases <- testing_weekly |>
  filter(district %in% c("Lusenga", "Mbarako")) |>
  transmute(city = factor(district, levels = c("Lusenga", "Mbarako")),
            date = week_start, tested, cases)
describe("two_cities_cases", two_cities_cases)

peek("two focus cities")
city_cols <- c(Lusenga = "steelblue", Mbarako = "firebrick")

p_cities <- two_cities_cases |>
  ggplot(aes(date, cases, colour = city)) +
  geom_line(linewidth = 0.4) +
  scale_colour_manual(values = city_cols) +
  labs(x = NULL, y = "Cases", title = "Weekly cases, two focus cities") +
  theme(legend.position = "bottom")

p_citypos <- two_cities_cases |>
  ggplot(aes(date, cases / tested, colour = city)) +
  geom_point(size = 0.5, alpha = 0.6) +
  geom_smooth(se = FALSE, linewidth = 0.6, method = "loess", formula = y ~ x, span = 0.2) +
  scale_colour_manual(values = city_cols) +
  labs(x = NULL, y = "Positivity", title = "Test positivity") +
  theme(legend.position = "bottom")

print(p_cities + p_citypos)

# 7) regions_incidence: population + total cases per district
regions_incidence <- case_linelist |>
  count(district, name = "cases") |>
  left_join(select(districts, district, population), by = "district") |>
  transmute(region = district, population, cases)
describe("regions_incidence", regions_incidence)
peek("regions_incidence")
print(as.data.frame(regions_incidence |> mutate(per_1000_yr = round(1000 * cases / population / 5, 1))))

# 8) hospital_admissions: 250 hospitalised cases (messiness injected at export)
hospital_admissions <- case_linelist |>
  filter(hospitalised) |> slice_sample(n = 250) |>
  transmute(patient_id = case_id, hospital = district,
            admit_date = admission_date, discharge_date, age,
            icu_flag = ifelse(icu, "Y", "N"), outcome)
describe("hospital_admissions", hospital_admissions)
peek("hospital_admissions")
print(head(hospital_admissions, 4))
print(table(icu = hospital_admissions$icu_flag, outcome = hospital_admissions$outcome))

# 9) CFR_debugging: study_A clean, study_B mismatched lengths, study_C with NAs
agg <- case_linelist |>
  filter(district %in% c("Lusenga","Mbarako","Kanyeru","Tendaji")) |>
  group_by(district) |>
  summarise(deaths = sum(outcome == "died"), cases = n(), .groups = "drop")
study_A <- tibble(deaths = agg$deaths, cases = agg$cases)                      # clean
study_B <- list(deaths = agg$deaths[1:2], cases = agg$cases)                   # length mismatch
study_C <- study_A; study_C$deaths[2] <- NA; study_C$cases[3] <- NA            # NAs
describe("CFR_debugging (A)", study_A)
peek("CFR_debugging")
print(cbind(as.data.frame(agg), CFR_pct = round(100 * agg$deaths / agg$cases, 2)))

# 10) malaria_linelist: the master itself, renamed to a surveillance-style schema
malaria_linelist <- case_linelist |>
  transmute(Patient_ID = case_id, Health_facility = facility_id, Age = age, Sex = sex,
            District = district, admission_date,
            icu_admission_date = as.Date(ifelse(icu, admission_date, NA), origin = "1970-01-01"),
            discharge_date, death_date)
describe("malaria_linelist", malaria_linelist)

# ============================ from environment_weekly =========================
cat("\n== from environment_weekly ==\n")

env_cases <- environment_weekly |>
  left_join(select(testing_weekly, district, year, week, cases),
            by = c("district", "year", "week"))

# 11) rainfall_cases: one district's weekly rainfall + malaria cases
rainfall_cases <- env_cases |>
  filter(district == "Kanyeru") |>
  transmute(year, week, t, rainfall_mm = round(rainfall_mm, 1), malaria_cases = cases)
describe("rainfall_cases", rainfall_cases)

# 12) two_cities_temp: monthly mean temperature for the two cities (messy names)
two_cities_temp <- environment_weekly |>
  filter(district %in% c("Lusenga", "Mbarako")) |>
  mutate(month = month(week_start, label = TRUE, abbr = FALSE)) |>
  group_by(City = district, Year = year, Month = month) |>
  summarise(Temp_celcius = round(mean(temp_c), 1), .groups = "drop")
describe("two_cities_temp", two_cities_temp)

peek("rainfall_cases (Kanyeru) and two_cities_temp")

# rainfall and cases share a panel via a scaled secondary axis
scl <- max(rainfall_cases$rainfall_mm) / max(rainfall_cases$malaria_cases)
p_rc <- ggplot(rainfall_cases, aes(t)) +
  geom_col(aes(y = malaria_cases), fill = "grey70") +
  geom_line(aes(y = rainfall_mm / scl), colour = "steelblue", linewidth = 0.5) +
  scale_y_continuous(name = "Malaria cases",
                     sec.axis = sec_axis(~ . * scl, name = "Rainfall (mm)")) +
  labs(x = "Week", title = "Kanyeru: cases (bars) and rainfall (blue)") +
  theme(axis.title.y.right = element_text(colour = "steelblue"),
        axis.text.y.right  = element_text(colour = "steelblue"))

p_tt <- two_cities_temp |>
  ggplot(aes(Month, Temp_celcius, colour = factor(Year), group = Year)) +
  geom_line(linewidth = 0.5) +
  facet_wrap(~ City) +
  scale_colour_viridis_d(name = "Year") +
  scale_x_discrete(labels = function(x) substr(x, 1, 3)) +
  labs(x = NULL, y = "Temperature (C)", title = "Monthly mean temperature") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "bottom")

print(p_rc + p_tt)

cat("\nAll views derived from the two master objects.\n")

# ============================ internal consistency ============================
cat("\n== internal consistency checks ==\n")
national_weekly <- weekly_national |> arrange(year, week) |> pull(cases)
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
