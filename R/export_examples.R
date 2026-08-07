#' Export teaching datasets to files for data-import practice
#'
#' Writes file copies of the canonical malaria teaching datasets — derived from the
#' master \code{case_linelist} and \code{environment_weekly} objects (plus the
#' analysis/genetics objects) — into a directory, in the formats and with the
#' deliberate "messiness" that the Week 3+ import and cleaning problem sets expect.
#'
#' The point is that the file a student imports is provably the same data they
#' already met via \code{library(Rcoding)}: the package is the single source of
#' truth, and file import is just one way to reach it.
#'
#' @param dir Directory to write files into (created if needed).
#' @param which Character vector of dataset keys to export, or \code{"all"}.
#' @param seed Integer seed so the injected messiness is reproducible.
#'
#' @return (invisibly) a character vector of the file paths written.
#' @export
#'
#' @examples
#' \dontrun{
#' dir.create("data_raw")
#' Rcoding::export_examples("data_raw")
#' }
export_examples <- function(dir, which = "all", seed = 2026) {
  stopifnot(is.character(dir), length(dir) == 1)
  dir.create(dir, showWarnings = FALSE, recursive = TRUE)
  set.seed(seed)

  # canonical objects (lazy-loaded from the package; supplied in globalenv when testing)
  case_linelist           <- get("case_linelist")
  environment_weekly      <- get("environment_weekly")
  testing_weekly          <- get("testing_weekly")
  districts               <- get("districts")
  resistance_trajectories <- get("resistance_trajectories")
  chw_trial               <- get("chw_trial")
  posterior_density       <- get("posterior_density")

  cities <- c("Lusenga", "Mbarako")
  written <- character(0)
  want <- function(key) identical(which, "all") || key %in% which
  put  <- function(p) { written <<- c(written, p); p }

  wk_nat <- stats::aggregate(cbind(cases, tested) ~ year + week,
                             data = testing_weekly, FUN = sum)
  wk_nat <- wk_nat[order(wk_nat$year, wk_nat$week), ]

  # weekly_cases.rds -----------------------------------------------------------
  if (want("weekly_cases"))
    saveRDS(wk_nat[, c("year", "week", "cases")], put(file.path(dir, "weekly_cases.rds")))

  # weekly_tests.txt (tab-delimited) ------------------------------------------
  if (want("weekly_tests"))
    utils::write.table(wk_nat[, c("year", "week", "tested")],
                       put(file.path(dir, "weekly_tests.txt")),
                       sep = "\t", quote = FALSE, row.names = FALSE)

  # regions_incidence.rds ------------------------------------------------------
  if (want("regions_incidence")) {
    ri <- as.data.frame(table(case_linelist$district)); names(ri) <- c("district", "cases")
    ri$population <- districts$population[match(ri$district, districts$district)]
    saveRDS(ri[, c("district", "population", "cases")],
            put(file.path(dir, "regions_incidence.rds")))
  }

  # hospital_admissions.csv (messy: chr dates, one "missing" age, Y/N flag) -----
  if (want("hospital_admissions")) {
    h <- case_linelist[case_linelist$hospitalised, ]; h <- h[sample(nrow(h), 250), ]
    age_chr <- as.character(h$age); age_chr[sample.int(250, 1)] <- "missing"
    adm <- data.frame(
      patient_id     = h$case_id,
      hospital       = as.character(h$district),
      admit_date     = format(h$admission_date, "%Y-%m-%d"),
      discharge_date = format(h$discharge_date, "%Y-%m-%d"),
      age            = age_chr,
      icu_flag       = ifelse(h$icu, "Y", "N"),
      outcome        = as.character(h$outcome),
      stringsAsFactors = FALSE)
    utils::write.csv(adm, put(file.path(dir, "hospital_admissions.csv")), row.names = FALSE)
  }

  # misc_objects.RData ---------------------------------------------------------
  if (want("misc_objects")) {
    surveillance_regions <- sort(unique(as.character(districts$region)))
    lab_turnaround <- data.frame(lab_id = paste0("LAB", 1:6),
      median_days = sample(1:5, 6, TRUE), urgent_pct = round(stats::runif(6, .05, .30), 2),
      stringsAsFactors = FALSE)
    metadata <- list(source = "Synthetic malaria surveillance data for teaching",
                     contact = "teaching-team@example.org")
    save(surveillance_regions, lab_turnaround, metadata,
         file = put(file.path(dir, "misc_objects.RData")))
  }

  # rainfall_cases.csv (one district: weekly rainfall + malaria cases) ---------
  if (want("rainfall_cases")) {
    e  <- environment_weekly[environment_weekly$district == "Kanyeru", ]
    tw <- testing_weekly[testing_weekly$district == "Kanyeru", ]
    e  <- merge(e, tw[, c("year", "week", "cases")], by = c("year", "week"))
    e  <- e[order(e$t), ]
    rc <- data.frame(year = e$year, week = e$week, t = e$t,
                     rainfall_mm = round(e$rainfall_mm, 1), malaria_cases = e$cases)
    utils::write.csv(rc, put(file.path(dir, "rainfall_cases.csv")), row.names = FALSE)
  }

  # two_cities_cases.rds -------------------------------------------------------
  if (want("two_cities_cases")) {
    tc <- testing_weekly[testing_weekly$district %in% cities, ]
    tc <- data.frame(city = factor(as.character(tc$district), levels = cities),
                     date = tc$week_start, tested = tc$tested, cases = tc$cases)
    saveRDS(tc, put(file.path(dir, "two_cities_cases.rds")))
  }

  # two_cities_temp.rds (monthly, deliberately inconsistent column names) ------
  if (want("two_cities_temp")) {
    e <- environment_weekly[environment_weekly$district %in% cities, ]
    e$month <- factor(format(e$week_start, "%B"), levels = month.name, ordered = TRUE)
    agg <- stats::aggregate(temp_c ~ district + year + month, data = e, FUN = mean)
    tt <- data.frame(City = agg$district, Year = agg$year, Month = agg$month,
                     Temp_celcius = round(agg$temp_c, 1))
    saveRDS(tt, put(file.path(dir, "two_cities_temp.rds")))
  }

  # weather_raw.csv (DAILY, messy city strings + character dates) --------------
  if (want("weather_raw")) {
    e <- environment_weekly[environment_weekly$district %in% cities &
                              environment_weekly$year == min(environment_weekly$year), ]
    rows <- do.call(rbind, lapply(seq_len(nrow(e)), function(i) {
      days <- e$week_start[i] + 0:6
      data.frame(city = as.character(e$district[i]),
                 date = format(days, "%Y-%m-%d"),
                 t_mean_c = round(e$temp_c[i] + stats::rnorm(7, 0, 1), 1),
                 rain_mm  = round(pmax(0, e$rainfall_mm[i] / 7 + stats::rnorm(7, 0, 2)), 1),
                 stringsAsFactors = FALSE)
    }))
    # inject whitespace + inconsistent case on ~5% of city names
    n_ws <- round(0.05 * nrow(rows)); idx <- sample(nrow(rows), n_ws)
    rows$city[idx] <- paste0(sample(c(" ", "  ", "\t"), n_ws, TRUE), rows$city[idx])
    idx2 <- sample(setdiff(seq_len(nrow(rows)), idx), round(0.03 * nrow(rows)))
    rows$city[idx2] <- ifelse(sample(c(TRUE, FALSE), length(idx2), TRUE),
                              tolower(rows$city[idx2]), toupper(rows$city[idx2]))
    rows <- rows[sample(nrow(rows)), ]
    utils::write.csv(rows, put(file.path(dir, "weather_raw.csv")), row.names = FALSE)
  }

  # ward_usage_*.csv (two hospitals, deliberately inconsistent schemas) --------
  if (want("ward_usage")) {
    sim_hosp <- function(hid, dates, wards, caps, occ) {
      g <- expand.grid(dt = dates, ward = wards, KEEP.OUT.ATTRS = FALSE)
      g$hosp_id  <- hid
      g$beds_cap <- caps[match(g$ward, wards)]
      prob <- pmin(pmax(occ + stats::rnorm(nrow(g), 0, .05), 0), 1)
      g$beds_occ <- pmax(0, pmin(g$beds_cap, stats::rbinom(nrow(g), g$beds_cap, prob)))
      g
    }
    central <- sim_hosp("LUSENGA_CENTRAL",
                        seq(as.Date("2023-01-01"), as.Date("2023-01-30"), "day"),
                        c("Paediatric", "ICU", "General"), c(30, 12, 40), 0.82)
    utils::write.csv(central[, c("hosp_id", "ward", "dt", "beds_occ", "beds_cap")],
                     put(file.path(dir, "ward_usage_central.csv")), row.names = FALSE)
    hill <- sim_hosp("MBARAKO_DISTRICT",
                     seq(as.Date("2023-01-05"), as.Date("2023-02-03"), "day"),
                     c("Paeds", "ICU", "Gen"), c(24, 10, 36), 0.78)
    utils::write.csv(data.frame(date_reported = hill$dt, hospital = hill$hosp_id,
                                ward_name = hill$ward, n_occupied = hill$beds_occ,
                                n_beds = hill$beds_cap),
                     put(file.path(dir, "ward_usage_hill.csv")), row.names = FALSE)
  }

  # malaria_linelist.rds (the master, surveillance-style column names) ---------
  if (want("malaria_linelist")) {
    ml <- data.frame(
      Patient_ID = case_linelist$case_id, Health_facility = case_linelist$facility_id,
      Age = case_linelist$age, Sex = case_linelist$sex, District = case_linelist$district,
      admission_date = case_linelist$admission_date,
      icu_admission_date = as.Date(ifelse(case_linelist$icu, case_linelist$admission_date, NA),
                                   origin = "1970-01-01"),
      discharge_date = case_linelist$discharge_date, death_date = case_linelist$death_date)
    saveRDS(ml, put(file.path(dir, "malaria_linelist.rds")))
  }

  # CFR_debugging.RData (clean / length-mismatch / NA variants) ----------------
  if (want("CFR_debugging")) {
    d4 <- c("Lusenga", "Mbarako", "Kanyeru", "Tendaji")
    sub <- case_linelist[case_linelist$district %in% d4, ]
    ag <- stats::aggregate(cbind(deaths = outcome == "died", cases = 1) ~ district,
                           data = sub, FUN = sum)
    study_A <- data.frame(deaths = ag$deaths, cases = ag$cases)
    study_B <- list(deaths = ag$deaths[1:2], cases = ag$cases)
    study_C <- study_A; study_C$deaths[2] <- NA; study_C$cases[3] <- NA
    save(study_A, study_B, study_C, file = put(file.path(dir, "CFR_debugging.RData")))
  }

  # file copies of keeper objects (clean reads for later weeks) ----------------
  if (want("resistance_trajectories"))
    saveRDS(resistance_trajectories, put(file.path(dir, "resistance_trajectories.rds")))
  if (want("chw_trial"))
    saveRDS(chw_trial, put(file.path(dir, "chw_trial.rds")))
  if (want("posterior_density"))
    saveRDS(posterior_density, put(file.path(dir, "posterior_density.rds")))

  invisible(written)
}
