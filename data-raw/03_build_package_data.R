# 03_build_package_data.R
#
# Integration step: build the package's lazy-loaded data objects (data/*.rda) from
# the masters produced by 01_simulate_universe.R. These are the objects exposed via
# library(RCodingSupport) and used directly by the early (pre-import) weeks.
#
# SAFETY: by default this writes .rda files to STAGE_DIR, NOT the live package
# data/ folder, so it can be reviewed before it replaces anything. To actually
# install into the package, run with TARGET=package (which writes to ../data).
# ------------------------------------------------------------------------------

suppressPackageStartupMessages({ library(dplyr); library(tidyr) })

stage_dir <- Sys.getenv("STAGE_DIR", unset = file.path(getwd(), "staged_data"))
target    <- Sys.getenv("TARGET", unset = "stage")   # "stage" or "package"
out_dir   <- if (target == "package") file.path(dirname(getwd()), "data") else stage_dir
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
message("Building package data into: ", out_dir, "  (TARGET=", target, ")")

# --- load masters -------------------------------------------------------------
set.seed(2026)   # reproducible sampling for patient_records

case_linelist      <- readRDS(file.path(stage_dir, "case_linelist.rds"))
environment_weekly <- readRDS(file.path(stage_dir, "environment_weekly.rds"))
testing_weekly     <- readRDS(file.path(stage_dir, "testing_weekly.rds"))
districts          <- readRDS(file.path(stage_dir, "districts.rds"))
facilities         <- readRDS(file.path(stage_dir, "facilities.rds"))

# --- derived view objects used before import is taught ------------------------
# Surveillance-week index 1..260 from the delay-free epi week carried in the
# line-list, so every case is counted and the matrix is exactly consistent with
# testing_weekly (no cases lost to reporting delay near the series ends).
base_year <- min(case_linelist$epi_year)
wk_id <- (case_linelist$epi_year - base_year) * 52L + case_linelist$epi_week
incidence_weekly_age <- matrix(
  as.integer(table(factor(case_linelist$age_group,
                          levels = levels(case_linelist$age_group)),
                   factor(wk_id, levels = 1:260))),
  nrow = nlevels(case_linelist$age_group),
  dimnames = list(levels(case_linelist$age_group), sprintf("Week_%03d", 1:260)))
incidence_weekly <- unname(colSums(incidence_weekly_age))

patient_records <- case_linelist %>% slice_sample(n = 150) %>%
  transmute(ids = case_id, age, sex, ht_cm = height_cm, wt_kg = weight_kg, ethnicity, bednet) %>%
  as.data.frame()

# District B starts a year later than A on purpose, so the two series cover
# different periods -- this is what makes the merge()/join lesson in 2.B.1 bite
# (non-overlapping rows are dropped by an inner join).
mk_d <- function(dn, sfx, min_year = -Inf) testing_weekly %>%
  filter(district == dn, year >= min_year) %>%
  transmute(year, week, !!paste0("tested_", sfx) := tested, !!paste0("cases_", sfx) := cases) %>%
  as.data.frame()
district_weekly_list <- list(A = mk_d("Lusenga", "A"),
                             B = mk_d("Mbarako", "B", min_year = 2021))

# --- keeper objects from generators 04/05/06 (read from stage) ----------------
allele_freq_matrix      <- readRDS(file.path(stage_dir, "allele_freq_matrix.rds"))
resistance_trajectories <- readRDS(file.path(stage_dir, "resistance_trajectories.rds"))
analysis_bundle         <- readRDS(file.path(stage_dir, "analysis_bundle.rds"))
posterior_density       <- readRDS(file.path(stage_dir, "posterior_density.rds"))
chw_trial               <- readRDS(file.path(stage_dir, "chw_trial.rds"))

# --- save each object as its own .rda (name = object name) --------------------
objs <- c("case_linelist", "environment_weekly", "testing_weekly", "districts",
          "facilities", "incidence_weekly", "incidence_weekly_age",
          "patient_records", "district_weekly_list",
          "allele_freq_matrix", "resistance_trajectories", "analysis_bundle",
          "posterior_density", "chw_trial")
for (nm in objs) {
  save(list = nm, file = file.path(out_dir, paste0(nm, ".rda")), compress = "bzip2")
  cat(sprintf("  saved %s.rda\n", nm))
}
cat(sprintf("\nBuilt %d package data objects.\n", length(objs)))
