# 03_build_package_data.R
#
# Integration step: build the package's lazy-loaded data objects (data/*.rda) from
# the masters produced by 01_simulate_universe.R. These are the objects exposed via
# library(RCodingSupport) and used directly by the early (pre-import) weeks.
#
# This writes straight to data/. Everything is seeded, so re-running the pipeline
# from 01 reproduces the same objects byte for byte; git is the safety net, not a
# staging copy. All inputs are read before anything is written, so a missing or
# incomplete stage fails before it can touch data/.
# ------------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(ggplot2); library(patchwork); library(here)
})
# no stray Rplots.pdf when run via Rscript -- but never clobber a device the
# caller already opened (e.g. pdf("sanity.pdf") to capture every plot to a file)
if (!interactive() && is.null(dev.list())) pdf(NULL)
theme_set(theme_bw(base_size = 9))
peek <- function(title) cat("\n---", title, "---\n")

stage_dir <- Sys.getenv("STAGE_DIR", unset = here("data-raw", "staged_data"))
out_dir   <- here("data")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
message("Building package data into: ", out_dir)

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

patient_records <- case_linelist |>
  slice_sample(n = 150) |>
  transmute(ids = case_id, age, sex, ht_cm = height_cm, wt_kg = weight_kg, ethnicity, bednet) |>
  as.data.frame()

# District B starts a year later than A on purpose, so the two series cover
# different periods -- this is what makes the merge()/join lesson in 2.B.1 bite
# (non-overlapping rows are dropped by an inner join).
mk_d <- function(dn, sfx, min_year = -Inf) testing_weekly |>
  filter(district == dn, year >= min_year) |>
  transmute(year, week, !!paste0("tested_", sfx) := tested, !!paste0("cases_", sfx) := cases) |>
  as.data.frame()
district_weekly_list <- list(A = mk_d("Lusenga", "A"),
                             B = mk_d("Mbarako", "B", min_year = 2021))

# --- keeper objects from generators 04/05/06 (read from stage) ----------------
allele_freq_matrix      <- readRDS(file.path(stage_dir, "allele_freq_matrix.rds"))
resistance_trajectories <- readRDS(file.path(stage_dir, "resistance_trajectories.rds"))
analysis_bundle         <- readRDS(file.path(stage_dir, "analysis_bundle.rds"))
posterior_density       <- readRDS(file.path(stage_dir, "posterior_density.rds"))
chw_trial               <- readRDS(file.path(stage_dir, "chw_trial.rds"))

# ---- SANITY: what the students will see -------------------------------------
# (all RNG use is finished above, so these cannot perturb the build)
peek("the week-2 objects, as a student meets them")
cat("incidence_weekly:      length", length(incidence_weekly), "| range", range(incidence_weekly), "\n")
cat("incidence_weekly_age:  ", paste(dim(incidence_weekly_age), collapse = " x "),
    "| sums to", sum(incidence_weekly_age), "vs", nrow(case_linelist), "cases\n")
print(head(patient_records, 4)); print(sapply(patient_records, class))
cat("district_weekly_list: A", nrow(district_weekly_list$A), "rows",
    paste(range(district_weekly_list$A$year), collapse = "-"),
    "| B", nrow(district_weekly_list$B), "rows",
    paste(range(district_weekly_list$B$year), collapse = "-"),
    "| inner merge ->", nrow(merge(district_weekly_list$A, district_weekly_list$B)), "rows\n")

p_inc <- data.frame(week = seq_along(incidence_weekly), cases = incidence_weekly) |>
  ggplot(aes(week, cases)) +
  geom_line(linewidth = 0.4) + geom_point(size = 0.5) +
  labs(x = "Week", y = "Cases", title = "incidence_weekly", subtitle = "what 2.A.1 plots")

p_ab <- bind_rows(
  district_weekly_list$A |>
    transmute(row = row_number(), positivity = cases_A / tested_A, series = "A (Lusenga)"),
  district_weekly_list$B |>
    transmute(row = row_number() + 52, positivity = cases_B / tested_B, series = "B (Mbarako)")
) |>
  ggplot(aes(row, positivity, colour = series)) +
  geom_line(linewidth = 0.4) +
  scale_colour_manual(values = c("A (Lusenga)" = "steelblue", "B (Mbarako)" = "firebrick")) +
  labs(x = "Row (weeks from series start)", y = "Positivity", colour = NULL,
       title = "2.B.1: B starts a year later than A",
       subtitle = "the non-overlap an inner join drops") +
  theme(legend.position = "bottom")

print(p_inc + p_ab)

peek("keeper objects from 04/05/06")
cat("allele_freq_matrix:     ", paste(dim(allele_freq_matrix), collapse = " x "),
    sprintf("| %.1f%% NA | %d negatives\n", 100 * mean(is.na(allele_freq_matrix)),
            sum(allele_freq_matrix < 0, na.rm = TRUE)))
cat("resistance_trajectories:", nrow(resistance_trajectories$data), "obs rows,",
    nrow(resistance_trajectories$model), "model rows |",
    nlevels(resistance_trajectories$data$marker), "markers x",
    nlevels(resistance_trajectories$data$site), "sites\n")
cat("analysis_bundle:        ", nrow(analysis_bundle$data), "villages | slope",
    round(coef(analysis_bundle$fit)[2], 2), "\n")
cat("posterior_density:      ", length(posterior_density$R0), "x",
    length(posterior_density$reporting_rate), "grid\n")
cat("chw_trial:\n"); print(as.data.frame(chw_trial))

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
