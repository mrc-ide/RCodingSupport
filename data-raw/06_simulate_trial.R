# 06_simulate_trial.R
#
# chw_trial: a small malaria intervention trial summary, for the grouped/dodged
# bar-plot + error-bar lesson (replaces the old CHW newborn-birthweight trial).
#
# Design: a community health worker (CHW) intervention vs control, evaluated in two
# service settings. Outcome = mean time from fever onset to effective malaria
# treatment (days; lower is better). Character condition labels are used on purpose
# so students meet factor ordering / dodging.
# ------------------------------------------------------------------------------

source("00_setting.R")
suppressPackageStartupMessages(library(dplyr))
stage_dir <- Sys.getenv("STAGE_DIR", unset = file.path(getwd(), "staged_data"))
dir.create(stage_dir, showWarnings = FALSE, recursive = TRUE)

.set_seed()
conditions <- c("Basic services (lower)", "Enhanced services (higher)")
groups     <- c("CHW intervention", "Control")

raw <- tidyr::crossing(condition = conditions, group = groups) %>%
  mutate(n_per_cell = sample(c(60, 80, 100), n(), replace = TRUE)) %>%
  rowwise() %>%
  mutate(
    base_mu = if_else(group == "CHW intervention", 2.4, 3.6),          # CHW shortens delay
    mu      = base_mu - if_else(condition == "Enhanced services (higher)", 0.5, 0.0),
    y       = list(pmax(0.2, rnorm(n_per_cell, mu, 0.9)))
  ) %>%
  unnest(y) %>%
  ungroup()

chw_trial <- raw %>%
  group_by(condition, group) %>%
  summarise(mean = mean(y), n = n(), sd = sd(y),
            se = sd / sqrt(n), tcrit = qt(0.975, n - 1),
            ci_lower = mean - tcrit * se, ci_upper = mean + tcrit * se,
            .groups = "drop") %>%
  transmute(condition, group = factor(group, levels = groups),
            mean_days_to_treatment = round(mean, 2),
            ci_lower = round(ci_lower, 2), ci_upper = round(ci_upper, 2))

saveRDS(chw_trial, file.path(stage_dir, "chw_trial.rds"))
cat("chw_trial:\n"); print(as.data.frame(chw_trial))
