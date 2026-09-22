# 06_simulate_trial.R
#
# chw_trial: a small malaria intervention trial summary, for the grouped/dodged
# bar-plot + error-bar lesson.
#
# Design: a community health worker (CHW) intervention vs control, evaluated in two
# service settings. Outcome = mean time from fever onset to effective malaria
# treatment (days; lower is better). Character condition labels are used on purpose
# so students meet factor ordering / dodging.
# ------------------------------------------------------------------------------

source(here::here("data-raw", "00_setting.R"))
dir.create(stage_dir, showWarnings = FALSE, recursive = TRUE)

.set_seed()
conditions <- c("Basic services (lower)", "Enhanced services (higher)")
groups     <- c("CHW intervention", "Control")

raw <- tidyr::crossing(condition = conditions, group = groups) |>
  mutate(n_per_cell = sample(c(60, 80, 100), n(), replace = TRUE)) |>
  rowwise() |>
  mutate(
    base_mu = if_else(group == "CHW intervention", 2.4, 3.6),          # CHW shortens delay
    mu      = base_mu - if_else(condition == "Enhanced services (higher)", 0.5, 0.0),
    y       = list(pmax(0.2, rnorm(n_per_cell, mu, 0.9)))
  ) |>
  unnest(y) |>
  ungroup()

chw_trial <- raw |>
  group_by(condition, group) |>
  summarise(mean = mean(y), n = n(), sd = sd(y),
            se = sd / sqrt(n), tcrit = qt(0.975, n - 1),
            ci_lower = mean - tcrit * se, ci_upper = mean + tcrit * se,
            .groups = "drop") |>
  transmute(condition, group = factor(group, levels = groups),
            mean_days_to_treatment = round(mean, 2),
            ci_lower = round(ci_lower, 2), ci_upper = round(ci_upper, 2))

saveRDS(chw_trial, file.path(stage_dir, "chw_trial.rds"))
cat("chw_trial:\n"); print(as.data.frame(chw_trial))

# ---- SANITY: the individual-level data behind the summary, and the target plot
peek("chw_trial: cell sizes and raw outcome")
print(as.data.frame(
  raw |> group_by(condition, group) |>
    summarise(n = n(), target_mu = first(mu), observed = round(mean(y), 2), .groups = "drop")))

group_cols <- c(`CHW intervention` = "steelblue", Control = "grey70")

p_raw <- ggplot(raw, aes(group, y, fill = group)) +
  geom_boxplot(outlier.size = 0.4, show.legend = FALSE) +
  facet_wrap(~ condition) +
  scale_fill_manual(values = group_cols) +
  labs(x = NULL, y = "Days to treatment", title = "Raw outcome per cell") +
  theme(axis.text.x = element_text(angle = 20, hjust = 1))

p_bars <- ggplot(chw_trial, aes(condition, mean_days_to_treatment, fill = group)) +
  geom_col(position = position_dodge(0.8), width = 0.7) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper),
                position = position_dodge(0.8), width = 0.2) +
  scale_fill_manual(values = group_cols) +
  labs(x = NULL, y = "Mean days to treatment", fill = NULL,
       title = "Dodged bars + 95% CI", subtitle = "the week-4 target figure") +
  theme(legend.position = "bottom", axis.text.x = element_text(angle = 20, hjust = 1))

print(p_raw + p_bars)
