# 05_simulate_analysis_objects.R
#
# "Analysis output" objects for the malaria scenario — the datasets that teach
# students to handle the complex objects statistical work hands back.
#
#   analysis_bundle    a list(data, summary_stats, fit) from a village survey of
#                      bednet coverage vs malaria incidence (list-navigation +
#                      model object lesson).
#   posterior_density  a list(R0, reporting_rate, density) 2D posterior surface from
#                      fitting the transmission model (raster/contour + colour-scale
#                      lesson).
# ------------------------------------------------------------------------------

source(here::here("data-raw", "00_setting.R"))
dir.create(stage_dir, showWarnings = FALSE, recursive = TRUE)

# ==============================================================================
# 1) analysis_bundle: 110-village survey, bednet coverage vs malaria incidence
# ==============================================================================
.set_seed()
n_vill <- 110
bednet_coverage <- sort(runif(n_vill, 10, 85))                 # % of households with an ITN
# incidence falls with coverage; mild curvature + heteroskedastic noise + 2 outliers
true_mu <- 420 - 4.2 * bednet_coverage + 0.02 * bednet_coverage^2
sd_eps  <- 20 + 0.6 * bednet_coverage
malaria_incidence <- pmax(true_mu + rnorm(n_vill, 0, sd_eps), 0)   # per 1000 per year
out_idx <- sample(n_vill, 2)                                       # two visible outliers
malaria_incidence[out_idx] <- malaria_incidence[out_idx] + c(120, 160)

survey <- data.frame(bednet_coverage = bednet_coverage,
                     malaria_incidence = malaria_incidence)
fit <- lm(malaria_incidence ~ bednet_coverage, data = survey)
summary_stats <- c(
  bednet_coverage_mean   = mean(survey$bednet_coverage),
  bednet_coverage_sd     = sd(survey$bednet_coverage),
  malaria_incidence_mean = mean(survey$malaria_incidence),
  malaria_incidence_sd   = sd(survey$malaria_incidence)
)
analysis_bundle <- list(data = survey, summary_stats = summary_stats, fit = fit)
saveRDS(analysis_bundle, file.path(stage_dir, "analysis_bundle.rds"))
cat(sprintf("analysis_bundle: %d villages | slope = %.2f cases/1000 per %% ITN (R2=%.2f)\n",
            n_vill, coef(fit)[2], summary(fit)$r.squared))

# ---- SANITY: what 2.A.3 asks students to look at -----------------------------
peek("analysis_bundle")
print(round(summary_stats, 1))
print(summary(fit)$coefficients)

survey_plot <- survey |>
  mutate(outlier = seq_len(n_vill) %in% out_idx,
         true_mu = true_mu, resid = residuals(fit))

p_fit <- ggplot(survey_plot, aes(bednet_coverage, malaria_incidence)) +
  geom_line(aes(y = true_mu), colour = "grey50", linetype = 2) +
  geom_point(aes(colour = outlier), size = 1.5, show.legend = FALSE) +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE, colour = "steelblue", linewidth = 0.7) +
  scale_colour_manual(values = c(`FALSE` = "black", `TRUE` = "red")) +
  labs(x = "Bednet coverage (%)", y = "Malaria incidence (per 1,000/yr)",
       title = "Survey data and lm fit",
       subtitle = "red = planted outliers; grey dashed = true (curved) mean")

p_res <- ggplot(survey_plot, aes(bednet_coverage, resid)) +
  geom_hline(yintercept = 0, linetype = 3) +
  geom_point(size = 1.5) +
  labs(x = "Bednet coverage (%)", y = "Residual",
       title = "Residuals", subtitle = "spread widens with coverage (heteroskedastic by design)")

p_qq <- ggplot(survey_plot, aes(sample = resid)) +
  stat_qq(size = 1.2) + stat_qq_line() +
  labs(x = "Theoretical quantiles", y = "Sample quantiles",
       title = "Residual QQ", subtitle = "outliers in the upper tail")

print(p_fit + (p_res / p_qq))

# ==============================================================================
# 2) posterior_density: 2D posterior over (R0, reporting_rate) from the model fit
# ==============================================================================
.set_seed()
# correlated, mildly multi-modal posterior draws
w <- c(0.6, 0.4)
d1 <- MASS::mvrnorm(4800, mu = c(2.4, 0.35), Sigma = matrix(c(0.09, 0.010, 0.010, 0.0020), 2))
d2 <- MASS::mvrnorm(3200, mu = c(2.9, 0.28), Sigma = matrix(c(0.06,-0.006,-0.006, 0.0015), 2))
draws <- rbind(d1, d2)
kde <- MASS::kde2d(draws[,1], draws[,2], n = 150,
                   lims = c(1.5, 4.0, 0.15, 0.55))
posterior_density <- list(R0 = kde$x, reporting_rate = kde$y, density = kde$z)
saveRDS(posterior_density, file.path(stage_dir, "posterior_density.rds"))
cat(sprintf("posterior_density: %d x %d grid over R0[%.1f,%.1f] x reporting_rate[%.2f,%.2f]\n",
            length(kde$x), length(kde$y), min(kde$x), max(kde$x), min(kde$y), max(kde$y)))

# ---- SANITY: the 4.A.2 surface -----------------------------------------------
peek("posterior_density")
cat(sprintf("draws: R0 mean %.2f (sd %.2f), reporting_rate mean %.3f (sd %.3f), cor %.2f\n",
            mean(draws[, 1]), sd(draws[, 1]), mean(draws[, 2]), sd(draws[, 2]),
            cor(draws[, 1], draws[, 2])))
peak <- which(kde$z == max(kde$z), arr.ind = TRUE)
cat(sprintf("density peak at R0 = %.2f, reporting_rate = %.3f\n", kde$x[peak[1]], kde$y[peak[2]]))

p_draws <- data.frame(R0 = draws[, 1], reporting_rate = draws[, 2]) |>
  ggplot(aes(R0, reporting_rate)) +
  geom_point(size = 0.3, alpha = 0.15) +
  coord_cartesian(xlim = c(1.5, 4), ylim = c(0.15, 0.55)) +
  labs(title = "Raw posterior draws", subtitle = "two overlapping modes")

grid_df <- expand.grid(R0 = kde$x, reporting_rate = kde$y)
grid_df$density <- as.vector(kde$z)
p_kde <- ggplot(grid_df, aes(R0, reporting_rate)) +
  geom_raster(aes(fill = density)) +
  geom_contour(aes(z = density), colour = "white", linewidth = 0.2, bins = 8) +
  scale_fill_viridis_c(option = "inferno") +
  scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0)) +
  labs(title = "KDE surface", subtitle = "what students plot") +
  theme(legend.position = "bottom", legend.key.height = unit(3, "mm"))

print(p_draws + p_kde)
