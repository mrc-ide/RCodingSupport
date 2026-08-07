# 05_simulate_analysis_objects.R
#
# "Analysis output" objects for the malaria scenario — the datasets that teach
# students to handle the complex objects statistical work hands back.
#
#   analysis_bundle    a list(data, summary_stats, fit) from a village survey of
#                      bednet coverage vs malaria incidence (list-navigation + model
#                      object lesson; replaces the old smoking_analysis_list).
#   posterior_density  a list(R0, reporting_rate, density) 2D posterior surface from
#                      fitting the transmission model (raster/contour + colour-scale
#                      lesson; replaces the old posterior_density KDE grid).
# ------------------------------------------------------------------------------

source("00_setting.R")
suppressPackageStartupMessages(library(MASS))
stage_dir <- Sys.getenv("STAGE_DIR", unset = file.path(getwd(), "staged_data"))
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
