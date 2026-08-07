# 04_simulate_genetics.R
#
# Molecular datasets for the malaria scenario. Both sit in the same fictional world
# (parasite samples collected from cases at the sentinel sites defined in 00_setting.R).
#
#   allele_freq_matrix       loci x samples SNP allele-frequency matrix, with lab
#                            artefacts and structured missingness (matrix-cleaning lesson)
#   resistance_trajectories  observed prevalence + modelled credible bands for two
#                            drug-resistance markers across sentinel sites over time
#                            (faceting / ribbon-plot lesson). Self-contained: no
#                            external packages, simulated for the FICTIONAL sites
#                            (replaces the old real-Uganda DR_frequency dataset).
# ------------------------------------------------------------------------------

source("00_setting.R")
stage_dir <- Sys.getenv("STAGE_DIR", unset = file.path(getwd(), "staged_data"))
dir.create(stage_dir, showWarnings = FALSE, recursive = TRUE)

# ==============================================================================
# 1) allele_freq_matrix  (300 parasite SNP loci x 200 samples)
# ==============================================================================
.set_seed()
n_loci <- 300; n_samples <- 200
logit <- function(p) log(p / (1 - p)); inv_logit <- function(x) 1 / (1 + exp(-x))

loc_base     <- rbeta(n_loci, 1, 6)                       # per-locus base frequency
sample_noise <- rnorm(n_samples, 0, 0.25)
allele_freq_matrix <- inv_logit(outer(logit(loc_base), sample_noise, `+`))
rownames(allele_freq_matrix) <- sprintf("locus_%04d", seq_len(n_loci))
colnames(allele_freq_matrix) <- sprintf("sample_%03d", seq_len(n_samples))

# lab normalisation artefacts (a few negative values)
n_art <- round(0.01 * n_loci * n_samples)
idx_art <- cbind(sample(n_loci, n_art, TRUE), sample(n_samples, n_art, TRUE))
allele_freq_matrix[idx_art] <- -runif(n_art, 0, 0.10)

# structured missingness: bad loci, bad samples, background
bad_loci    <- sample(n_loci,    round(0.10 * n_loci))
bad_samples <- sample(n_samples, round(0.10 * n_samples))
is_na <- matrix(runif(n_loci * n_samples) < 0.03, n_loci, n_samples)
is_na[bad_loci, ]    <- is_na[bad_loci, ]    | (matrix(runif(length(bad_loci)*n_samples),   length(bad_loci))  < 0.45)
is_na[, bad_samples] <- is_na[, bad_samples] | (matrix(runif(n_loci*length(bad_samples)),   n_loci)            < 0.35)
allele_freq_matrix[is_na] <- NA_real_

saveRDS(allele_freq_matrix, file.path(stage_dir, "allele_freq_matrix.rds"))
cat(sprintf("allele_freq_matrix: %d x %d | %.1f%% missing | %d artefacts (<0)\n",
            nrow(allele_freq_matrix), ncol(allele_freq_matrix),
            100*mean(is.na(allele_freq_matrix)), sum(allele_freq_matrix < 0, na.rm = TRUE)))

# ==============================================================================
# 2) resistance_trajectories  (2 markers x sentinel sites, over the study window)
# ==============================================================================
.set_seed()
markers <- tibble(
  marker = c("kelch13 (C469Y)", "dhps (K540E)"),
  p0     = c(0.04, 0.35),     # starting prevalence
  s      = c(0.9, -0.2)       # selection: one spreading, one slowly declining (per year)
)
sites   <- sentinel_sites
obs_dates  <- seq(start_date, start_date + n_weeks * 7, by = 26 * 7)   # ~biannual
fine_dates <- seq(start_date, start_date + n_weeks * 7, by = 7)        # weekly grid
yr <- function(d) as.numeric(d - start_date) / 365.25

p_true <- function(p0, s, d, site_shift) inv_logit(logit(p0) + s * yr(d) + site_shift)

wilson <- function(x, n, z = 1.96) {
  p <- x / n; denom <- 1 + z^2/n
  c(lower = (p + z^2/(2*n) - z*sqrt(p*(1-p)/n + z^2/(4*n^2)))/denom,
    upper = (p + z^2/(2*n) + z*sqrt(p*(1-p)/n + z^2/(4*n^2)))/denom)
}

data_rows <- list(); model_rows <- list(); k <- 1; m <- 1
for (i in seq_len(nrow(markers))) {
  for (site in sites) {
    site_shift <- rnorm(1, 0, 0.3)
    # observed data at sampling dates
    for (d in obs_dates) {
      d <- as.Date(d, origin = "1970-01-01")
      n_samp <- sample(40:80, 1)
      p <- p_true(markers$p0[i], markers$s[i], d, site_shift)
      x <- rbinom(1, n_samp, p)
      ci <- wilson(x, n_samp)
      data_rows[[k]] <- tibble(marker = markers$marker[i], site = site, t = d,
                               n_tested = n_samp, n_positive = x,
                               p_est = 100 * x / n_samp,
                               CI_lower = unname(100 * ci["lower"]),
                               CI_upper = unname(100 * ci["upper"]))
      k <- k + 1
    }
    # modelled credible band on the fine grid (Beta band around the true trajectory)
    pt <- p_true(markers$p0[i], markers$s[i], fine_dates, site_shift)
    kk <- 45  # effective sample size controlling band width
    a <- kk * pt + 1; b <- kk * (1 - pt) + 1
    model_rows[[m]] <- tibble(
      marker = markers$marker[i], site = site, t = fine_dates,
      Q2.5  = 100 * qbeta(0.025, a, b), Q25 = 100 * qbeta(0.25, a, b),
      Q75   = 100 * qbeta(0.75,  a, b), Q97.5 = 100 * qbeta(0.975, a, b))
    m <- m + 1
  }
}

fac_m <- function(x) factor(x, levels = markers$marker)
fac_s <- function(x) factor(x, levels = sites)
resistance_trajectories <- list(
  data  = bind_rows(data_rows)  %>% mutate(marker = fac_m(marker), site = fac_s(site)),
  model = bind_rows(model_rows) %>% mutate(marker = fac_m(marker), site = fac_s(site))
)
saveRDS(resistance_trajectories, file.path(stage_dir, "resistance_trajectories.rds"))
cat(sprintf("resistance_trajectories: %d markers x %d sites | %d obs rows | %d model rows\n",
            nrow(markers), length(sites),
            nrow(resistance_trajectories$data), nrow(resistance_trajectories$model)))
