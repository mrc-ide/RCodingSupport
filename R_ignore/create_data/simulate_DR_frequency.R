# simulate_DR_frequency.R
#
# Author: Bob Verity
# Date: 2025-10-10
#
# Inputs: (none)
#
# Outputs:
#   - R_ignore/non_package_data/DR_frequency.rds
#
# Purpose:
#   Simulate synthetic allele-frequency trajectories for two drug-resistance
#   mutations across multiple populations using a Wright–Fisher model.
#   Observed prevalence data are sampled at discrete timepoints, and posterior
#   distributions of true allele frequency are inferred using a Hidden Markov
#   Model (HMM). The script:
#     - Simulates true frequency trajectories under selection (sim_WF)
#     - Samples noisy observed data (sample_prev)
#     - Computes posterior prevalence distributions (get_posterior_prev)
#     - Summarises credible intervals and joins data across mutations and populations
#     - Produces a quick diagnostic plot of model fits
#     - Saves a list containing:
#         $data  : observed prevalence data with credible intervals (%)
#         $model : posterior frequency output for plotting ribbons
#
# ------------------------------------------------------------------

#remotes::install_github("mrc-ide/genecastR@v1.0.3")
library(genecastR)

set.seed(1)

# define parameters
s <- c(1.0, -0.3)
p0 <- c(0.05, 0.6)
N <- 1e3
generations <- 52*2
t_step <- 7
n_samp <- 50
n_pop <- 3
pop_name <- c("Agago", "Katakwi", "Tororo")
mutation_name <- c("k13-A675V", "dhps-K540E")

# run over each mutation
data_list <- post_list <- list()
for (i in seq_along(s)) {

  # simulate prevalence
  df_sim_WF <- sim_WF(s = s[i], p0 = p0[i], N = N, generations = generations,
                      n_pop = n_pop, t_step = t_step) |>
    mutate(pop = pop_name[pop])

  # sample data
  df_sample <- expand_grid(pop = pop_name, gen = seq(0, 52*5, by = 16), n_samp = n_samp)
  df_data <- sample_prev(df_sim = df_sim_WF, df_sample = df_sample) |>
    mutate(i = i)

  # run HMM
  df_post <- get_posterior_prev(df_data = df_data,
                                s = s[i],
                                sigma <- 0.25,
                                dt = 7,
                                dx = 0.001,
                                t_end = as.Date("2023-01-01")) |>
    mutate(mutation = mutation_name[i], .before = 1)

  data_list[[i]] <- df_data
  post_list[[i]] <- df_post
}

# finish of data frames
df_data <- data_list |>
  bind_rows() |>
  get_CIs() |>
  mutate(p_est = 1e2*p_est,
         CI_lower = 1e2*CI_lower,
         CI_upper = 1e2*CI_upper,
         mutation = mutation_name[i], .before = 1)

# get ribbons from raw output
df_CIs <- post_list |>
  bind_rows() |>
  group_by(mutation, pop, t) |>
  arrange(p) |>
  summarise(Q2.5 = 1e2*p[which(cumsum(post) > 0.025)[1]],
            Q25 = 1e2*p[which(cumsum(post) > 0.25)[1]],
            Q75 = 1e2*p[which(cumsum(post) > 0.75)[1]],
            Q97.5 = 1e2*p[which(cumsum(post) > 0.975)[1]])

# plot
df_data |>
  ggplot() + theme_bw() +
  geom_ribbon(aes(x = t, ymin = Q2.5, ymax = Q97.5), data = df_CIs,
              fill = "dodgerblue", alpha = 0.3) +
  geom_ribbon(aes(x = t, ymin = Q25, ymax = Q75), data = df_CIs,
              fill = "dodgerblue", alpha = 0.5) +
  geom_pointrange(aes(x = t, y = p_est, ymin = CI_lower, ymax = CI_upper), size = 0.2) +
  facet_grid(mutation ~ pop) +
  scale_y_continuous(limits = c(0, 100))

# tidy up for saving
df_data <- df_data |>
  dplyr::select(-gen, -i) |>
  mutate(mutation = factor(mutation, levels = mutation_name),
         pop = factor(pop, levels = pop_name))

df_CIs <- df_CIs |>
  mutate(mutation = factor(mutation, levels = mutation_name),
         pop = factor(pop, levels = pop_name))

# save to file
saveRDS(list(data = df_data, model = df_CIs),
        file = here("R_ignore", "non_package_data", "DR_frequency.rds"))
