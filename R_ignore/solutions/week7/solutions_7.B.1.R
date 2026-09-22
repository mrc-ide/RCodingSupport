
# read in data
dat <- readRDS(here("data_raw", "two_cities_cases.rds")) |>
  mutate(year = year(date),
         week = week(date),
         positivity = 100 * cases / tested)

# quick exploratory plot
dat |>
  ggplot() + theme_bw() +
  geom_point(aes(x = date, y = positivity, col = city))

# filter to 2024 (the final year)
dat_2024 <- dat |>
  filter(year == 2024)

# filter to before 2024
dat_previous <- dat |>
  filter(year < 2024)

# get 95% prediction interval
dat_prediction <- dat_previous |>
  group_by(city, week) |>
  summarise(mean = mean(positivity),
            SD = sd(positivity)) |>
  mutate(min = mean - 1.96*SD,
         max = mean + 1.96*SD)

# plot prediction and overlay 2024 points
dat_prediction |>
  ggplot() + theme_bw() +
  geom_ribbon(aes(x = week, ymin = min, ymax = max, fill = city), alpha = 0.5) +
  geom_point(aes(x = week, y = positivity, col = city), data = dat_2024) +
  facet_wrap(~city)

