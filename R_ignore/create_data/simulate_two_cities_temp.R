
library(here)
library(lubridate)

set.seed(1)

dat <- readRDS(here("R_ignore/non_package_data/two_cities_cases.rds"))

dat |>
  ggplot() + theme_bw() +
  geom_point(aes(x = date, y = cases / tested, col = city))

dat <- dat |>
  mutate(year = year(date),
         month = month(date))

# make data frames of temperature in each city
df_temp_Riverton <- expand_grid(year = 2016:2019, month = 1:12) |>
  mutate(y = (month - 1) / 11,
         temp = 1 - sin(2*pi*(y^1.3 + 0.2)),
         temp = temp + rnorm(48, sd = 0.2),
         temp = 1 + temp * 8,
         city = "Riverton")

df_temp_Riverton |>
  ggplot() + theme_bw() +
  geom_line(aes(x = month, y = temp, col = as.factor(year))) +
  ggtitle("Riverton")

df_temp_Stonebridge <- expand_grid(year = 2016:2019, month = 1:12) |>
  mutate(y = (month - 1) / 11,
         temp = 1 - sin(2*pi*(y^1.1 + 0.2)),
         temp = temp + rnorm(48, sd = 0.2),
         temp = 6 + temp * 8,
         city = "Stonebridge")

df_temp_Stonebridge |>
  ggplot() + theme_bw() +
  geom_line(aes(x = month, y = temp, col = as.factor(year))) +
  ggtitle("Stonebridge")

# combine and finalize
months_vec <- c("January", "February", "March", "April", "May", "June", "July",
                "August", "September", "October", "November", "December")
df_temp_combined <- df_temp_Riverton |>
  bind_rows(df_temp_Stonebridge) |>
  mutate(month = months_vec[month],
         month = factor(month, levels = months_vec, ordered = TRUE)) |>
  select(city, year, month, temp) |>
  rename(temp_celcius = temp)

dat |>
  mutate(month = months(date),
         prev = cases / tested) |>
  left_join(df_temp_combined) |>
  ggplot() +
  geom_point(aes(x = temp_celcius, y = prev, col = city))

# deliberately mess up the names to make them inconsistent between datasets
df_temp_combined <- df_temp_combined |>
  rename(City = city,
         Year = year,
         Month = month,
          Temp_celcius = temp_celcius)

# save to file
saveRDS(df_temp_combined, file =here("R_ignore/non_package_data", "two_cities_temp.rds"))
