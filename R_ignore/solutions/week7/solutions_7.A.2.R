
central_raw <- read.csv(here("data_raw", "ward_usage_central.csv"))
hill_raw <- read.csv(here("data_raw", "ward_usage_hill.csv"))

central_renamed <- central_raw |>
  rename(Hospital_ID = hosp_id,
         Ward = ward,
         Date = dt,
         Beds_occupied = beds_occ,
         Beds_capacity = beds_cap)

hill_renamed <- hill_raw |>
  rename(Hospital_ID = hospital,
         Date = date_reported,
         Ward = ward_name,
         Beds_occupied = n_occupied,
         Beds_capacity = n_beds)


all_wards <- bind_rows(central_renamed, hill_renamed)

all_wards_clean <- all_wards |>
  mutate(Occupancy = Beds_occupied / Beds_capacity,
         High_load = Occupancy > 0.8) |>
  mutate(Hospital_ID = as.factor(Hospital_ID),
         Date = as.Date(Date))

all_wards_clean |>
  ggplot() + theme_bw() +
  geom_point(aes(x = Date, y = Occupancy, color = Hospital_ID)) +
  ylim(c(0, 1))
