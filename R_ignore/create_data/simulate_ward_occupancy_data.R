# simulate_ward_occupancy_data.R
#
# Author: Bob Verity
# Date: 2025-11-11
#
# Inputs: (none)
#
# Outputs:
#   - ward_usage_central.csv
#   - ward_usage_hill.csv
#
# Purpose:
#   Simulate daily ward occupancy data for two hospitals, using deliberately
#   inconsistent column names across files to motivate:
#     - rename()
#     - bind_rows()
#     - mutate() to calculate occupancy and flags
#
# ------------------------------------------------------------------

library(here)

set.seed(1)

# ------------------------------------------------------------------
# Helper function to generate ward-level occupancy for one hospital
# ------------------------------------------------------------------
simulate_hospital <- function(
    hospital_id,
    dates,
    wards,
    capacities,
    mean_occupancy = 0.8
) {
  stopifnot(length(wards) == length(capacities))

  df <- expand.grid(
    dt    = dates,
    ward  = wards,
    KEEP.OUT.ATTRS = FALSE
  )

  df$hosp_id   <- hospital_id
  df$beds_cap  <- capacities[match(df$ward, wards)]

  # simulate occupied beds using a binomial model around mean_occupancy
  df$beds_occ <- rbinom(
    n     = nrow(df),
    size  = df$beds_cap,
    prob  = mean_occupancy + rnorm(nrow(df), mean = 0, sd = 0.05)
  )
  df$beds_occ <- pmax(pmin(df$beds_occ, df$beds_cap), 0)

  df
}

# ------------------------------------------------------------------
# Central Hospital data (messy names: hosp_id, ward, dt, beds_occ, beds_cap)
# ------------------------------------------------------------------

dates_central <- seq(as.Date("2023-01-01"), as.Date("2023-01-30"), by = "day")
wards_central <- c("ICU", "Respiratory", "General_Med")
caps_central  <- c(12, 24, 40)

central_raw <- simulate_hospital(
  hospital_id    = "CENTRAL",
  dates          = dates_central,
  wards          = wards_central,
  capacities     = caps_central,
  mean_occupancy = 0.82
)

# Keep only columns in the "messy" order and names you want
central_out <- central_raw[, c("hosp_id", "ward", "dt", "beds_occ", "beds_cap")]

# Save to CSV
write.csv(central_out, here("R_ignore", "non_package_data", "ward_usage_central.csv"), row.names = FALSE)

# ------------------------------------------------------------------
# Hill County Hospital data
# messy names: hospital, ward_name, date_reported, n_occupied, n_beds
# ------------------------------------------------------------------

dates_hill <- seq(as.Date("2023-01-05"), as.Date("2023-02-03"), by = "day")
wards_hill <- c("ICU", "Resp", "Gen_Med")
caps_hill  <- c(10, 20, 36)

hill_raw <- simulate_hospital(
  hospital_id    = "HILL_COUNTY",
  dates          = dates_hill,
  wards          = wards_hill,
  capacities     = caps_hill,
  mean_occupancy = 0.78
)

# Now rename columns into the *other* messy naming scheme
hill_out <- data.frame(
  date_reported = hill_raw$dt,
  hospital      = hill_raw$hosp_id,
  ward_name     = hill_raw$ward,
  n_occupied    = hill_raw$beds_occ,
  n_beds        = hill_raw$beds_cap
)

write.csv(hill_out, here("R_ignore", "non_package_data", "ward_usage_hill.csv"), row.names = FALSE)

