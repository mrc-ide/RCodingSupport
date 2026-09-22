# 3A1_import_solution.R
# ------------------------------------------------------------
# Purpose: Import the provided datasets, check their structure,
#          and make short notes on their provenance.
# ------------------------------------------------------------

# All files live inside ./data_raw relative to the project root

## --- 1) weekly_cases.rds -----------------------------------------------------
cases <- readRDS("data_raw/weekly_cases.rds")
class(cases)      # "data.frame"
dim(cases)        # 260 rows, 3 columns (year, week, cases)
head(cases)

## Notes:
# - Saved as a single R object in .rds format.
# - Data frame of weekly case counts over five years (2020-2024).
# - Variables: year, week, cases.

## --- 2) weekly_tests.txt (tab-delimited) ------------------------------------
tests <- read.table("data_raw/weekly_tests.txt", sep = "\t", header = TRUE)
class(tests)      # "data.frame"
dim(tests)        # 260 rows, 3 columns (year, week, tested)
head(tests)

## Notes:
# - Tab-delimited text file with a header row (header = TRUE is essential;
#   without it every column is read as character and the header becomes row 1).
# - Similar structure to weekly_cases: year, week, tested.
# - Same number of rows; should align week-by-week with cases.

## --- 3) hospital_admissions.csv ---------------------------------------------
hosp <- read.csv("data_raw/hospital_admissions.csv")
class(hosp)       # "data.frame"
dim(hosp)         # 250 rows, 7 columns
str(hosp)

## Notes:
# - Comma-separated file (CSV).
# - Columns include patient_id, hospital, admit/discharge dates,
#   age, icu_flag, outcome.
# - Some columns may not import in the ideal type:
#   * admit/discharge dates are characters (need conversion with as.Date).
#   * icu_flag coded as "Y"/"N" (students can convert to logical).
#   * outcome is free text (convert to factor if desired).

## --- 4) misc_objects.RData --------------------------------------------------
load("data_raw/misc_objects.RData")
# Objects now in environment: surveillance_regions, lab_turnaround, metadata
ls()

class(surveillance_regions)   # "character"
class(lab_turnaround)         # "data.frame"
class(metadata)               # "list"

## Notes:
# - .RData bundles multiple objects in a single file.
# - Contents include:
#   * surveillance_regions: simple character vector
#   * lab_turnaround: data frame of lab metrics
#   * metadata: list with data source and a contact address
