## ------------------------------------------------------------
## R Coding Support Sessions – Solutions
## 2.A.2 Working with Data Frames
## ------------------------------------------------------------

# remotes::install_github("mrc-ide/RCodingSupport@teaching-2026")
library(RCodingSupport)

## ------------------------------------------------------------
## Task 1: Inspect the data
## ------------------------------------------------------------

# The dataset
patient_records <- RCodingSupport::patient_records

# Quick looks
head(patient_records)
tail(patient_records)
str(patient_records)

# Dimensions (use functions, not scrolling)
nrow(patient_records)
ncol(patient_records)

# Classes of each column (alternative to str())
sapply(patient_records, class)

## ------------------------------------------------------------
## Task 2: Fix some issues with column names
## ------------------------------------------------------------
# See current names
names(patient_records)

# Example: rename specific columns you think could be clearer.
# (Adjust as you prefer; keep a consistent style.)
names(patient_records)   <- c("Patient_ID", "Age", "Sex", "Height_cm", "Weight_kg", "Ethnicity", "Bednet")

# Check
names(patient_records)

## ------------------------------------------------------------
## Task 3: Fix some issues with variable classes
## ------------------------------------------------------------
# ethnicity is character -> convert to factor
class(patient_records$Ethnicity)           # before
patient_records$Ethnicity <- as.factor(patient_records$Ethnicity)
class(patient_records$Ethnicity)           # after
levels(patient_records$Ethnicity)

## ------------------------------------------------------------
## Task 4: Calculate BMI in a new column
## ------------------------------------------------------------
# BMI = weight (kg) / height (m)^2
# Current height is in cm, so convert to meters:
patient_records$BMI <- patient_records$Weight_kg / ((patient_records$Height_cm / 100) ^ 2)

# Quick range of BMI (ignore any potential NAs)
range(patient_records$BMI, na.rm = TRUE)

## ------------------------------------------------------------
## Task 5: Scatterplot of age vs BMI
## ------------------------------------------------------------
cols <- as.integer(patient_records$Ethnicity)  # simple color mapping by ethnicity

plot(patient_records$Age, patient_records$BMI,
     xlab = "Age (years)", ylab = "BMI (kg/m^2)",
     main = "Age vs BMI (colored by ethnicity)",
     pch = 19, cex = 0.9, col = cols)

# Add legend
legend("topright", legend = levels(patient_records$Ethnicity),
       col = 1:length(levels(patient_records$Ethnicity)),
       pch = 19, cex = 0.8, title = "Ethnicity")
