## ------------------------------------------------------------
## R Coding Support Sessions – Solutions
## 2.A.3 Working with Lists
## ------------------------------------------------------------

# remotes::install_github("mrc-ide/RCodingSupport@v1.0.0")
library(RCodingSupport)

## Object supplied by the package:
smoking_analysis_list
names(smoking_analysis_list)   # should be: "data", "summary_stats", "fit"

## ------------------------------------------------------------
## Task 1: Explore the list
## ------------------------------------------------------------
class(smoking_analysis_list)                   # "list"
typeof(smoking_analysis_list)                  # "list"
length(smoking_analysis_list)                  # 3
names(smoking_analysis_list)                   # element names
str(smoking_analysis_list, max.level = 1)      # top-level structure

# Element formats (expect: data.frame, named numeric, lm)
class(smoking_analysis_list$data)
class(smoking_analysis_list$summary_stats)
class(smoking_analysis_list$fit)

## Why a list (notes for tutors):
## - Can hold mixed types (data.frame, numeric vector, model object) under one name.
## - Keeps data + results + metadata together for clean workflows.

## ------------------------------------------------------------
## Task 2: Access list elements (two ways)
## ------------------------------------------------------------
# By name:
dat1 <- smoking_analysis_list$data
sst1 <- smoking_analysis_list$summary_stats
fit1 <- smoking_analysis_list$fit

# By index (double brackets return the object itself):
dat2 <- smoking_analysis_list[[1]]
sst2 <- smoking_analysis_list[[2]]
fit2 <- smoking_analysis_list[[3]]

# Sanity check (same objects):
identical(dat1, dat2)
identical(sst1, sst2)
identical(fit1, fit2)

# Demonstrate single vs double brackets:
class(smoking_analysis_list["data"])   # sub-list
class(smoking_analysis_list[["data"]]) # data.frame

## ------------------------------------------------------------
## Task 3: Explore the data
## ------------------------------------------------------------
# Quick structure and summary
head(dat1)
str(dat1)
summary(dat1)

# Base scatterplot
plot(dat1$smoking_prev, dat1$lung_cancer_incidence,
     pch = 19,
     xlab = "Smoking prevalence (%)",
     ylab = "Lung cancer incidence (per 100k)",
     main = "Observed data")

# Tutor note: look for slight curvature, heteroskedasticity, outliers.

## ------------------------------------------------------------
## Task 4: Explore the fitted model
## ------------------------------------------------------------

# Basic view of fit object
fit1

# Coefficients (intercept and slope)
coef(fit1)

# Detailed summary, including p-values and R-squared
summary(fit1)

# Add fitted line to the scatterplot
abline(fit1, col = 2)

# Where is the model best/worst?
# (Informally: inspect residuals across x; optionally:)
res <- residuals(fit1)
fitted_vals <- fitted(fit1)

# Quick residual look:
plot(dat1$smoking_prev, res,
     pch = 19,
     xlab = "Smoking prevalence (%)",
     ylab = "Residual",
     main = "Residuals vs Smoking prevalence")
abline(h = 0, lty = 3)

## ------------------------------------------------------------
## Task 5: Add your own notes
## ------------------------------------------------------------
# Add a short character-string summary as a new list element "notes"
smoking_analysis_list$notes <-
  "Synthetic dataset linking smoking prevalence to lung cancer incidence. List includes summaries on the data, and results of fitting a linear model relating smoking prevalence to cancer incidence."

# Verify the list now has four elements
names(smoking_analysis_list)
length(smoking_analysis_list)  # should now be 4

