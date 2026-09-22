## ------------------------------------------------------------
## R Coding Support Sessions – Solutions
## 2.A.3 Working with Lists
## ------------------------------------------------------------

# remotes::install_github("mrc-ide/RCodingSupport@teaching-2026")
library(RCodingSupport)

## Object supplied by the package:
analysis_bundle
names(analysis_bundle)   # should be: "data", "summary_stats", "fit"

## ------------------------------------------------------------
## Task 1: Explore the list
## ------------------------------------------------------------
class(analysis_bundle)                   # "list"
typeof(analysis_bundle)                  # "list"
length(analysis_bundle)                  # 3
names(analysis_bundle)                   # element names
str(analysis_bundle, max.level = 1)      # top-level structure

# Element formats (expect: data.frame, named numeric, lm)
class(analysis_bundle$data)
class(analysis_bundle$summary_stats)
class(analysis_bundle$fit)

## Why a list (notes for tutors):
## - Can hold mixed types (data.frame, numeric vector, model object) under one name.
## - Keeps data + results + metadata together for clean workflows.

## ------------------------------------------------------------
## Task 2: Access list elements (two ways)
## ------------------------------------------------------------
# By name:
dat1 <- analysis_bundle$data
sst1 <- analysis_bundle$summary_stats
fit1 <- analysis_bundle$fit

# By index (double brackets return the object itself):
dat2 <- analysis_bundle[[1]]
sst2 <- analysis_bundle[[2]]
fit2 <- analysis_bundle[[3]]

# Sanity check (same objects):
identical(dat1, dat2)
identical(sst1, sst2)
identical(fit1, fit2)

# Demonstrate single vs double brackets:
class(analysis_bundle["data"])   # sub-list
class(analysis_bundle[["data"]]) # data.frame

## ------------------------------------------------------------
## Task 3: Explore the data
## ------------------------------------------------------------
# Quick structure and summary
head(dat1)
str(dat1)
summary(dat1)

# Base scatterplot: 110 villages, ITN coverage vs malaria incidence
plot(dat1$bednet_coverage, dat1$malaria_incidence,
     pch = 19,
     xlab = "Bednet coverage (% of households)",
     ylab = "Malaria incidence (per 1,000 per year)",
     main = "Observed data")

# Tutor note: look for slight curvature, heteroskedasticity, and two outliers.

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
plot(dat1$bednet_coverage, res,
     pch = 19,
     xlab = "Bednet coverage (% of households)",
     ylab = "Residual",
     main = "Residuals vs bednet coverage")
abline(h = 0, lty = 3)

## ------------------------------------------------------------
## Task 5: Add your own notes
## ------------------------------------------------------------
# Add a short character-string summary as a new list element "notes"
analysis_bundle$notes <-
  "Synthetic village survey linking insecticide-treated bednet coverage to malaria incidence. List includes summaries of the data, and results of fitting a linear model relating bednet coverage to incidence."

# Verify the list now has four elements
names(analysis_bundle)
length(analysis_bundle)  # should now be 4
