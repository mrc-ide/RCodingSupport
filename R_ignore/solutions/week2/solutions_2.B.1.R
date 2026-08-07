## 2.B.1 – Combining District Case Reports — Solutions
## ---------------------------------------------------

## remotes::install_github("mrc-ide/RCodingSupport@v1.0.0")
library(RCodingSupport)

## ---------------------------------------------------
## Task 1: Explore each dataset
## ---------------------------------------------------

# Unpack the list into two data.frames
dfA <- district_weekly_list$A
dfB <- district_weekly_list$B

# Basic exploration
head(dfA); tail(dfA); summary(dfA); dim(dfA)
head(dfB); tail(dfB); summary(dfB); dim(dfB)

# Add weekly prevalence
dfA$prev_A <- dfA$cases_A / dfA$tested_A
dfB$prev_B <- dfB$cases_B / dfB$tested_B

# Simple line plots (separate)
plot(dfA$prev_A, type = "l",
     xlab = "Time (weeks)", ylab = "Prevalence", main = "District A prevalence")
plot(dfB$prev_B, type = "l",
     xlab = "Time (weeks)", ylab = "Prevalence", main = "District B prevalence")


## ---------------------------------------------------
## Task 2: Align and combine the datasets
## ---------------------------------------------------
## Look at year/week ranges to understand overlap:
range(dfA$year); range(dfB$year)
range(dfA$week); range(dfB$week)

## Merge on both keys; by default merge() keeps only matching
## (overlapping) year-week pairs when all=FALSE (the default).
df_combined <- merge(dfA, dfB)

# Check result structure and that non-overlapping rows from A are dropped
head(df_combined); tail(df_combined); dim(df_combined)


## ---------------------------------------------------
## Task 3: Calculate combined prevalence
## ---------------------------------------------------

# Totals (numerator and denominator)
df_combined$total_cases  <- df_combined$cases_A  + df_combined$cases_B
df_combined$total_tested <- df_combined$tested_A + df_combined$tested_B

# Combined prevalence
df_combined$prev_total <- df_combined$total_cases / df_combined$total_tested

## ---------------------------------------------------
## Task 4: Subset by year and plot
## ---------------------------------------------------

df_y <- subset(df_combined, year >= 2018)

# Plot combined prevalence over weeks in that year
plot(df_y$prev_total, type = "l",
     xlab = "Time (weeks)", ylab = "Combined prevalence", main = "Combined prevalence")

