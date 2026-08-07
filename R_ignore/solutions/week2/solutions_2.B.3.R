## 2.B.3 – Creating a Vaccine Trial Dataset — Solutions
## ----------------------------------------------------

## remotes::install_github("mrc-ide/RCodingSupport@v1.0.0")
library(RCodingSupport)

## ----------------------------------------------------
## Task 1: Build the component vectors
## ----------------------------------------------------
sites   <- c("site1", "site2")
visits  <- seq(from = 0, to = 20, by = 4)   # 0, 4, 8, 12, 16, 20
index   <- 1:50

## ----------------------------------------------------
## Task 2: Build the database with expand.grid()
## ----------------------------------------------------
df <- expand.grid(site = sites,
                  visit = visits,
                  index = index)

## Expected row count = length(sites) * length(visits) * length(index)
length(sites) * length(visits) * length(index)
nrow(df)

## ----------------------------------------------------
## Task 3: Create a combined participant ID
## ----------------------------------------------------
df$id <- sprintf("%s_%s", df$site, df$index)

## ----------------------------------------------------
## Task 4: Assign trial arms
## ----------------------------------------------------
df$arm <- ifelse(df$site == "site1", "placebo", "vaccine")

## Quick cross-tab check
table(df$site, df$arm)

## ----------------------------------------------------
## Task 5: Blinding
## ----------------------------------------------------

# The main issue with the current object is that rows are not shuffled.
# Currently there is a very predictable repeating pattern between placebo and
# vaccine. This could be fixed by randomly shuffling rows prior to assigning
# IDs.
