## 2.B.2 – Cleaning a Matrix of Allele Frequencies — Solutions
## ------------------------------------------------------------

## remotes::install_github("mrc-ide/RCodingSupport@v1.0.0")
library(RCodingSupport)

## ------------------------------------------------------------
## Task 1: Explore the dataset
## ------------------------------------------------------------
# Basic structure
dim(allele_freq_matrix)         # 300 x 200 expected
rownames(allele_freq_matrix)[1:6]
colnames(allele_freq_matrix)[1:6]

# Peek at values
head(allele_freq_matrix[, 1:6]) # first few loci across first 6 samples
summary(as.vector(allele_freq_matrix))  # overall summary (flattens the matrix)

# Heatmap-style quick look
image(allele_freq_matrix, xlab = "Column (samples)", ylab = "Row (loci)",
      main = "Allele frequency matrix (raw)")
# Flip axes if preferred
image(t(allele_freq_matrix), xlab = "Samples (columns)", ylab = "Loci (rows)",
      main = "Allele frequency matrix (transposed)")

## ------------------------------------------------------------
## Task 2: Identify artefacts (< 0) and replace with NA
## ------------------------------------------------------------
# Find positions with invalid (negative) values
neg_idx <- which(allele_freq_matrix < 0, arr.ind = TRUE)
nrow(neg_idx)  # how many artefacts

# Replace negatives with NA
allele_freq_clean <- allele_freq_matrix
allele_freq_clean[neg_idx] <- NA_real_

# Verify replacement happened and non-artefacts unchanged at the sampled spots
allele_freq_matrix[33:35, 1:3]
allele_freq_clean[33:35, 1:3]

## ------------------------------------------------------------
## Task 3: Summarise missingness
## ------------------------------------------------------------
# Counts of missing values per locus (row) and per sample (column)
row_na_count <- rowSums(is.na(allele_freq_clean))
col_na_count <- colSums(is.na(allele_freq_clean))

# Optional: proportions (often easier to reason about)
row_na_prop <- row_na_count / ncol(allele_freq_clean)
col_na_prop <- col_na_count / nrow(allele_freq_clean)

# Histograms to visualise missingness
hist(row_na_prop, breaks = 20, main = "Row-wise NA proportion", xlab = "Proportion NA (per locus)")
hist(col_na_prop, breaks = 20, main = "Column-wise NA proportion", xlab = "Proportion NA (per sample)")

## Choose thresholds (illustrative; students may justify different values)
row_thresh <- 0.30  # e.g., drop loci with >30% missing
col_thresh <- 0.30  # e.g., drop samples with >30% missing

## ------------------------------------------------------------
## Task 4: Apply filtering (rows then columns)
## ------------------------------------------------------------
rows_to_keep <- which(row_na_prop <= row_thresh)
allele_freq_filter1 <- allele_freq_clean[rows_to_keep, ]

cols_to_keep <- which(col_na_prop <= col_thresh)
allele_freq_filter2 <- allele_freq_filter1[, cols_to_keep]

# Compare dimensions before vs after
dim(allele_freq_matrix)
dim(allele_freq_filter2)

## ------------------------------------------------------------
## Task 5: Analysis of cleaned data
## ------------------------------------------------------------
# Row means with NA removal
row_means <- rowMeans(allele_freq_clean, na.rm = TRUE)

# Quick plot
plot(row_means, ylim = c(0, 1))
abline(h = 0.5, lty = 2)

# Count how many loci have mean allele frequency > 0.5
sum(row_means > 0.5)
