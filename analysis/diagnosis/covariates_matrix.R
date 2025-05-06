library(plyr)
library(dplyr)
library(tidyr)
library(readr)
library(survival)
# Specify arguments ------------------------------------------------------------
print("Specify arguments")

args <- commandArgs(trailingOnly = TRUE)

if (length(args) == 0) {
  name <- "cohort_vax-main_preex_FALSE-pneumonia"
} else {
  name <- args[[1]]
}

# Define diagnosis output folder ------------------------------------------
print("Creating diagnosis output folder")

diag_dir <- "output/diagnosis/"
fs::dir_create(here::here(diag_dir))

# Load data and interval generator
source("analysis/diagnosis/fn-generate_time_interval_indicators.R")
df <- readRDS(paste0("output/model/model_input-", name, ".rds"))
df_surv <- generate_time_interval_indicators(df)

# Identify covariates and time intervals
covariate_vars <- grep("^cov_(bin|cat)_", names(df), value = TRUE)
time_intervals <- grep("^days", names(df_surv), value = TRUE)

# Join covariates to survival data
df_cov <- df_surv %>%
  select(patient_id, all_of(time_intervals)) %>%
  left_join(
    df[, c("patient_id", all_of(covariate_vars))],
    by = "patient_id"
  )

# Force all covariates to plain character
df_cov <- df_cov %>%
  mutate(across(all_of(covariate_vars), ~ as.character(as.vector(.))))

# Function to compute co-occurrence and correlation matrices
compute_matrices <- function(interval_name) {
  df_subset <- df_cov %>%
    filter(.data[[interval_name]] == 1) %>%
    select(patient_id, all_of(covariate_vars))

  # Convert to long then wide dummy matrix
  df_long <- df_subset %>%
    pivot_longer(-patient_id, names_to = "variable", values_to = "value") %>%
    mutate(level = paste0(variable, " = ", value)) %>%
    select(patient_id, level)

  df_wide <- df_long %>%
    mutate(present = 1L) %>%
    pivot_wider(names_from = level, values_from = present, values_fill = 0)

  mat <- as.matrix(df_wide[, -1]) # Drop patient_id

  # Co-occurrence matrix
  co_occurrence <- t(mat) %*% mat
  # Convert to long format, tidy, and drop within-variable pairs
  df_co <- as.data.frame(co_occurrence)
  df_co <- tibble::rownames_to_column(df_co, var = "V1")

  df_co_long <- tidyr::pivot_longer(
    df_co,
    cols = setdiff(colnames(df_co), "V1"),
    names_to = "V2"
  )
  
  df_co_long <- df_co_long %>%
    tidyr::separate(
      V1,
      into = c("V1_1", "V1_2"),
      sep = " = ",
      remove = TRUE
    ) %>%
    tidyr::separate(V2, into = c("V2_1", "V2_2"), sep = " = ", remove = TRUE)

  df_co_long <- df_co_long[df_co_long$V1_1 != df_co_long$V2_1, ]
  df_co_long <- df_co_long %>%
    dplyr::rename(
      cov_1 = V1_1,
      level1 = V1_2,
      cov_2 = V2_1,
      level2 = V2_2,
      count = value
    )
  # Correlation matrix (if >1 variable)
  corr_matrix <- if (ncol(mat) > 1) cor(mat) else NA

  return(list(co = df_co_long, corr = corr_matrix))
}

# Loop over time intervals
for (interval in time_intervals) {
  message("Processing: ", interval)
  mats <- compute_matrices(interval)

  # Write cleaned co-occurrence long table, sorted
  co_long_ordered <- mats$co %>%
    dplyr::arrange(cov_1, level1, cov_2, level2)

  write.csv(
    as.data.frame(co_long_ordered),
    paste0(diag_dir, "cov_crosstab-", name, "-", interval, ".csv"),
    row.names = TRUE
  )

  if (!is.na(mats$corr)[1]) {
    corr_matrix_ordered <- mats$corr[
      order(rownames(mats$corr)),
      order(colnames(mats$corr))
    ]
    write.csv(
      as.data.frame(corr_matrix_ordered),
      paste0(diag_dir, "cov_correlation-", name, "-", interval, ".csv"),
      row.names = TRUE
    )
  }
}
