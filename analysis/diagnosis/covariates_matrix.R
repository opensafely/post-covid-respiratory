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

  # Correlation matrix (if >1 variable)
  corr_matrix <- if (ncol(mat) > 1) cor(mat) else NA

  return(list(co = co_occurrence, corr = corr_matrix))
}

# Loop over time intervals
for (interval in time_intervals) {
  message("Processing: ", interval)
  mats <- compute_matrices(interval)

  # Sort row/column order
  ordered_names <- colnames(mats$co) %>%
    as.data.frame() %>%
    setNames("full_name") %>%
    mutate(
      variable = sub(" = .*", "", full_name),
      level = sub(".* = ", "", full_name)
    ) %>%
    arrange(variable, level) %>%
    pull(full_name)

  co_matrix_ordered <- mats$co[ordered_names, ordered_names]
  write.csv(
    as.data.frame(co_matrix_ordered),
    paste0(diag_dir, "cov_crosstab-", name, "-", interval, ".csv"),
    row.names = TRUE
  )

  if (!is.na(mats$corr)[1]) {
    corr_matrix_ordered <- mats$corr[ordered_names, ordered_names]
    write.csv(
      as.data.frame(corr_matrix_ordered),
      paste0(diag_dir, "cov_correlation-", name, "-", interval, ".csv"),
      row.names = TRUE
    )
  }
}
