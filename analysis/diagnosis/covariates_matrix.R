library(dplyr)
library(tidyr)
library(readr)
library(survival)
library(plyr)

# Define diagnosis output folder ------------------------------------------
print("Creating diagnosis output folder")

diag_dir <- "output/diagnosis/"
fs::dir_create(here::here(diag_dir))

# Load data and interval generator
source("analysis/diagnosis/fn-generate_time_interval_indicators.R")
df <- readRDS(
  "output/model/model_input-cohort_vax-main_preex_FALSE-pneumonia.rds"
)
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

# Function to compute co-occurrence matrix for a given time interval
compute_co_matrix <- function(interval_name) {
  df_subset <- df_cov %>%
    filter(.data[[interval_name]] == 1) %>%
    select(patient_id, all_of(covariate_vars))

  # Drop factor/ordered classes for all covariates
  for (v in names(df_subset)[names(df_subset) != "patient_id"]) {
    df_subset[[v]] <- as.character(df_subset[[v]])
  }

  df_long <- df_subset %>%
    pivot_longer(-patient_id, names_to = "variable", values_to = "value") %>%
    mutate(level = paste0(variable, " = ", value)) %>%
    select(patient_id, level)

  df_wide <- df_long %>%
    mutate(present = 1L) %>%
    pivot_wider(names_from = level, values_from = present, values_fill = 0)

  mat <- as.matrix(df_wide[, -1])
  co_occurrence <- t(mat) %*% mat

  return(co_occurrence)
}

# Generate and save co-occurrence matrix for each interval
for (interval in time_intervals) {
  co_matrix <- compute_co_matrix(interval)

  # Optional: sort variable levels
  ordered_names <- colnames(co_matrix) %>%
    as.data.frame() %>%
    setNames("full_name") %>%
    mutate(
      variable = sub(" = .*", "", full_name),
      level = sub(".* = ", "", full_name)
    ) %>%
    arrange(variable, level) %>%
    pull(full_name)

  co_matrix_ordered <- co_matrix[ordered_names, ordered_names]

  write.csv(
    as.data.frame(co_matrix_ordered),
    paste0(diag_dir, "variable_level_cross_counts_", interval, ".csv"),
    row.names = TRUE
  )
}
