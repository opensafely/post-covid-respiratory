# Load packages ----------------------------------------------------------------
print('Load packages')

library(magrittr)

# Source common functions ------------------------------------------------------
print('Source common functions')

source("analysis/utility.R")

# Specify arguments ------------------------------------------------------------
print("Specify arguments")

args <- commandArgs(trailingOnly = TRUE)

if (length(args) == 0) {
  subgroup <- "main"
} else {
  subgroup <- args[[1]]
}

# Define model output folder ---------------------------------------
print("Creating output/model output folder")

# setting up the sub directory
makeout_dir <- "output/make_output/"
model_dir <- "output/model/"

# check if sub directory exists, create if not
fs::dir_create(here::here(makeout_dir))

# Load active analyses ---------------------------------------------------------
print('Load active analyses')

active_analyses <- readr::read_rds("lib/active_analyses.rds")

# List available R model outputs -----------------------------------------------
print('List available model outputs')

files_R <- list.files(model_dir, pattern = "model_output-")

# Combine R model output -------------------------------------------------------
print('Combine R model output')

df_R <- NULL

for (i in files_R) {
  ## Load model output
  tmp <- readr::read_csv(paste0(model_dir, i))

  ## Handle errors
  if (colnames(tmp)[1] == "error") {
    dummy <- data.frame(
      model = "",
      exposure = "",
      outcome = gsub(".*-", "", gsub(".csv", "", i)),
      term = "",
      lnhr = NA,
      se_lnhr = NA,
      hr = NA,
      conf_low = NA,
      conf_high = NA,
      N_total = NA,
      N_exposed = NA,
      N_events = NA,
      person_time_total = NA,
      outcome_time_median = NA,
      strata_warning = "",
      surv_formula = "",
      input = "",
      error = tmp$error
    )
    tmp <- dummy
  } else {
    tmp$error <- ""
  }

  ## Add source file name
  tmp$name <- gsub("model_output-", "", gsub(".csv", "", i))

  ## Append to master dataframe
  df_R <- plyr::rbind.fill(df_R, tmp)
}

# Add details from active analyses ---------------------------------------------
print('Add details from active analyses')

df_R[, c("exposure", "outcome")] <- NULL

df_R <- merge(
  df_R,
  active_analyses[, c("name", "cohort", "outcome", "analysis")],
  by = "name",
  all.x = TRUE
)

df_R$outcome <- gsub("out_date_", "", df_R$outcome)

df_R$source <- "R"

# Save model output ------------------------------------------------------------
print('Save model output')

df_R <- df_R[, c(
  "name",
  "cohort",
  "outcome",
  "analysis",
  "error",
  "model",
  "term",
  "lnhr",
  "se_lnhr",
  "hr",
  "conf_low",
  "conf_high",
  "N_total",
  "N_exposed",
  "N_events",
  "person_time_total",
  "outcome_time_median",
  "strata_warning",
  "surv_formula",
  "source"
)]

# List available Stata model outputs -------------------------------------------
print('List Stata available model outputs')

files_S <- list.files(model_dir, pattern = "stata_model_output-")

# Combine Stata model output ---------------------------------------------------
print('Combine Stata model output')

df_S <- NULL

for (j in files_S) {
  print(paste0("File: ", j))

  # Load data ------------------------------------------------------------------
  print('Load data')

  stata_model_output <- readr::read_tsv(
    file = paste0(model_dir, j),
    skip = 2,
    col_names = c(
      "term",
      "b_min",
      "se_min",
      "t_min",
      "lci_min",
      "uci_min",
      "p_min",
      "b_max",
      "se_max",
      "t_max",
      "lci_max",
      "uci_max",
      "p_max"
    )
  )

  # Make variables numeric -----------------------------------------------------
  print('Make variables numeric')

  stata_model_output <- stata_model_output %>%
    dplyr::mutate_at(setdiff(colnames(stata_model_output), "term"), as.numeric)

  # Extract other details ------------------------------------------------------
  print('Extract other details')

  tmp <- stata_model_output[
    stata_model_output$term %in% c("N_sub", "risk"),
    c("term", "b_min", "b_max")
  ]

  # Transform data -------------------------------------------------------------
  print('Transform data')

  stata_model_output <- stata_model_output[
    (grepl("cov", stata_model_output$term) |
      grepl("age", stata_model_output$term) |
      grepl("days", stata_model_output$term)),
  ]

  stata_model_output <- tidyr::pivot_longer(
    stata_model_output,
    cols = setdiff(colnames(stata_model_output), "term")
  )

  stata_model_output <- tidyr::separate(
    stata_model_output,
    col = "name",
    into = c("stat", "model")
  )

  stata_model_output <- tidyr::pivot_wider(
    stata_model_output,
    id_cols = c("term", "model"),
    names_from = "stat"
  )

  stata_model_output <- stata_model_output[!is.na(stata_model_output$b), ]

  # Add name -------------------------------------------------------------------
  print('Add name')

  stata_model_output$name <- gsub(
    ".txt",
    "",
    gsub("stata_model_output-", "", j)
  )

  # Add median fup -------------------------------------------------------------
  print('Add median fup')

  median_fup <- readr::read_csv(
    file = paste0(
      model_dir,
      gsub(".txt", ".csv", gsub("stata_model_output-", "stata_fup-", j))
    )
  )

  median_fup <- dplyr::rename(
    median_fup,
    "outcome_time_median" = "median_tte",
    "N_events" = "events"
  )

  stata_model_output <- merge(
    stata_model_output,
    median_fup,
    by = "term",
    all.x = TRUE
  )

  # Rename columns -------------------------------------------------------------
  print('Rename columns')

  stata_model_output <- dplyr::rename(
    stata_model_output,
    "lnhr" = "b",
    "se_lnhr" = "se"
  )

  stata_model_output$model <- ifelse(
    stata_model_output$model == "max",
    "mdl_max_adj",
    stata_model_output$model
  )
  stata_model_output$model <- ifelse(
    stata_model_output$model == "min",
    "mdl_age_sex",
    stata_model_output$model
  )

  # Add other details ----------------------------------------------------------
  print('Add other details')

  tmp <- tidyr::pivot_longer(
    tmp,
    cols = setdiff(colnames(tmp), "term"),
    names_to = "model"
  )

  tmp$model <- ifelse(tmp$model == "b_max", "mdl_max_adj", tmp$model)
  tmp$model <- ifelse(tmp$model == "b_min", "mdl_age_sex", tmp$model)

  tmp$term <- ifelse(tmp$term == "N_sub", "N_total", tmp$term)
  tmp$term <- ifelse(tmp$term == "risk", "person_time_total", tmp$term)

  tmp <- tidyr::pivot_wider(tmp, names_from = "term")
  stata_model_output <- merge(
    stata_model_output,
    tmp,
    by = "model",
    all.x = TRUE
  )

  # Merge to master data frame -------------------------------------------------
  print('Merge to master data frame')

  df_S <- rbind(df_S, stata_model_output)
}

# Add missing columns --------------------------------------------------------
print('Add missing columns')

df_S <- tidyr::separate(
  df_S,
  col = "name",
  into = c("cohort", "analysis", "outcome"),
  sep = "-",
  remove = FALSE
)

df_S$cohort <- gsub("cohort_", "", df_S$cohort)
df_S$hr <- exp(df_S$lnhr)
df_S$conf_low <- exp(df_S$lci)
df_S$conf_high <- exp(df_S$uci)
df_S$N_exposed <- NA
df_S$error <- ""
df_S$strata_warning <- ""
df_S$surv_formula <- ""
df_S$source <- "Stata"

df_S <- df_S[, c(
  "name",
  "cohort",
  "outcome",
  "analysis",
  "error",
  "model",
  "term",
  "lnhr",
  "se_lnhr",
  "hr",
  "conf_low",
  "conf_high",
  "N_total",
  "N_exposed",
  "N_events",
  "person_time_total",
  "outcome_time_median",
  "strata_warning",
  "surv_formula",
  "source"
)]

# Combine R and Stata model output ---------------------------------------------
print('Combine R and Stata model output')

df <- rbind(df_R, df_S)

# Save model output ------------------------------------------------------------
print('Save model output')

df <- df[, c(
  "name",
  "cohort",
  "outcome",
  "analysis",
  "error",
  "model",
  "term",
  "lnhr",
  "se_lnhr",
  "hr",
  "conf_low",
  "conf_high",
  "N_total",
  "N_exposed",
  "N_events",
  "person_time_total",
  "outcome_time_median",
  "strata_warning",
  "surv_formula",
  "source"
)]

readr::write_csv(df, paste0(makeout_dir, "model_output-", subgroup, ".csv"))

# Perform redaction ------------------------------------------------------------
print('Perform redaction')

df$N_total_midpoint6 <- roundmid_any(df$N_total)
df$N_exposed_midpoint6 <- roundmid_any(df$N_exposed)
df$N_events_midpoint6 <- roundmid_any(df$N_events)
df[, c("N_total", "N_exposed", "N_events")] <- NULL

# Save model output ------------------------------------------------------------
print('Save model output')

readr::write_csv(
  df,
  paste0(makeout_dir, "model_output-", subgroup, "-midpoint6.csv")
)
