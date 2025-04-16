# Load packages ----------------------------------------------------------------
print("Load packages")

library(magrittr)
library(data.table)
library(stringr)
library(lubridate)
# Source functions -------------------------------------------------------------
print("Source functions")

lapply(
  list.files("analysis/model", full.names = TRUE, pattern = "fn-"),
  source
)

# Specify arguments ------------------------------------------------------------
print("Specify arguments")

args <- commandArgs(trailingOnly = TRUE)

if (length(args) == 0) {
  name <- "cohort_prevax-main_preex_TRUE-pf" #Testing main loop + pre-existing as true
  # name <- "cohort_unvax-sub_covidhospital_TRUE_preex_FALSE-asthma" # covidhospital TRUE test
  # name <- "cohort_unvax-sub_covidhospital_FALSE_preex_FALSE-asthma" # covidhospital FALSE test
  # name <- "cohort_vax-sub_covidhistory_preex_FALSE-pf" # covidhistory test
  # name <- "cohort_vax-sub_sex_female_preex_FALSE-asthma" # Testing sex group
  # name <- "cohort_vax-sub_age_40_59_preex_FALSE-pf" # Testing age group
  name <- "cohort_prevax-sub_ethnicity_asian_preex_FALSE-copd" # Check that the "asian" ethnicity is processing correctly
  name <- "cohort_prevax-sub_smoking_never_preex_FALSE-pneumonia" # Check that the smoking subgroup is processing correctly
} else {
  name <- args[[1]]
}

analysis <- gsub(
  "cohort_.*vax-",
  "",
  name
)

# Define make_model_input output folder ---------------------------------------
print("Creating output/table1 output folder")

# setting up the sub directory
model_dir <- "output/model/"

# check if sub directory exists, create if not
fs::dir_create(here::here(model_dir))

# Load and prepare data by selecting project-required columns and filtering the study population
print("Load and prepare data for analysis")

pmi <- prepare_model_input(name, analysis)
analysis <- pmi$analysis

## Perform subgroup-specific manipulation
print("Perform subgroup-specific manipulation")

print(paste0("Make model input: ", analysis))

check_for_subgroup <- (grepl("main", analysis)) # =1 if subgroup is main, =0 otherwise

# Make model input: main/sub_covidhistory ------------------------------------
if (grepl("sub_covidhistory", analysis)) {
  check_for_subgroup <- 1
  df <- pmi$input[pmi$input$sub_bin_covidhistory == TRUE, ] # Only selecting for this subgroup
} else {
  df <- pmi$input[pmi$input$sub_bin_covidhistory == FALSE, ] # all other subgroups (inc. Main)
}

# Make model input: sub_covidhospital ----------------------------------------
if (grepl("sub_covidhospital", analysis)) {
  check_for_subgroup <- 1
  covidhosp <- as.logical(gsub(
    ".*sub_covidhospital_",
    "",
    analysis
  ))
  str_covidhosp_cens <- ifelse(covidhosp, "non_hospitalised", "hospitalised")
  df <- df %>%
    dplyr::mutate(
      end_date_outcome = replace(
        end_date_outcome,
        which(sub_cat_covidhospital == str_covidhosp_cens),
        exp_date - 1
      ),
      exp_date = replace(
        exp_date,
        which(sub_cat_covidhospital == str_covidhosp_cens),
        NA
      ),
      out_date = replace(
        out_date,
        which(out_date > end_date_outcome),
        NA
      )
    )
  df <- df[df$end_date_outcome >= df$index_date, ]
}

# Make model input: sub_sex_* ------------------------------------------------
if (grepl("sub_sex_", analysis)) {
  check_for_subgroup <- 1
  sex <- str_to_title(gsub(
    ".*sub_sex_",
    "",
    analysis
  ))
  df <- df[df$cov_cat_sex == sex, ]
}

# Make model input: sub_age_* ------------------------------------------------
if (grepl("sub_age_", analysis) == TRUE) {
  check_for_subgroup <- 1
  min_age <- as.numeric(strsplit(
    gsub(".*sub_age_", "", analysis),
    split = "_"
  )[[1]][1])
  max_age <- as.numeric(strsplit(
    gsub(".*sub_age_", "", analysis),
    split = "_"
  )[[1]][2])
  df <- df[
    df$cov_num_age >= min_age &
      df$cov_num_age < (max_age + 1),
  ]
}

# Make model input: sub_ethnicity_* ------------------------------------------
if (grepl("sub_ethnicity_", analysis) == TRUE) {
  check_for_subgroup <- 1
  ethnicity <- str_to_title(gsub(
    "_",
    " ",
    gsub(
      ".*sub_ethnicity_",
      "",
      analysis
    )
  ))
  df <- df[df$cov_cat_ethnicity == ethnicity, ]
}

# Make model input: sub_smoking_* ------------------------------------------
if (grepl("sub_smoking_", analysis)) {
  smoking_label <- gsub(".*sub_smoking_", "", analysis)

  # Create a mapping from label to code
  smoking_map <- c(
    "never" = "N",
    "ever" = "E",
    "current" = "S"
  )
  smoking_code <- smoking_map[[smoking_label]]

  if (is.null(smoking_code)) {
    stop(paste0("Unrecognized smoking subgroup: ", smoking_label))
  }

  df <- df[df$cov_cat_smoking == smoking_code, ]
}

# Stop code if no subgroup/main analysis was correctly selected
if (!check_for_subgroup) {
  stop(paste0("Input: ", name, " did not undergo any subgroup filtering!"))
}

# Save model output
df <- df %>%
  dplyr::select(tidyselect::all_of(pmi$keep))

check_vitals(df)
readr::write_rds(
  df,
  file.path(
    model_dir,
    paste0("model_input-", name, ".rds")
  ),
  compress = "gz"
)
print(paste0(
  "Saved: ",
  model_dir,
  "model_input-",
  name,
  ".rds"
))
rm(df)
