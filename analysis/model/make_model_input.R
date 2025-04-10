# Load packages ----------------------------------------------------------------
print("Load packages")

library(magrittr)
library(data.table)
library(stringr)

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
  # name <- "cohort_prevax-main_preex_TRUE-pf" #Testing pre-existing as true
  name <- "cohort_unvax-sub_covidhospital_TRUE_preex_FALSE-asthma" # covidhospital test
  name <- "cohort_unvax-sub_covidhospital_FALSE_preex_FALSE-asthma" # covidhospital test
  # name <- "cohort_vax-sub_covidhistory_preex_FALSE-pf" # covidhistory test
  # name <- "cohort_vax-sub_sex_female_preex_FALSE-asthma" # Testing sex group
  # name <- "cohort_vax-sub_age_40_59_preex_FALSE-pf" # Testing age group
  # name <- "cohort_prevax-sub_ethnicity_south_asian_preex_FALSE-copd" # Check that the "south_asian" ethnicity is processing correctly
} else {
  name <- args[[1]]
}

# Define make_model_input output folder ---------------------------------------
print("Creating output/table1 output folder")

# setting up the sub directory
model_dir <- "output/model/"

# check if sub directory exists, create if not
fs::dir_create(here::here(model_dir))

# Load active analyses ---------------------------------------------------------
print("Load active analyses")

active_analyses <- readr::read_rds("lib/active_analyses.rds")


# Filter active_analyses to model inputs to be prepared ------------------------
print("Filter active_analyses to model inputs to be prepared")

active_analyses <- active_analyses[active_analyses$name == name, ]

if (nrow(active_analyses) == 0) {
  stop(paste0("Input: ", name, " does not match any analyses"))
}

# Load and prepare data for analysis
print("Load and prepare data for analysis")

df_model <- prepare_model(active_analyses)
input <- df_model$input
active_analyses <- df_model$active_analyses

## Perform subgroup-specific manipulation
print("Perform subgroup-specific manipulation")

# Make model input: main/sub_covidhistory ------------------------------------

if (grepl("sub_covidhistory", active_analyses$analysis[i])) {
  df <- input[input$sub_bin_covidhistory == TRUE, ] # Only selecting for this subgroup
} else {
  df <- input[input$sub_bin_covidhistory == FALSE, ] # all other subgroups (inc. Main)
}

# Make model input: sub_covidhospital ----------------------------------------
if (grepl("sub_covidhospital", active_analyses$analysis[i])) {
  covidhosp <- as.logical(gsub(
    ".*sub_covidhospital_",
    "",
    active_analyses$analysis[i]
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
if (grepl("sub_sex_", active_analyses$analysis[i])) {
  sex <- str_to_title(gsub(
    ".*sub_sex_",
    "",
    active_analyses$analysis[i]
  ))
  df <- df[df$cov_cat_sex == sex, ]
}

# Make model input: sub_age_* ------------------------------------------------
if (grepl("sub_age_", active_analyses$analysis[i]) == TRUE) {
  min_age <- as.numeric(strsplit(
    gsub(".*sub_age_", "", active_analyses$analysis[i]),
    split = "_"
  )[[1]][1])
  max_age <- as.numeric(strsplit(
    gsub(".*sub_age_", "", active_analyses$analysis[i]),
    split = "_"
  )[[1]][2])
  df <- df[
    df$cov_num_age >= min_age &
      df$cov_num_age < (max_age + 1),
  ]
}

# Make model input: sub_ethnicity_* ------------------------------------------
if (grepl("sub_ethnicity_", active_analyses$analysis[i]) == TRUE) {
  ethnicity <- str_to_title(gsub(
    "_",
    " ",
    gsub(
      ".*sub_ethnicity_",
      "",
      active_analyses$analysis[i]
    )
  ))
  df <- df[df$cov_cat_ethnicity == ethnicity, ]
}

# Save model output
df <- df %>%
  dplyr::select(tidyselect::all_of(keep))

check_vitals(df)
readr::write_rds(
  df,
  file.path(
    model_dir,
    paste0("model_input-", active_analyses$name[i], ".rds")
  ),
  compress = "gz"
)
print(paste0(
  "Saved: ",
  model_dir,
  "model_input-",
  active_analyses$name[i],
  ".rds"
))
# rm(df)
