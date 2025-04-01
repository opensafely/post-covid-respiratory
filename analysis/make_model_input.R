# Load packages ----------------------------------------------------------------
print("Load packages")

library(magrittr)
library(data.table)

# Source functions -------------------------------------------------------------
print("Source functions")

source("analysis/fn-check_vitals.R") # check if needed

# Specify arguments ------------------------------------------------------------
print("Specify arguments")

args <- commandArgs(trailingOnly = TRUE)

if (length(args) == 0) {
  name <- "vax-sub_sex_female-copd" # prepare datasets for all active analyses # cohort-covariate-outcome
  # name <- "cohort_vax-sub_age_65_84-migraine" # prepare datasets for all active analyses # cohort-covariate-outcome
  name <- "cohort_prevax-main_preex_TRUE-pf;cohort_prevax-main_preex_FALSE-pf" #Testing pre-existing as true and false at once (and main test)
  name <- "cohort_unvax-sub_covidhospital_TRUE_preex_FALSE-asthma;cohort_unvax-sub_covidhospital_FALSE_preex_FALSE-asthma" # covidhospital test
  # name <- "cohort_vax-sub_sex_female_preex_FALSE-asthma;cohort_vax-sub_sex_male_preex_FALSE-asthma" # Testing sex groups
  # name <- "cohort_vax-subhistory_migraine; cohort_vax-subhistory_depression" # This one should fail (it's a neuro group)
  # name <- "cohort_vax-sub_age_18_39_preex_FALSE-pf;cohort_vax-sub_age_40_59_preex_FALSE-pf;cohort_vax-sub_age_60_79_preex_FALSE-pf;cohort_vax-sub_age_80_110_preex_FALSE-pf" # This is a test for 4 at once
  # name <- "cohort_prevax-sub_ethnicity_white_preex_FALSE-copd;cohort_prevax-sub_ethnicity_black_preex_FALSE-copd;cohort_prevax-sub_ethnicity_mixed_preex_FALSE-copd;cohort_prevax-sub_ethnicity_asian_preex_FALSE-copd;cohort_prevax-sub_ethnicity_other_preex_FALSE-copd"
  # name <- "cohort_vax-sub_covidhistory_preex_FALSE-pf"
} else {
  name <- args[[1]]
}

# Define make_model_input output folder ---------------------------------------
print("Creating output/table1 output folder")

# setting up the sub directory
mmi_dir <- "output/make_model_input/"

# check if sub directory exists, create if not
fs::dir_create(here::here(mmi_dir))

# Load active analyses ---------------------------------------------------------
print("Load active analyses")

active_analyses <- readr::read_rds("lib/active_analyses.rds")

# Identify model inputs to be prepared -----------------------------------------
print("Identify model inputs to be prepared")

if (name == "all") {
  prepare <- active_analyses$name
} else if (grepl(";", name)) {
  prepare <- stringr::str_split(as.vector(name), ";")[[1]]
} else {
  prepare <- active_analyses[grepl(name, active_analyses$name), ]$name
}

# Filter active_analyses to model inputs to be prepared ------------------------
print("Filter active_analyses to model inputs to be prepared")

active_analyses <- active_analyses[active_analyses$name %in% prepare, ]

if (nrow(active_analyses) == 0) {
  stop(paste0("Input: ", name, " does not match any analyses"))
}

for (i in 1:nrow(active_analyses)) {
  # Load data ------------------------------------------------------------------
  print(paste0("Load data for ", active_analyses$name[i]))

  input <- dplyr::as_tibble(readr::read_rds(paste0(
    "output/dataset_clean/input_",
    active_analyses$cohort[i],
    "_clean.rds"
  )))

  # Restrict to required variables for dataset preparation ---------------------
  print("Restrict to required variables for dataset preparation")

  input <- input[, unique(c(
    "patient_id",
    "index_date",
    "end_date_exposure",
    "end_date_outcome",
    active_analyses$exposure[i],
    active_analyses$outcome[i],
    unlist(strsplit(active_analyses$strata[i], split = ";")),
    unlist(strsplit(active_analyses$covariate_other[i], split = ";")),
    "sub_cat_covidhospital", # variables needed for subgroup analysis
    "sub_bin_covidhistory", # variables needed for subgroup analysis
    "cov_cat_sex", # variables needed for subgroup analysis
    "cov_num_age", # variables needed for subgroup analysis
    "cov_cat_ethnicity" # variables needed for subgroup analysis
    #  colnames(input)[grepl("cov_cat_history_",colnames(input))] # Likely not needed (mental-health only)
  ))]

  # Identify final list of variables to keep -----------------------------------
  print("Identify final list of variables to keep")

  keep <- c(
    "patient_id",
    "index_date",
    "exp_date",
    "out_date",
    "end_date_exposure",
    "end_date_outcome"
  )
  varlists <- c("strata", "covariate_age", "covariate_sex", "covariate_other")
  for (j in varlists) {
    if (active_analyses[i, j] != "NULL") {
      keep <- c(
        keep,
        stringr::str_split(as.vector(active_analyses[i, j]), ";")[[1]]
      )
    }
  }

  # Remove outcomes outside of follow-up time ----------------------------------
  print("Remove outcomes outside of follow-up time")

  input <- dplyr::rename(
    input,
    "out_date" = active_analyses$outcome[i],
    "exp_date" = active_analyses$exposure[i]
  )

  input <- input %>%
    dplyr::mutate(
      out_date = replace(
        out_date,
        which(out_date > end_date_outcome | out_date < index_date),
        NA
      ),
      exp_date = replace(
        exp_date,
        which(exp_date > end_date_exposure | exp_date < index_date),
        NA
      ),
      sub_cat_covidhospital = replace(
        sub_cat_covidhospital,
        which(is.na(exp_date)),
        "no_infection"
      )
    )

  # Update end date to be outcome date where applicable ------------------------
  print("Update end date to be outcome date where applicable")

  input <- input %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      end_date_outcome = min(end_date_outcome, out_date, na.rm = TRUE)
    )

  print(paste0("Make model input: ", active_analyses$analysis[i]))

  # Covid history selection

  if (grepl("sub_covidhistory", active_analyses$analysis[i])) {
    df <- input[input$sub_bin_covidhistory == TRUE, ] # Only selecting for this subgroup
  } else {
    df <- input[input$sub_bin_covidhistory == FALSE, ] # all other subgroups (inc. Main)
  }

  # Creating a pre-existing condition variable where appropriate

  if (grepl("preex", active_analyses$analysis[i])) {
    # True false indicator of preex (not sure if needed)
    preex <- gsub(
      ".*preex_",
      "",
      active_analyses$analysis[i]
    )
    # Remove preex string from active analysis
    active_analyses$analysis[i] <- gsub(
      "_preex_.*",
      "",
      active_analyses$analysis[i]
    )
    # Preserve the string we removed from active_analysis$analysis
    preex_str <- paste0("_preex_", preex)
  }

  # Make model input: sub_covidhospital ----------------------------------------
  if (grepl("sub_covidhospital", active_analyses$analysis[i])) {
    covidhosp <- as.logical(gsub(
      ".*sub_covidhospital_",
      "",
      active_analyses$analysis[i]
    ))
    str_covidhosp <- ifelse(covidhosp, "non_hospitalised", "hospitalised")
    df <- df %>%
      dplyr::mutate(
        end_date_outcome = replace(
          end_date_outcome,
          which(sub_cat_covidhospital == str_covidhosp),
          exp_date - 1
        ),
        exp_date = replace(
          exp_date,
          which(sub_cat_covidhospital == str_covidhosp),
          NA
        ),
        out_date = replace(
          out_date,
          which(out_date > end_date_outcome),
          NA
        )
      )
    df <- df[df$end_date_outcome >= df$index_date, ]
    if (!covidhosp) {
      df$index_date <- as.Date(df$index_date)
    }
  }

  # Make model input: sub_sex_* ------------------------------------------------
  if (grepl("sub_sex_", active_analyses$analysis[i])) {
    sex <- tolower(gsub(
      ".*sub_sex_",
      "",
      active_analyses$analysis[i]
    ))
    df <- df[tolower(df$cov_cat_sex) == sex, ]
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
    ethnicity <- tolower(gsub(
      ".*sub_ethnicity_",
      "",
      active_analyses$analysis[i]
    ))
    if (ethnicity == "asian") {
      ethnicity <- "south asian"
    }
    df <- df[tolower(df$cov_cat_ethnicity) == ethnicity, ]
  }

  # Save model output
  df <- df %>%
    dplyr::select(tidyselect::all_of(keep))

  check_vitals(df)
  readr::write_rds(
    df,
    file.path(
      mmi_dir,
      paste0("model_input-", active_analyses$name[i], ".rds")
    ),
    compress = "gz"
  )
  print(paste0(
    "Saved: ",
    mmi_dir,
    "model_input-",
    active_analyses$name[i],
    ".rds"
  ))
  rm(df)
}
