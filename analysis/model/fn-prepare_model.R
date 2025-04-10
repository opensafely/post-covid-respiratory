prepare_model <- function(active_analyses) {
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
  return(list(active_analyses = active_analyses, input = input))
}
