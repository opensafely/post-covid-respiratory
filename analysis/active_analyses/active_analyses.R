library(jsonlite)
library(dplyr)

# Create output directory ----
fs::dir_create(here::here("lib"))

# Source common functions ----
lapply(
  list.files("analysis/", full.names = TRUE, pattern = "fn-"),
  source
)

# Define cohorts ----
cohorts <- c("vax", "unvax", "prevax")

# Define outcomes ----
outcomes_preex <- c("out_date_copd", "out_date_asthma")
outcomes_all <- c(outcomes_preex, "out_date_pneumonia", "out_date_pf")

# Define subgroups ----
subgroups <- c(
  "sub_covidhospital_TRUE",
  "sub_covidhospital_FALSE",
  "sub_covidhistory",
  "sub_sex_female",
  "sub_sex_male",
  "sub_age_18_39",
  "sub_age_40_59",
  "sub_age_60_79",
  "sub_age_80_110",
  "sub_ethnicity_white",
  "sub_ethnicity_black",
  "sub_ethnicity_mixed",
  "sub_ethnicity_asian",
  "sub_ethnicity_other",
  "sub_smoking_never",
  "sub_smoking_ever",
  "sub_smoking_current"
)

# Define covariates ----
core_covars <- c(
  "cov_cat_ethnicity",
  "cov_cat_imd",
  "cov_num_consrate2019",
  "cov_bin_hcworker",
  "cov_cat_smoking",
  "cov_bin_carehome",
  "cov_bin_obesity",
  "cov_bin_ami",
  "cov_bin_dementia",
  "cov_bin_liver_disease",
  "cov_bin_ckd",
  "cov_bin_cancer",
  "cov_bin_hypertension",
  "cov_bin_diabetes",
  "cov_bin_depression",
  "cov_bin_copd",
  "cov_bin_stroke_isch"
)
project_covars <- c("cov_bin_pneumonia", "cov_bin_asthma", "cov_bin_pf")
all_covars <- c(core_covars, project_covars)

# Create empty data frame ----
df <- data.frame(
  cohort = character(),
  exposure = character(),
  outcome = character(),
  ipw = logical(),
  strata = character(),
  covariate_sex = character(),
  covariate_age = character(),
  covariate_other = character(),
  cox_start = character(),
  cox_stop = character(),
  study_start = character(),
  study_stop = character(),
  cut_points = character(),
  controls_per_case = numeric(),
  total_event_threshold = numeric(),
  episode_event_threshold = numeric(),
  covariate_threshold = numeric(),
  age_spline = logical(),
  analysis = character(),
  stringsAsFactors = FALSE
)

# Generate analyses ----
for (c in cohorts) {
  for (i in outcomes_all) {
    preex_groups <- if (i %in% outcomes_preex) "preex_FALSE" else
      c("preex_FALSE", "preex_TRUE")

    for (p in preex_groups) {
      covariate_other <- ifelse(
        p == "preex_FALSE",
        paste0(
          setdiff(all_covars, c("cov_bin_asthma", "cov_bin_copd")),
          collapse = ";"
        ),
        paste0(all_covars, collapse = ";")
      )

      # Add main analysis ----
      df[nrow(df) + 1, ] <- add_analysis(
        cohort = c,
        outcome = i,
        preex = p,
        analysis_name = "main",
        covariate_other = covariate_other,
        age_spline = TRUE
      )

      # Add subgroup analyses ----
      for (sub in subgroups) {
        # Skip sub_covidhistory if cohort is "prevax"
        if (sub == "sub_covidhistory" && c == "prevax") {
          next
        }

        # Adjust covariate_other for ethnicity and smoking subgroups
        adjusted_covariate_other <- covariate_other
        if (grepl("sub_ethnicity", sub)) {
          adjusted_covariate_other <- paste0(
            setdiff(strsplit(covariate_other, ";")[[1]], "cov_cat_ethnicity"),
            collapse = ";"
          )
        } else if (grepl("sub_smoking", sub)) {
          adjusted_covariate_other <- paste0(
            setdiff(strsplit(covariate_other, ";")[[1]], "cov_cat_smoking"),
            collapse = ";"
          )
        }

        # Add analysis for the subgroup
        df[nrow(df) + 1, ] <- add_analysis(
          cohort = c,
          outcome = i,
          preex = p,
          analysis_name = sub,
          covariate_other = adjusted_covariate_other,
          age_spline = ifelse(grepl("sub_age", sub), FALSE, TRUE)
        )
      }
    }
  }
}

# Add name for each analysis ----
df$name <- paste0(
  "cohort_",
  df$cohort,
  "-",
  df$analysis,
  "-",
  gsub("out_date_", "", df$outcome)
)

# Check names are unique and save active analyses list ----
if (length(unique(df$name)) == nrow(df)) {
  saveRDS(df, file = "lib/active_analyses.rds", compress = "gzip")
} else {
  stop("ERROR: names must be unique in active analyses table")
}
