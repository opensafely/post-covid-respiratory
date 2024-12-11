install.packages("jsonlite")
library(jsonlite)

# Create output directory ----

fs::dir_create(here::here("lib"))

# Create empty data frame ----

df <- data.frame(cohort = character(),
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
                 stringsAsFactors = FALSE)

# Set constant values ----

ipw <- TRUE
age_spline <- TRUE
exposure <- "exp_date_covid19_confirmed"
strata <- "cov_cat_region"
covariate_sex <- "cov_cat_sex"
covariate_age <- "cov_num_age"
cox_start <- "index_date"
cox_stop <- "end_date_outcome"
controls_per_case <- 20L
total_event_threshold <- 50L
episode_event_threshold <- 5L
covariate_threshold <- 5L

# Define dates ----
study_dates <- fromJSON("output/study_dates.json")
prevax_start <- study_dates$pandemic_start
vax_unvax_start <- study_dates$delta_date
study_stop <- study_dates$omicron_date

# Define cut points ----
prevax_cuts <- "1;28;197;365;714"
vax_unvax_cuts <- "1;28;197"

# Define covariates ----

## Core covariates (common across projects) ----
core_covars <- c(
  "cov_cat_ethnicity", "cov_cat_imd", "cov_num_consultation_rate", 
  "cov_bin_healthcare_worker", "cov_cat_smoking_status", "cov_bin_carehome_status", 
  "cov_bin_obesity", "cov_bin_ami", "cov_bin_dementia_combined", "cov_bin_liver_disease",
  "cov_bin_chronic_kidney_disease", "cov_bin_cancer", "cov_bin_hypertension", "cov_bin_diabetes", "cov_bin_depression", "cov_bin_history_copd"
)

## Define project-specific covariates (specific to respiratory project) ----
project_covars <- c(
  "cov_bin_history_pneumonia_snomed", "cov_bin_history_asthma_snomed", 
  "cov_bin_history_pulmonary_fibrosis_snomed", "cov_bin_all_stroke"
)
# Combine covariates into a single vector ----
all_covars <- c(core_covars, project_covars)

## Combine covariates into a single string for analysis ----
preex_FALSE_covars <- paste0(all_covars[!all_covars %in% c("cov_bin_history_asthma_snomed", "cov_bin_history_copd")], collapse = ";")
all_covars <- paste0(c(core_covars, project_covars), collapse = ";")

# Specify cohorts ----

cohorts <- c("vax","unvax","prevax")

# Specify outcomes ----

outcomes_preex <- c("out_date_copd",
                    "out_date_asthma")

outcomes_all <- c(outcomes_preex,
                  "out_date_pneumonia",
                  "out_date_pulmonary_fibrosis")

# For each cohort ----

for (c in cohorts) {
  
  # For each outcome ----
  
  for (i in outcomes_all) {
    
    # For each pre-existing subgroup -------------------------------------------
    
    preex <- if (i %in% outcomes_preex) "preex_FALSE" else c("preex_FALSE", "preex_TRUE")
    
    for (p in preex) {
      
      # Define analyses ----
      
      ## analysis: main ----
      df[nrow(df)+1,] <- c(cohort = c,
                           exposure = exposure, 
                           outcome = i,
                           ipw = ipw, 
                           strata = strata,
                           covariate_sex = covariate_sex,
                           covariate_age = covariate_age,
                           covariate_other = ifelse(p=="preex_FALSE", preex_FALSE_covars, all_covars),
                           cox_start = cox_start,
                           cox_stop = cox_stop,
                           study_start = ifelse(c=="prevax", prevax_start, vax_unvax_start),
                           study_stop = study_stop,
                           cut_points = ifelse(c=="prevax", prevax_cuts, vax_unvax_cuts),
                           controls_per_case = controls_per_case,
                           total_event_threshold = total_event_threshold,
                           episode_event_threshold = episode_event_threshold,
                           covariate_threshold = covariate_threshold,
                           age_spline = TRUE,
                           analysis = paste0("main","_",p))
      
      ## analysis: sub_covid_hospitalised ----
      df[nrow(df)+1,] <- c(cohort = c,
                           exposure = exposure, 
                           outcome = i,
                           ipw = ipw, 
                           strata = strata,
                           covariate_sex = covariate_sex,
                           covariate_age = covariate_age,
                           covariate_other = ifelse(p=="preex_FALSE", preex_FALSE_covars, all_covars),
                           cox_start = cox_start,
                           cox_stop = cox_stop,
                           study_start = ifelse(c=="prevax", prevax_start, vax_unvax_start),
                           study_stop = study_stop,
                           cut_points = ifelse(c=="prevax", prevax_cuts, vax_unvax_cuts),
                           controls_per_case = controls_per_case,
                           total_event_threshold = total_event_threshold,
                           episode_event_threshold = episode_event_threshold,
                           covariate_threshold = covariate_threshold,
                           age_spline = TRUE,
                           analysis = paste0("sub_covid_hospitalised","_",p))
      
      ## analysis: sub_covid_nonhospitalised ----
      df[nrow(df)+1,] <- c(cohort = c,
                           exposure = exposure, 
                           outcome = i,
                           ipw = ipw, 
                           strata = strata,
                           covariate_sex = covariate_sex,
                           covariate_age = covariate_age,
                           covariate_other = ifelse(p=="preex_FALSE", preex_FALSE_covars, all_covars),
                           cox_start = cox_start,
                           cox_stop = cox_stop,
                           study_start = ifelse(c=="prevax", prevax_start, vax_unvax_start),
                           study_stop = study_stop,
                           cut_points = ifelse(c=="prevax", prevax_cuts, vax_unvax_cuts),
                           controls_per_case = controls_per_case,
                           total_event_threshold = total_event_threshold,
                           episode_event_threshold = episode_event_threshold,
                           covariate_threshold = covariate_threshold,
                           age_spline = TRUE,
                           analysis = paste0("sub_covid_nonhospitalised","_",p))    
      
      ## analysis: sub_covid_history ----
      if (c!="prevax") {
        df[nrow(df)+1,] <- c(cohort = c,
                             exposure = exposure, 
                             outcome = i,
                             ipw = ipw, 
                             strata = strata,
                             covariate_sex = covariate_sex,
                             covariate_age = covariate_age,
                             covariate_other = ifelse(p=="preex_FALSE", preex_FALSE_covars, all_covars),
                             cox_start = cox_start,
                             cox_stop = cox_stop,
                             study_start = vax_unvax_start,
                             study_stop = study_stop,
                             cut_points = vax_unvax_cuts,
                             controls_per_case = controls_per_case,
                             total_event_threshold = total_event_threshold,
                             episode_event_threshold = episode_event_threshold,
                             covariate_threshold = covariate_threshold,
                             age_spline = TRUE,
                             analysis = paste0("sub_covid_history","_",p))
      }
      
      ## analysis: sub_sex_female ----
      df[nrow(df)+1,] <- c(cohort = c,
                           exposure = exposure, 
                           outcome = i,
                           ipw = ipw, 
                           strata = strata,
                           covariate_sex = "NULL",
                           covariate_age = covariate_age,
                           covariate_other = ifelse(p=="preex_FALSE", preex_FALSE_covars, all_covars),
                           cox_start = cox_start,
                           cox_stop = cox_stop,
                           study_start = ifelse(c=="prevax", prevax_start, vax_unvax_start),
                           study_stop = study_stop,
                           cut_points = ifelse(c=="prevax", prevax_cuts, vax_unvax_cuts),
                           controls_per_case = controls_per_case,
                           total_event_threshold = total_event_threshold,
                           episode_event_threshold = episode_event_threshold,
                           covariate_threshold = covariate_threshold,
                           age_spline = TRUE,
                           analysis = paste0("sub_sex_female","_",p))
      
      ## analysis: sub_sex_male ----
      df[nrow(df)+1,] <- c(cohort = c,
                           exposure = exposure, 
                           outcome = i,
                           ipw = ipw, 
                           strata = strata,
                           covariate_sex = "NULL",
                           covariate_age = covariate_age,
                           covariate_other = ifelse(p=="preex_FALSE", preex_FALSE_covars, all_covars),
                           cox_start = cox_start,
                           cox_stop = cox_stop,
                           study_start = ifelse(c=="prevax", prevax_start, vax_unvax_start),
                           study_stop = study_stop,
                           cut_points = ifelse(c=="prevax", prevax_cuts, vax_unvax_cuts),
                           controls_per_case = controls_per_case,
                           total_event_threshold = total_event_threshold,
                           episode_event_threshold = episode_event_threshold,
                           covariate_threshold = covariate_threshold,
                           age_spline = TRUE,
                           analysis = paste0("sub_sex_male","_",p))
      
      ## analysis: sub_age_18_39 ----
      df[nrow(df)+1,] <- c(cohort = c,
                           exposure = exposure, 
                           outcome = i,
                           ipw = ipw, 
                           strata = strata,
                           covariate_sex = covariate_sex,
                           covariate_age = covariate_age,
                           covariate_other = ifelse(p=="preex_FALSE", preex_FALSE_covars, all_covars),
                           cox_start = cox_start,
                           cox_stop = cox_stop,
                           study_start = ifelse(c=="prevax", prevax_start, vax_unvax_start),
                           study_stop = study_stop,
                           cut_points = ifelse(c=="prevax", prevax_cuts, vax_unvax_cuts),
                           controls_per_case = controls_per_case,
                           total_event_threshold = total_event_threshold,
                           episode_event_threshold = episode_event_threshold,
                           covariate_threshold = covariate_threshold,
                           age_spline = FALSE,
                           analysis = paste0("sub_age_18_39","_",p))
      
      ## analysis: sub_age_40_59 ----
      df[nrow(df)+1,] <- c(cohort = c,
                           exposure = exposure, 
                           outcome = i,
                           ipw = ipw, 
                           strata = strata,
                           covariate_sex = covariate_sex,
                           covariate_age = covariate_age,
                           covariate_other = ifelse(p=="preex_FALSE", preex_FALSE_covars, all_covars),
                           cox_start = cox_start,
                           cox_stop = cox_stop,
                           study_start = ifelse(c=="prevax", prevax_start, vax_unvax_start),
                           study_stop = study_stop,
                           cut_points = ifelse(c=="prevax", prevax_cuts, vax_unvax_cuts),
                           controls_per_case = controls_per_case,
                           total_event_threshold = total_event_threshold,
                           episode_event_threshold = episode_event_threshold,
                           covariate_threshold = covariate_threshold,
                           age_spline = FALSE,
                           analysis = paste0("sub_age_40_59","_",p))
      
      ## analysis: sub_age_60_79 ----
      df[nrow(df)+1,] <- c(cohort = c,
                           exposure = exposure, 
                           outcome = i,
                           ipw = ipw, 
                           strata = strata,
                           covariate_sex = covariate_sex,
                           covariate_age = covariate_age,
                           covariate_other = ifelse(p=="preex_FALSE", preex_FALSE_covars, all_covars),
                           cox_start = cox_start,
                           cox_stop = cox_stop,
                           study_start = ifelse(c=="prevax", prevax_start, vax_unvax_start),
                           study_stop = study_stop,
                           cut_points = ifelse(c=="prevax", prevax_cuts, vax_unvax_cuts),
                           controls_per_case = controls_per_case,
                           total_event_threshold = total_event_threshold,
                           episode_event_threshold = episode_event_threshold,
                           covariate_threshold = covariate_threshold,
                           age_spline = FALSE,
                           analysis = paste0("sub_age_60_79","_",p))
      
      ## analysis: sub_age_80_110 ----
      df[nrow(df)+1,] <- c(cohort = c,
                           exposure = exposure, 
                           outcome = i,
                           ipw = ipw, 
                           strata = strata,
                           covariate_sex = covariate_sex,
                           covariate_age = covariate_age,
                           covariate_other = ifelse(p=="preex_FALSE", preex_FALSE_covars, all_covars),
                           cox_start = cox_start,
                           cox_stop = cox_stop,
                           study_start = ifelse(c=="prevax", prevax_start, vax_unvax_start),
                           study_stop = study_stop,
                           cut_points = ifelse(c=="prevax", prevax_cuts, vax_unvax_cuts),
                           controls_per_case = controls_per_case,
                           total_event_threshold = total_event_threshold,
                           episode_event_threshold = episode_event_threshold,
                           covariate_threshold = covariate_threshold,
                           age_spline = FALSE,
                           analysis = paste0("sub_age_80_110","_",p))
      
      ## analysis: sub_ethnicity_white ----
      df[nrow(df)+1,] <- c(cohort = c,
                           exposure = exposure, 
                           outcome = i,
                           ipw = ipw, 
                           strata = strata,
                           covariate_sex = covariate_sex,
                           covariate_age = covariate_age,
                           covariate_other = ifelse(p=="preex_FALSE", gsub("cov_cat_ethnicity;","",preex_FALSE_covars), gsub("cov_cat_ethnicity;","",all_covars)),
                           cox_start = cox_start,
                           cox_stop = cox_stop,
                           study_start = ifelse(c=="prevax", prevax_start, vax_unvax_start),
                           study_stop = study_stop,
                           cut_points = ifelse(c=="prevax", prevax_cuts, vax_unvax_cuts),
                           controls_per_case = controls_per_case,
                           total_event_threshold = total_event_threshold,
                           episode_event_threshold = episode_event_threshold,
                           covariate_threshold = covariate_threshold,
                           age_spline = TRUE,
                           analysis = paste0("sub_ethnicity_white","_",p))
      
      ## analysis: sub_ethnicity_black ----
      df[nrow(df)+1,] <- c(cohort = c,
                           exposure = exposure, 
                           outcome = i,
                           ipw = ipw, 
                           strata = strata,
                           covariate_sex = covariate_sex,
                           covariate_age = covariate_age,
                           covariate_other = ifelse(p=="preex_FALSE", gsub("cov_cat_ethnicity;","",preex_FALSE_covars), gsub("cov_cat_ethnicity;","",all_covars)),
                           cox_start = cox_start,
                           cox_stop = cox_stop,
                           study_start = ifelse(c=="prevax", prevax_start, vax_unvax_start),
                           study_stop = study_stop,
                           cut_points = ifelse(c=="prevax", prevax_cuts, vax_unvax_cuts),
                           controls_per_case = controls_per_case,
                           total_event_threshold = total_event_threshold,
                           episode_event_threshold = episode_event_threshold,
                           covariate_threshold = covariate_threshold,
                           age_spline = TRUE,
                           analysis = paste0("sub_ethnicity_black","_",p))
      
      ## analysis: sub_ethnicity_mixed ----
      df[nrow(df)+1,] <- c(cohort = c,
                           exposure = exposure, 
                           outcome = i,
                           ipw = ipw, 
                           strata = strata,
                           covariate_sex = covariate_sex,
                           covariate_age = covariate_age,
                           covariate_other = ifelse(p=="preex_FALSE", gsub("cov_cat_ethnicity;","",preex_FALSE_covars), gsub("cov_cat_ethnicity;","",all_covars)),
                           cox_start = cox_start,
                           cox_stop = cox_stop,
                           study_start = ifelse(c=="prevax", prevax_start, vax_unvax_start),
                           study_stop = study_stop,
                           cut_points = ifelse(c=="prevax", prevax_cuts, vax_unvax_cuts),
                           controls_per_case = controls_per_case,
                           total_event_threshold = total_event_threshold,
                           episode_event_threshold = episode_event_threshold,
                           covariate_threshold = covariate_threshold,
                           age_spline = TRUE,
                           analysis = paste0("sub_ethnicity_mixed","_",p))
      
      ## analysis: sub_ethnicity_asian ----
      df[nrow(df)+1,] <- c(cohort = c,
                           exposure = exposure, 
                           outcome = i,
                           ipw = ipw, 
                           strata = strata,
                           covariate_sex = covariate_sex,
                           covariate_age = covariate_age,
                           covariate_other = ifelse(p=="preex_FALSE", gsub("cov_cat_ethnicity;","",preex_FALSE_covars), gsub("cov_cat_ethnicity;","",all_covars)),
                           cox_start = cox_start,
                           cox_stop = cox_stop,
                           study_start = ifelse(c=="prevax", prevax_start, vax_unvax_start),
                           study_stop = study_stop,
                           cut_points = ifelse(c=="prevax", prevax_cuts, vax_unvax_cuts),
                           controls_per_case = controls_per_case,
                           total_event_threshold = total_event_threshold,
                           episode_event_threshold = episode_event_threshold,
                           covariate_threshold = covariate_threshold,
                           age_spline = TRUE,
                           analysis = paste0("sub_ethnicity_asian","_",p))
      
      ## analysis: sub_ethnicity_other ----
      df[nrow(df)+1,] <- c(cohort = c,
                           exposure = exposure, 
                           outcome = i,
                           ipw = ipw, 
                           strata = strata,
                           covariate_sex = covariate_sex,
                           covariate_age = covariate_age,
                           covariate_other = ifelse(p=="preex_FALSE", gsub("cov_cat_ethnicity;","",preex_FALSE_covars), gsub("cov_cat_ethnicity;","",all_covars)),
                           cox_start = cox_start,
                           cox_stop = cox_stop,
                           study_start = ifelse(c=="prevax", prevax_start, vax_unvax_start),
                           study_stop = study_stop,
                           cut_points = ifelse(c=="prevax", prevax_cuts, vax_unvax_cuts),
                           controls_per_case = controls_per_case,
                           total_event_threshold = total_event_threshold,
                           episode_event_threshold = episode_event_threshold,
                           covariate_threshold = covariate_threshold,
                           age_spline = TRUE,
                           analysis = paste0("sub_ethnicity_other","_",p))
      
      ## analysis: sub_smoking_never ----
      df[nrow(df)+1,] <- c(cohort = c,
                           exposure = exposure, 
                           outcome = i,
                           ipw = ipw, 
                           strata = strata,
                           covariate_sex = covariate_sex,
                           covariate_age = covariate_age,
                           covariate_other = ifelse(p=="preex_FALSE", gsub("cov_cat_smoking_status;","",preex_FALSE_covars), gsub("cov_cat_smoking_status;","",all_covars)),
                           cox_start = cox_start,
                           cox_stop = cox_stop,
                           study_start = ifelse(c=="prevax", prevax_start, vax_unvax_start),
                           study_stop = study_stop,
                           cut_points = ifelse(c=="prevax", prevax_cuts, vax_unvax_cuts),
                           controls_per_case = controls_per_case,
                           total_event_threshold = total_event_threshold,
                           episode_event_threshold = episode_event_threshold,
                           covariate_threshold = covariate_threshold,
                           age_spline = TRUE,
                           analysis = paste0("sub_smoking_never","_",p))
      
      ## analysis: sub_smoking_ever ----
      df[nrow(df)+1,] <- c(cohort = c,
                           exposure = exposure, 
                           outcome = i,
                           ipw = ipw, 
                           strata = strata,
                           covariate_sex = covariate_sex,
                           covariate_age = covariate_age,
                           covariate_other = ifelse(p=="preex_FALSE", gsub("cov_cat_smoking_status;","",preex_FALSE_covars), gsub("cov_cat_smoking_status;","",all_covars)),
                           cox_start = cox_start,
                           cox_stop = cox_stop,
                           study_start = ifelse(c=="prevax", prevax_start, vax_unvax_start),
                           study_stop = study_stop,
                           cut_points = ifelse(c=="prevax", prevax_cuts, vax_unvax_cuts),
                           controls_per_case = controls_per_case,
                           total_event_threshold = total_event_threshold,
                           episode_event_threshold = episode_event_threshold,
                           covariate_threshold = covariate_threshold,
                           age_spline = TRUE,
                           analysis = paste0("sub_smoking_ever","_",p))
      
      ## analysis: sub_smoking_current ----
      df[nrow(df)+1,] <- c(cohort = c,
                           exposure = exposure, 
                           outcome = i,
                           ipw = ipw, 
                           strata = strata,
                           covariate_sex = covariate_sex,
                           covariate_age = covariate_age,
                           covariate_other = ifelse(p=="preex_FALSE", gsub("cov_cat_smoking_status;","",preex_FALSE_covars), gsub("cov_cat_smoking_status;","",all_covars)),
                           cox_start = cox_start,
                           cox_stop = cox_stop,
                           study_start = ifelse(c=="prevax", prevax_start, vax_unvax_start),
                           study_stop = study_stop,
                           cut_points = ifelse(c=="prevax", prevax_cuts, vax_unvax_cuts),
                           controls_per_case = controls_per_case,
                           total_event_threshold = total_event_threshold,
                           episode_event_threshold = episode_event_threshold,
                           covariate_threshold = covariate_threshold,
                           age_spline = TRUE,
                           analysis = paste0("sub_smoking_current","_",p))
      
    }
    
  }
  
}

# Add name for each analysis ----

df$name <- paste0("cohort_",df$cohort, "-", 
                  df$analysis, "-", 
                  gsub("out_date_","",df$outcome))

# Check names are unique and save active analyses list ----

if (!dir.exists("lib")) {
  dir.create("lib")
}
if (length(unique(df$name))==nrow(df)) {
  saveRDS(df, file = "lib/active_analyses.rds", compress = "gzip")
} else {
  stop(paste0("ERROR: names must be unique in active analyses table"))
}