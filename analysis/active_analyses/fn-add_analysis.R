# Helper function to add rows ----
add_analysis <- function(
  cohort,
  outcome,
  analysis_name,
  covariate_other,
  age_spline
) {
  # Define dates ----
  study_dates <- fromJSON("output/study_dates.json")
  dates <- list(
    prevax_start = study_dates$pandemic_start,
    vax_unvax_start = study_dates$delta_date,
    study_stop = study_dates$lcd_date
  )
  study_start <- ifelse(
    cohort == "prevax",
    dates$prevax_start,
    dates$vax_unvax_start
  )

  # Define cut points ----

  cut_points_list <- list(
    prevax = "1;28;183;365;730;1095;1460;1979",
    vax_unvax = "1;28;183;365;730;1095;1460",
    vax_unvax_day0_FALSE = "28;183;365;730;1095;1460"
  )

  cut_points <- ifelse(
    cohort == "prevax",
    cut_points_list$prevax,
    cut_points_list$vax_unvax
  )

  # The model needs to collaspse day0 to the first interval

  if (
    cohort == "vax" &&
    outcome == "out_date_copd" &&
    analysis_name == "sub_age_18_39_preex_FALSE"
  ) {
    cut_points <-  gsub("1;", "", cut_points_list$vax_unvax)
  }

  # Define sampling ----
  ipw <- ifelse(
    cohort == "unvax",
    FALSE,
    TRUE
  )

  new_analysis <- c(
    cohort = cohort,
    exposure = "exp_date_covid",
    outcome = outcome,
    ipw = ipw,
    strata = "strat_cat_region",
    covariate_sex = ifelse(
      grepl("sex", analysis_name),
      "NULL",
      "cov_cat_sex"
    ),
    covariate_age = "cov_num_age",
    covariate_other = covariate_other,
    cox_start = "index_date",
    cox_stop = "end_date_outcome",
    study_start = study_start,
    study_stop = dates$study_stop,
    cut_points = cut_points,
    controls_per_case = 20L,
    total_event_threshold = 50L,
    episode_event_threshold = 5L,
    covariate_threshold = 5L,
    age_spline = age_spline,
    analysis = analysis_name
  )

  return(new_analysis)
}
