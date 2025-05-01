generate_time_interval_indicators <- function(df) {
  # Set dates based on YAML
  study_start <- as.Date("2021-06-01")
  study_stop <- as.Date("2024-04-30")

  # Extract exposure and outcome variable names from YAML
  exposure_var <- "exp_date"
  outcome_var <- "out_date"
  cox_start_var <- "index_date"
  cox_stop_var <- "end_date_outcome"

  # Cut points (days since exposure) from YAML
  cut_points <- as.numeric(c(1, 7, 14, 28, 56, 84, 183, 365, 730, 1065))

  # Rename to generic names for compatibility
  df <- dplyr::rename(df, exposure = !!exposure_var, outcome = !!outcome_var)

  df$study_start <- study_start
  df$study_stop <- study_stop

  df$fup_start <- pmax(df$study_start, df[[cox_start_var]], na.rm = TRUE)
  df$fup_stop <- pmin(
    df$study_stop,
    df[[cox_stop_var]],
    df$outcome,
    na.rm = TRUE
  )

  df <- df[df$fup_stop >= df$fup_start, ]

  # Constrain exposure and outcome to valid follow-up
  df$exposure[df$exposure < df$fup_start | df$exposure > df$fup_stop] <- NA
  df$outcome[df$outcome < df$fup_start | df$outcome > df$fup_stop] <- NA

  # Create outcome status flag
  df$outcome_status <- df$outcome == df$fup_stop & !is.na(df$outcome)

  # Generate episode labels
  time_period_labels <- paste0(
    "days",
    c(0, cut_points[-length(cut_points)]),
    "_",
    cut_points
  )

  episode_labels <- data.frame(
    episode = 0:length(cut_points),
    time_period = c("days_pre", time_period_labels),
    stringsAsFactors = FALSE
  )

  # Source the survival setup logic (assumed available)
  source("analysis/diagnosis/fn-survival_data_setup.R")

  df_surv <- survival_data_setup(
    df = df,
    cut_points = cut_points,
    episode_labels = episode_labels
  )

  return(df_surv)
}
