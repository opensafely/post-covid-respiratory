library(tidyverse)

# Specify command arguments ----------------------------------------------------

args <- commandArgs(trailingOnly=TRUE)

if(length(args)==0){
  # use for interactive testing
  cohort <- "vax"
  
} else {
  cohort <- args[[1]]
}

# Input stage 1 cohort file ----------------------------------------------------
stage1_cohort <- read_rds(file.path("output", paste0("input_", cohort, "_stage1.rds"))) %>%
  select(patient_id, index_date, exposure_date =exp_date_covid19_confirmed, end_date_outcome) %>%
  mutate(exposure_date = if_else(
    !is.na(exposure_date) & (exposure_date < index_date | exposure_date >= end_date_outcome),
    as.Date(NA),
    as.Date(exposure_date)))

# Input repeat events file ----------------------------------------------------------------
data_repeat_events_cohort <- read_rds(file.path("output", "repeat_events", "data_repeat_events.rds")) %>%
  # Remove events occurring outside study period
  inner_join(stage1_cohort[, c("patient_id", "index_date", "end_date_outcome")], by = "patient_id")

# Reshape repeat events data ----------------------------------------------------
# reshape data
data_repeat_events_long <- data_repeat_events_cohort %>%
  # reshape to long
  pivot_longer(
    # select columns to reshape
    cols = starts_with("out_date_"),
    # set names of columns in the long data
    names_to = "outcome",
    values_to = "out_date",
    # drop rows where is.na(out_date)
    values_drop_na = TRUE
  ) %>%
  # tidy up the outcome column so it only shows the event type
  mutate(across(outcome, ~str_remove_all(.x, "out_date_|_\\d+"))) %>%
  # drop outcome events that don't happen between index_date and end_date_outcome (inclusive)
  filter(between(out_date, index_date, end_date_outcome)) %>%
  select(!c("index_date", "end_date_outcome"))

rm(data_repeat_events_cohort)

# Reshape stage1 data ----------------------------------------------------------
stage1_cohort_long <- stage1_cohort %>%
  select(-exposure_date) %>%
  pivot_longer(
    cols = contains("date"),
    names_to = "date_label",
    values_to = "date",
    values_drop_na = TRUE
  )

# Keep exposure dates for later -----------------------------------------------
data_exposure_dates <- stage1_cohort %>%
  select(patient_id, exposure_date) %>%
  filter(!is.na(exposure_date))

rm(stage1_cohort)

# Create episode data ----------------------------------------------------------
population_episode_length <- tibble(
  no_preexisting = 7,
  preexisting = 14
)

for (population in c("no_preexisting", "preexisting")) {
  
  episode_length <- population_episode_length[[population]]
  
  for (outcome_name in c("asthma_exac",
                         "breathless",
                         "copd_exac",
                         "cough",
                         "urti")) {
    
    data_repeat_events_episodes <- data_repeat_events_long %>%
      filter(outcome == outcome_name) %>%
      arrange(patient_id, out_date) %>%
      group_by(patient_id) %>%
      mutate(
        out_date_end = out_date + episode_length,
        diff = c(as.integer(diff(out_date_end)), episode_length + 1),
        diff_lag = lag(diff, default = episode_length + 1)
      ) %>%
      ungroup() %>%
      mutate(
        episode_start_indicator = case_when(
          # flag the start of episodes of length 1
          (diff > episode_length) & (diff_lag > episode_length) ~ TRUE,
          # flag the start of episodes of length > 1
          (diff <= episode_length) & (diff_lag > episode_length) ~ TRUE,
          # otherwise not an episode start
          TRUE ~ FALSE
        ),
        # create a distinct id for each episode
        episode_id = cumsum(episode_start_indicator)
      ) %>% 
      # within episode id, get the earliest out_date and latest out_date_end
      # note: don't need to patient_id in group_by(), episode_id is sufficient
      group_by(episode_id) %>%
      summarise(
        # first(patient_id) is quicker than grouping by patient id
        patient_id = first(patient_id),
        episode_start = min(out_date),
        episode_end = max(out_date_end),
        .groups = "drop"
      ) %>%
      mutate(episode_length = as.integer(episode_end - episode_start))
    
    stopifnot(
      "Some values of `data_repeat_events_episodes$episode_length` are less than `episode_length`." = 
        all(data_repeat_events_episodes$episode_length >= episode_length)
    )
    
    data_repeat_events_episodes_long <- data_repeat_events_episodes %>%
      select(-episode_id, -episode_length) %>%
      pivot_longer(
        cols = starts_with("episode"),
        names_to = "date_label",
        values_to = "date") 
    
    rm(data_repeat_events_episodes)
    
    # Add index_date and end_date_outcome ------------------------------------
    data_repeat_events_episodes_long <- data_repeat_events_episodes_long %>%
      rbind(stage1_cohort_long) %>%
      arrange(patient_id, date)
    
    # Remove if episode start or end is >= end_date_outcome -------------------------------
    data_repeat_events_episodes_long <- data_repeat_events_episodes_long %>%
      group_by(patient_id) %>%
      # remove any dates after the patient's outcome date where date_label != "end_date_outcome"
      # note: this only works when grouped by patient_id
      filter(!((date >= date[date_label == "end_date_outcome"]) & date_label != "end_date_outcome")) %>%
      ungroup()
    
    # add column of exposure dates -------------------------------------------
    data_repeat_events_episodes_long <- data_repeat_events_episodes_long %>%
      left_join(data_exposure_dates, by = "patient_id")
    
    # Save data --------------------------------------------------------------------------
    write.csv(data_repeat_events_episodes_long,
              file = file.path("output", "repeat_events", paste0("repeat_events_", cohort, "_", outcome_name, "_", population, ".csv")),
              row.names=FALSE)
    
    rm(data_repeat_events_episodes_long)
    
  }
  
}
