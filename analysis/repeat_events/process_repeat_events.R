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
  # remove exposure dates that do not occur during cohort follow-up
  mutate(exposure_date = if_else(
    !is.na(exposure_date) & (exposure_date < index_date | exposure_date >= end_date_outcome),
    as.Date(NA),
    as.Date(exposure_date)))

# Input repeat events long file ------------------------------------------------
data_repeat_events_long <- 
  read_rds(file.path("output", "repeat_events", "data_repeat_events_long.rds")) 

# Create episode data ----------------------------------------------------------
population_episode_length <- tibble(
  no_preexisting = 7,
  preexisting = 14
)

for (population in c("no_preexisting", "preexisting")) { 
  episode_length <- population_episode_length[[population]]

  # Inner join data_repeat_events_long and stage1_cohort -------------------------
  # (this will include patients in the cohort who have at least one outcome event)
  data_repeat_events_long_cohort <- data_repeat_events_long %>%
    inner_join(stage1_cohort, by = "patient_id") %>%
    # drop outcome events that don't happen between index_date and end_date_outcome (inclusive)
    filter(between(out_date, index_date - episode_length, end_date_outcome)) %>%
    select(!c("index_date", "end_date_outcome"))

  for (outcome_name in c("asthma_exac",
                         "breathless",
                         "copd_exac",
                         "cough",
                         "urti")) {
  
    data_repeat_events_episodes <- data_repeat_events_long_cohort %>%
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
        exposure_date = first(exposure_date),
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
    
    # Update index date with end of first episode ---------------------------
    # if patient was in an episode at index date
    stage1_cohort_index <- data_repeat_events_episodes %>%
      select(-episode_id, -episode_length) %>%
      group_by(patient_id) %>%
        filter(row_number()==1) %>%
          inner_join(stage1_cohort[, c("patient_id", "index_date", "end_date_outcome")], by = "patient_id") %>%
            mutate(index_date = if_else(
              episode_start <= index_date,
              episode_end,
              index_date)
            ) %>%
              select(!c("episode_start", "episode_end"))

    rm(data_repeat_events_episodes)

    # Reshape stage1 data ----------------------------------------------------------
    stage1_cohort_long <- stage1_cohort_index %>%
        pivot_longer(
        cols = c("index_date", "end_date_outcome"),
        names_to = "date_label",
        values_to = "date",
        values_drop_na = TRUE
      )

    # Add index_date and end_date_outcome ------------------------------------
    data_repeat_events_episodes_long <- data_repeat_events_episodes_long %>%
      rbind(stage1_cohort_long) %>%
      arrange(patient_id, date)

    # Remove episodes before index date
    data_repeat_events_episodes_long <- data_repeat_events_episodes_long %>%
      inner_join(stage1_cohort_index[, c("patient_id", "index_date")], by = "patient_id") %>%
        filter(!((date_label == "episode_start" | date_label == "episode_end") & date <= index_date)) %>%
          select(!index_date)
    
    rm(stage1_cohort_index)

    # Remove if episode start or end is >= end_date_outcome -------------------------------
    data_repeat_events_episodes_long <- data_repeat_events_episodes_long %>%
      group_by(patient_id) %>%
      # remove any dates after the patient's outcome date where date_label != "end_date_outcome"
      # note: this only works when grouped by patient_id
      filter(!((date >= date[date_label == "end_date_outcome"]) & date_label != "end_date_outcome")) %>%
      ungroup()
    
    # Save data --------------------------------------------------------------------------
    write.csv(data_repeat_events_episodes_long,
              file = file.path("output", "repeat_events", paste0("repeat_events_", cohort, "_", outcome_name, "_", population, ".csv")),
              row.names=FALSE)
    
    rm(data_repeat_events_episodes_long)
    
  }
  
}
