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

# empty tibble for output
patients_removed <- tibble()

for (population in c("no_preexisting", "preexisting")) { 
  episode_length <- population_episode_length[[population]]

  # Inner join data_repeat_events_long and stage1_cohort -------------------------
  # (this will include patients in the cohort who have at least one outcome event)
  data_repeat_events_long_cohort <- data_repeat_events_long %>%
    inner_join(stage1_cohort, by = "patient_id") %>%
    # drop outcome events that don't happen between index_date - episode_length
    # and end_date_outcome (inclusive)
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
      select(patient_id, episode_start, episode_end)
    
    stopifnot(
      "Some values of `data_repeat_events_episodes$episode_length` are less than `episode_length`." = 
        all(
          data_repeat_events_episodes %>%
            transmute(check = as.integer(episode_end - episode_start) >= episode_length) %>%
            pull()
        )
    )
    
    # Update index date with end of first episode ---------------------------
    # if patient was in an episode at index date
    stage1_cohort_index <- stage1_cohort %>%
      # left join to keep all patients in the cohort, regardless of if they had 
      # an episode
      left_join(
        data_repeat_events_episodes %>%
          group_by(patient_id) %>%
          # get the first episode per patient (already sorted on episode dates)
          slice(1) %>%
          ungroup(),
        by = "patient_id"
      ) %>%
      # update index_date
      mutate(
        index_date = if_else(
          !is.na(episode_start) & (episode_start <= index_date),
          episode_end,
          index_date
        )
      ) %>%
      # remove patients where updated index_date >= end_date_outcome
      filter(index_date < end_date_outcome) %>%
      select(!c("episode_start", "episode_end"))
    
    # save the number of patients removed due to the updated index date
    patients_removed <- bind_rows(
      patients_removed,
      tibble(
        population = population,
        outcome_name = outcome_name,
        patients_removed = nrow(stage1_cohort) - nrow(stage1_cohort_index)
        )
    )
    
    # Reshape data_repeat_events_episodes --------------------------------------
    data_repeat_events_episodes_long <- stage1_cohort_index %>%
      # left join to keep all patients in the cohort, regardless of if they had 
      # an episode
      left_join(data_repeat_events_episodes, by = "patient_id") %>%
      mutate(
        # replace episode_start with NA unless between index_date and 
        # end_date_outcome (inclusive)
        episode_start = if_else(
          index_date <= episode_start & episode_start <= end_date_outcome,
          episode_start,
          as.Date(NA_character_)
        ),
        # replace episode_end with NA unless between index_date and 
        # end_date_outcome (not inclusive of index_date)
        episode_end = if_else(
          index_date < episode_end & episode_end <= end_date_outcome,
          episode_end,
          as.Date(NA_character_)
        )
      ) %>%
      # reshape
      pivot_longer(
        cols = c(starts_with("episode"), index_date, end_date_outcome),
        names_to = "date_label",
        values_to = "date",
        values_drop_na = TRUE
        ) %>%
      # apply distinct() to get rid of duplicate index_date and end_date_outcome rows
      distinct() %>%
      arrange(patient_id, date)
    
    # Save data --------------------------------------------------------------------------
    write.csv(data_repeat_events_episodes_long,
              file = file.path("output", "repeat_events", paste0("repeat_events_", cohort, "_", outcome_name, "_", population, ".csv")),
              row.names=FALSE)
    
    rm(data_repeat_events_episodes, data_repeat_events_episodes_long, stage1_cohort_index)
    
  }
  
}

# save the number of patients removed across all populations and outcomes
write_csv(
  patients_removed,
  file.path("output", "repeat_events", paste0("patients_removed_", cohort, ".csv"))
)
