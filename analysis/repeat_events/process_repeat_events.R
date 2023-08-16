library(tidyverse)
library(jsonlite)
library(here)
library(glue)

# Load repeat_events_steps ------------------------------------------------
# source rather than loading so that it runs checks
source(here::here("analysis", "repeat_events", "repeat_events_steps.R"))

input_repeat_events_path <- "input_repeat_events_{i}.csv.gz"

i<-1
data_repeat_events <- 
  read_csv(here("output", "repeat_events", glue(input_repeat_events_path))) %>%
  select(patient_id, starts_with("out_date_"))

for (i in repeat_events_steps$step[-1]) {
  
  index_event <- repeat_events_steps$upper[i-1]
  
  data_repeat_events_i <- 
    read_csv(here("output", "repeat_events", glue(input_repeat_events_path))) %>%
    select(-matches(glue("out_date_\\w+_{index_event}")))
  
  data_repeat_events <- data_repeat_events %>%
    left_join(data_repeat_events_i, by = "patient_id")
  
}


# Modify dummy data so that IDs match those from stage1 -----------------------

if(Sys.getenv("OPENSAFELY_BACKEND") %in% c("", "expectations")) {
  source("analysis/repeat_events/modify_repeat_events_dummy_data.R")
  message("Repeat events IDs overwritten successfully")
}

# Check number of outcome variables matches the max number of events for each outcome -----------

max_events_JSON <- fromJSON("output/repeat_events/max_events.json")
max_events <- as.data.frame(max_events_JSON)

for (i in c("asthma_exac",
            "breathless",
            "copd_exac",
            "cough",
            "urti")) {

  data_repeat_events_outcome <- data_repeat_events %>%
    select(patient_id, contains(i))

  if (max_events[[i]] != ncol(data_repeat_events_outcome)-1) {
    stop(paste0("Number of ", i, " outcome variables does not match maximum number of ", i, " events"))
  }

}

# Remove events occurring outside study period --------------------------------------------
for (cohort in c("prevax", "unvax", "vax")) {

  stage1_cohort <- read_rds(file.path("output", paste0("input_", cohort, "_stage1.rds"))) %>%
    select(patient_id, index_date, exposure_date =exp_date_covid19_confirmed, end_date_outcome) %>%
    mutate(exposure_date = if_else(
      !is.na(exposure_date) & (exposure_date < index_date | exposure_date >= end_date_outcome),
      as.Date(NA),
      as.Date(exposure_date)))

  data_repeat_events <- data_repeat_events %>%
    inner_join(stage1_cohort[, c("patient_id", "index_date", "end_date_outcome")], by = "patient_id")

  data_repeat_events <- data_repeat_events %>%
    mutate(
      across(
        contains("out_date_"),
          ~ if_else(!is.na(.x) & (.x < index_date | .x > end_date_outcome),
                as.Date(NA),
                as.Date(.x))
          )
      ) %>%
        select(!c("index_date", "end_date_outcome"))

  # Reshape repeat events data ----------------------------------------------------
  # reshape data
  data_repeat_events_long <- data_repeat_events %>%
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
    mutate(across(outcome, ~str_remove_all(.x, "out_date_|_\\d+")))

  # Reshape stage1 data ----------------------------------------------------------
  stage1_cohort_long <- stage1_cohort %>%
    pivot_longer(
      cols = contains("date"),
      names_to = "date_label",
      values_to = "date",
      values_drop_na = TRUE
    )

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
                      out_date_end_new = if_else(
                        diff <= episode_length,
                        lead(out_date_end), 
                        out_date_end)
                      ) %>%
                        filter(diff_lag > episode_length) %>%
                          select(patient_id, episode_start = out_date, episode_end = out_date_end_new) %>%
                            pivot_longer(
                              cols = starts_with("episode"),
                              names_to = "date_label",
                              values_to = "date")

      # Add index_date, exposure_date, end_date_outcome ------------------------------------
      data_repeat_events_episodes <- data_repeat_events_episodes %>%
       rbind(stage1_cohort_long) %>%
         group_by(patient_id) %>%
           arrange(date, .by_group = TRUE)
      
      # Remove if episode end date is after end_date_outcome -------------------------------
      data_repeat_events_episodes <- data_repeat_events_episodes %>%
        merge(stage1_cohort[, c("patient_id", "end_date_outcome")], by = "patient_id") %>%
          filter(!(date_label == "episode_end" & date >= end_date_outcome)) %>%
            select(!end_date_outcome)

    # Save data --------------------------------------------------------------------------
    saveRDS(data_repeat_events_episodes, file = file.path("output", "repeat_events", paste0("repeat_events_", cohort, "_", outcome_name, "_", population, ".rds")), compress = "gzip")

    }

  }

}