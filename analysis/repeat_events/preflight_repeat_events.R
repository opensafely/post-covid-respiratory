################################################################################
# This script creates and saves the following datasets which are necessary to  
# run the study_definition_repeat_events_{step} actions:
# 
# - where step==2:
#   - output/repeat_events/max_events.json (the maximum number of repeat events
#     for each outcome type extracted in study_definition_repeat_events_1)
#   - output/repeat_events/patient_ids_{step}.rds (the list of patient ids to be read 
#     into this and future preflight_repeat_events_{step} actions)
# 
# - where step>=2:
#   - output/repeat_events/out_date_{step}.csv.gz (a dataset with patient_ids 
#     and out_date_*_{i}) for all patients with >i events for any outcome type, 
#     to be read into study_definition_repeat_events_{step} actions
# 
################################################################################

library(tidyverse)  
library(glue)
library(here)

# Specify command arguments ----------------------------------------------------

args <- commandArgs(trailingOnly=TRUE)

if(length(args)==0){
  # use for interactive testing
  step <- 2L
  
} else {
  step <- as.integer(args[[1]])
}

stopifnot("No preflight required for step 1" = step > 1)

print(glue("Preflight for step {step}"))

# Load repeat_events_steps ------------------------------------------------
# source rather than loading so that it runs checks
source(here::here("analysis", "repeat_events", "repeat_events_steps.R"))

# Load repeat_events_{step-1} data ----------------------------------------------------
print(glue("Load input_repeat_events_{step-1}.csv.gz"))

df <- readr::read_csv(glue("output/repeat_events/input_repeat_events_{step-1}.csv.gz"))
print(paste0("Dataset has been read successfully with N = ", nrow(df), " rows"))

if (step == 2) {
  
  # only select count columns
  df_out_n <- df %>%
    select(patient_id, starts_with("out_n_"))
  
  # Reshape data to get max events ----------------------------------------------
  df_n <- df_out_n %>%
    pivot_longer(cols = -patient_id) %>%
    mutate(across(name, ~str_remove(.x, "out_n_"))) %>%
    # get the max events for each outcome
    group_by(name) %>%
    summarise(n = max(value)) %>%
    ungroup() 
  
  print("Max events for each outcome (df_n):")
  print(df_n)
  
  print("repeat_events_steps:")
  print(repeat_events_steps)
  stopifnot(
    "Re-define `repeat_events_steps$upper` such values are <= `max(df_n$n)` for all but the final step." 
    = min(df_n$n) >= max(repeat_events_steps$lower)
  )
  stopifnot(
    "Re-define `max(repeat_events_steps$max)` such that it is >= `max(df_n$n)`." 
    = max(df_n$n) <= max(repeat_events_steps$upper)
  )
  
  # Save max events results ----------------------------------------------------
  max_events <- as.list(df_n$n)
  names(max_events) <- df_n$name
  # save as json to read into study definition
  jsonlite::write_json(
    max_events, 
    path = here("output", "repeat_events", "max_events.json"),
    auto_unbox=TRUE, 
    pretty =TRUE
  )
  rm(df_n)
  
  # Save list of patient ids for each step -------------------------------------
  cat("----\n")
  for (i in repeat_events_steps$step[-1]) {
    # save ids for all patients with >= repeat_events_steps$lower[i] events
    patient_ids <- df %>% 
      filter_at(
        vars(starts_with("out_n_")),
        any_vars(. >= repeat_events_steps$lower[i])
        ) %>%
      select(patient_id) 
    write_rds(
      patient_ids,
      here("output", "repeat_events", glue("patient_ids_{i}.rds")),
      compress = "gz"
    )
    print(glue(nrow(patient_ids), " patients with >= {repeat_events_steps$lower[i]} events for any outcome"))
    rm(patient_ids)
  }
  cat("----\n")
  rm(df_out_n)
  
}

# Save ids and out_date_*_x for anyone with >x events for any outcome ------

# define params
n_index <- repeat_events_steps$upper[step - 1]
n_lower <- repeat_events_steps$lower[step]
n_upper <- repeat_events_steps$upper[step]

patient_ids_keep <- read_rds(here("output", "repeat_events", glue("patient_ids_{step}.rds")))

df_dates <- df %>%
  # only keep the patients with >=n_lower of any event type
  right_join(patient_ids_keep, by = "patient_id") %>%
  # only keep the dates of their n_index events
  select(patient_id, matches(glue("out_date_\\w+_{n_index}"))) %>%
  # make sure dates are formatted correctly
  mutate(across(matches(glue("out_date_\\w+_{n_index}")), as.Date)) 

write_csv(
  df_dates,
  here("output", "repeat_events", glue("out_date_{step}.csv.gz"))
)
