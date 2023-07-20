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

# Load repeat_events_increments ------------------------------------------------
repeat_events_increments <- read_csv(
  here("lib", "repeat_events_increments.csv")
)

# Load repeat_events_1 data ----------------------------------------------------
print(glue("Load input_repeat_events_{step}.csv.gz"))

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
  
  print("repeat_events_increments:")
  print(repeat_events_increments)
  stopifnot(
    "Re-define `repeat_events_increments$max` such values are <= `max(df_n$n)` for all but the final step." 
    = min(df_n$n) >= max(repeat_events_increments$min)
  )
  stopifnot(
    "Re-define `max(repeat_events_increments$max)` such that it is >= `max(df_n$n)`." 
    = max(df_n$n) <= max(repeat_events_increments$max)
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
  for (i in repeat_events_increments$step[-1]) {
    # save ids for all patients with >= repeat_events_increments$min[i] events
    patient_ids <- df %>% 
      filter_at(
        vars(starts_with("out_n_")),
        any_vars(. >= repeat_events_increments$min[i])
        ) %>%
      select(patient_id) 
    write_rds(
      patient_ids,
      here("output", "repeat_events", glue("patient_ids_{i}.rds")),
      compress = "gz"
    )
    print(glue(nrow(patient_ids), " patients with >= {repeat_events_increments$min[i]} events for any outcome"))
    rm(patient_ids)
  }
  cat("----\n")
  rm(df_out_n)
  
}

# Save ids and out_date_*_x for anyone with >x events for any outcome ------

# define params
n_index <- repeat_events_increments$max[step - 1]
n_min <- repeat_events_increments$min[step]
n_max <- repeat_events_increments$max[step]

patient_ids_keep <- read_rds(here("output", "repeat_events", glue("patient_ids_{step}.rds")))

df_dates <- df %>%
  # only keep the patients with >=n_min of any event type
  right_join(patient_ids_keep, by = "patient_id") %>%
  # only keep the dates of their n_index events
  select(patient_id, matches(glue("out_date_\\w+_{n_index}"))) %>%
  # make sure dates are formatted correctly
  mutate(across(matches(glue("out_date_\\w+_{n_index}")), as.Date)) 

write_csv(
  df_dates,
  here("output", "repeat_events", glue("out_date_{step}.csv.gz"))
)
