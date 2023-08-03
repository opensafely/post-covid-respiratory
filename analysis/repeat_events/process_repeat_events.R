library(tidyverse)
library(rjson)
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

# TODO
# run some checks to make sure that the max events extracted matches those in max_events.json
# for each cohort:
# - filter the events to only keep those that occured when the patient was under follow-up
# - create episodes from the filtered data
# - etc.


# Check number of outcome variables matches the max number of events for each outcome -----------

max_events_JSON <- fromJSON(file = "output/repeat_events/max_events.json")
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


# Remove events occurring outside study period for each cohort
for (cohort in c("prevax", "unvax", "vax")) {

stage1_cohort <- read_rds(file.path("output", paste0("input_", cohort, "_stage1.rds")))

stage1_cohort <- stage1_cohort %>%
  select(patient_id, index_date, end_date_outcome)

data_repeat_events_dates <- data_repeat_events %>%
  inner_join(stage1_cohort, by = "patient_id")


data_repeat_events_dates <- data_repeat_events_dates %>%
  # reshape to long
  pivot_longer(
    # select columns to reshape
    cols = starts_with("out_date_"),
    # set names of columns in the long data
    names_to = "event",
    values_to = "out_date",
    # drop rows where is.na(out_date)
    values_drop_na = TRUE
  ) %>%
  # tidy up the event column so it only shows the event type
  mutate(across(event, ~str_remove_all(.x, "out_date_|_\\d+"))) %>%
  # only rows where out_date is between index_date and end_date_outcome (inclusive)
  filter(between(out_date, index_date, end_date_outcome))
  # I'm not sure if you need to keep index_date and end_date_outcome in this 
  # dataset? If not, remove to avoid taking up unecessary storage.

saveRDS(data_repeat_events_dates, file = file.path("output", paste0("repeat_events_long_",cohort,".rds")), compress = "gzip")

rm(data_repeat_events_dates)

}

