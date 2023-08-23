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
  
  rm(data_repeat_events_i)
  
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
  
  rm(data_repeat_events_outcome)

}

# Reshape repeat events data ----------------------------------------------------
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
  mutate(across(outcome, ~ factor(str_remove_all(.x, "out_date_|_\\d+"))))

# Save data --------------------------------------------------------------------------
saveRDS(
  data_repeat_events_long, 
    file = file.path("output", "repeat_events", "data_repeat_events_long.rds"), 
    compress = "gzip"
)
