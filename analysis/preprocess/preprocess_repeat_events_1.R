library(tidyverse)  
library(glue)

# Create output directory ------------------------------------------------------
outdir <- here::here("output", "repeat_events")
fs::dir_create(outdir)

# Load repeat_events_increments ------------------------------------------------
repeat_events_increments <- read_csv(
  here::here("lib", "repeat_events_increments.csv")
)

# Load repeat_events_1 data ----------------------------------------------------
print('Load repeat_events_1 data')

df <- readr::read_csv(file = "output/repeat_events/input_repeat_events_1.csv.gz")
print(paste0("Dataset has been read successfully with N = ", nrow(df), " rows"))

# Reshape data to get max events ----------------------------------------------
df_n <- df %>%
  select(patient_id, starts_with("out_n_")) %>%
  pivot_longer(cols = -patient_id) %>%
  mutate(across(name, ~str_remove(.x, "out_n_"))) %>%
  # get the max events for each outcome
  group_by(name) %>%
  summarise(n = max(value)) %>%
  ungroup() 

cat("Max events for each outcome\n")
print(df_n)
stopifnot(
  "Re-define `repeat_events_increments$min` such that the maximum is greater than the max events across all outcomes." 
  = min(df_n$n) >= max(repeat_events_increments$min)
  )

# Save max events results ------------------------------------------------------
max_events <- as.list(df_n$n)
names(max_events) <- df_n$name
# save as json to read into study definition
jsonlite::write_json(
  max_events, 
  path = file.path(outdir, "max_events.json"),
  auto_unbox=TRUE, 
  pretty =TRUE
)
rm(df_n)

# Save ids and 5th outcome date for anyone with >x events for any outcome ------
for (x in seq_along(repeat_events_increments$min)[-1]) {

n_min <- repeat_events_increments$min[x]
n_max <- repeat_events_increments$max[x]
n_index <- n_min-1
  
df_dates <- df %>%
  # only keep the patients with >=n_min of any event type
  filter_at(vars(starts_with("out_n_")), any_vars(.>=n_min)) %>%
  # only keep the dates of their n_index events
  select(patient_id, matches(glue("out_date_\\w+_{n_index}"))) %>%
  # make sure dates are formatted correctly
  mutate(across(matches(glue("out_date_\\w+_{n_index}")), as.Date)) 

write_csv(
  df_dates,
  file.path(outdir, glue("out_date_{n_index}.csv.gz"))
)

print(
  glue(
    "Patients with >= {n_min} events: N = ", nrow(df_dates),
    " (i.e. nrows in input_repeat_events_{n_min}to{n_max})"
    )
  )

rm(df_dates)

}
