library(tidyverse)  

# Create output directory ------------------------------------------------------
outdir <- here::here("output", "preprocess")
fs::dir_create(outdir)

# Load repeat_events_1 data ----------------------------------------------------
print('Load repeat_events_1 data')

df <- readr::read_csv(file = "output/input_repeat_events_1.csv.gz")
print(paste0("Dataset has been read successfully with N = ", nrow(df), " rows"))

# Reshape data to get percentiles ----------------------------------------------
df_n <- df %>%
  select(patient_id, starts_with("out_n_")) %>%
  pivot_longer(cols = -patient_id) %>%
  mutate(across(name, ~str_remove(.x, "out_n_"))) %>%
  # get the 99.999th percentile
  # this means we can say we have all events for 99.999% of the patients
  group_by(name) %>%
  summarise(n = ceiling(unname(quantile(x=value, probs = 0.99999)))) %>%
  ungroup() 

# Save percentile results ------------------------------------------------------
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

# Save ids and 5th outcome date for anyone with >5 events for any outcome ------
df_dates <- df %>%
  # only keep the dates of the 5th events
  select(patient_id, matches("out_date_\\w+_5")) %>%
  mutate(across(matches("out_date_\\w+_5"), as.Date)) %>%
  # only keep rows where at least one date is nonmissing
  filter_at(vars(starts_with("out_date_")), any_vars(!is.na(.)))
write_csv(
  df_dates,
  file.path(outdir, glue::glue("out_date_5.csv.gz"))
)
rm(df_dates)
