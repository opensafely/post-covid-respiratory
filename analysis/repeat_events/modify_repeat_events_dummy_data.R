# Set seed ---------------------------------------------------------------------
set.seed(1)

# Replace IDs in repeat events data with stage1 IDs-----------------------------
stage1_ids <- readr::read_csv("output/stage1_ids.csv")

data_repeat_events <- data_repeat_events %>%
    slice_sample(n = nrow(stage1_ids))

data_repeat_events <- data_repeat_events %>%
    select(!patient_id) 

data_repeat_events <- cbind(patient_id = stage1_ids, data_repeat_events)



# Replace outcome dates with dates between 01/01/2020 and 14/12/2021 ----------

data_repeat_events2 <- data_repeat_events %>%
  mutate(
    across(
      contains("out_date_"),
      ~ ifelse(!is.na(.x),
               as.Date("2020-01-01") + sample(x = 1:713),
               as.Date(.x))
    )
  )
