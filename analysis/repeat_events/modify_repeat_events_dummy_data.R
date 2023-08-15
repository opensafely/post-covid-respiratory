# Set seed ---------------------------------------------------------------------
set.seed(1)

# Replace IDs in repeat events data with stage1 IDs-----------------------------
stage1_ids <- readr::read_csv("output/stage1_ids.csv")

data_repeat_events <- data_repeat_events %>%
    slice_sample(n = nrow(stage1_ids))

data_repeat_events <- data_repeat_events %>%
    select(!patient_id) 

data_repeat_events <- cbind(patient_id = stage1_ids, data_repeat_events)
