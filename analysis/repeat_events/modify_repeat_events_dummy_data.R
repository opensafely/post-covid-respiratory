# Set seed ---------------------------------------------------------------------
set.seed(1)

# Replace IDs in repeat events data with stage1 IDs-----------------------------
stage1_ids <- readr::read_csv("output/stage1_ids.csv")

data_repeat_events <- data_repeat_events %>%
    slice_sample(n = nrow(stage1_ids))

data_repeat_events <- data_repeat_events %>%
    select(!patient_id) 

data_repeat_events <- cbind(patient_id = stage1_ids, data_repeat_events)

rm(stage1_ids)


# Increase number of patients with no events ----------------------------------

for (outcome in c("asthma_exac",
                         "breathless",
                         "copd_exac",
                         "cough",
                         "urti")) {

data_repeat_events <- data_repeat_events %>%
    mutate(rand_num = runif(n = nrow(data_repeat_events))) %>%
    mutate(
        across(
            contains(outcome),
            ~ if_else(rand_num <0.35,
            as.Date(NA),
            as.Date(.x))
        )
    )
}

data_repeat_events <- data_repeat_events %>%
    select(!rand_num)

