library(tidyverse)

# Get stage 1 IDs --------------------------------------------------------------
stage1_ids_prevax <- read_rds(file.path("output/input_prevax_stage1.rds")) %>%
  select(patient_id)
stage1_ids_unvax <- read_rds(file.path("output/input_unvax_stage1.rds")) %>%
  select(patient_id)
stage1_ids_vax <- read_rds(file.path("output/input_vax_stage1.rds")) %>%
  select(patient_id)

stage1_ids <- rbind(stage1_ids_prevax, stage1_ids_unvax, stage1_ids_vax) %>%
  unique()


write.csv(stage1_ids, file = file.path("output", "stage1_ids.csv") , row.names=F)