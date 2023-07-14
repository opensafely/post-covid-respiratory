library(tidyverse)  

# Generate empty dataset for results -------------------------------------------
print('Generate empty datasets for results')

res1_all <- tibble() 
res2_all <- tibble() 

# Generate summary data for each cohort ----------------------------------------
print('Generate summary data for each cohort')

for (cohort in c("prevax", "unvax", "vax")) {
  
  print(paste0('Cohort: ',cohort))
  
  # Load cohort data -----------------------------------------------------------
  print('Load cohort data')
  
  df <- readr::read_csv(file = paste0("output/input_",cohort,".csv.gz") )
  print(paste0("Dataset has been read successfully with N = ", nrow(df), " rows"))
  
  # reshape data
  df_long <- df %>%
    select(patient_id, starts_with("out_date_")) %>%
    pivot_longer(cols = -patient_id)
  rm(df)
  
  # identify number of patients with more than x events of any outcome
  res1 <- df_long %>%
    group_by(patient_id) %>%
    summarise(max_value = max(value)) %>%
    ungroup() %>%
    mutate(
      more_than_2 = max_value > 2,
      more_than_3 = max_value > 3,
      more_than_4 = max_value > 4,
      more_than_5 = max_value > 5
      ) %>%
    summarise(across(starts_with("more_than"), sum)) %>%
    # add cohort column
    mutate(cohort = cohort, .before = 1)
  
  # bin the number of results into categories and count the number of patients 
  # in each category (within cohort and outcome)
  res2 <- df_long %>%
    mutate(
      across(
        value,
        ~cut(
          .x, 
          breaks = c(0,1,2,3,5,10,20,50,100,Inf), 
          right = FALSE,
          ordered_result = TRUE
          )
      )
    ) %>%
    group_by(name, value) %>%
    count() %>%
    ungroup() %>%
    pivot_wider(names_from = value, values_from = n) %>%
    # replace NAs with zeros
    mutate(across(where(is.numeric), ~ replace_na(.x, 0L))) %>% 
    # add cohort column
    mutate(cohort = cohort, .before = 1)
  
  # bind results
  res1_all <- bind_rows(res1_all, res1) 
  res2_all <- bind_rows(res2_all, res2) 
  
  rm(res1, res2, df_long)
  
}

roundmid_any <- function(x, to=1){
  # like ceiling_any, but centers on (integer) midpoint of the rounding points
  ceiling(x/to)*to - (floor(to/2)*(x!=0))
}

res1_all %>%
  write.csv("output/summarise_overlap_counts.csv", row.names = FALSE)  

res1_all %>%
  mutate(across(where(is.numeric), ~roundmid_any(.x, to = 6))) %>%
  write.csv("output/summarise_overlap_counts_midpoint6.csv", row.names = FALSE)  

res2_all <- res2_all %>%
  # replace NAs with zeros
  mutate(across(where(is.integer), ~replace_na(.x, 0L))) %>%
  mutate(total = rowSums(across(where(is.numeric)))) 
  
# save unrounded results
res2_all %>%
  write.csv("output/summarise_events_counts.csv", row.names = FALSE)  

# save midpoint6 results
res2_all %>%
  mutate(across(where(is.numeric), ~roundmid_any(.x, to = 6))) %>%
  # recalculate totals
  mutate(total = rowSums(across(where(is.numeric)))) %>%
  write.csv("output/summarise_events_counts_midpoint6.csv", row.names = FALSE)  
  