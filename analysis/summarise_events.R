library(tidyverse)  

# Generate empty dataset for results -------------------------------------------
print('Generate empty datasets for results')

results_all <- data.frame(cohort = character(),
                      name = character(),
                      events_0 = numeric(),
                      events_1 = numeric(),
                      events_2 = numeric(),
                      events_3_4 = numeric(),
                      events_5_9 = numeric(),
                      events_10_19 = numeric(),
                      events_20_49 = numeric(),
                      events_50_99 = numeric()
                     )

# Generate summary data for each cohort ----------------------------------------
print('Generate summary data for each cohort')

for (cohort in c("prevax", "unvax", "vax")) {
  
  print(paste0('Cohort: ',cohort))
  
  # Load cohort data -----------------------------------------------------------
  print('Load cohort data')
  
  df <- readr::read_csv(file = paste0("output/input_",cohort,".csv.gz") )
  print(paste0("Dataset has been read successfully with N = ", nrow(df), " rows"))
  

# bin the number of results into categories and count the number of patients 
# in each category (within cohort and outcome)
results <- df %>%
  select(starts_with("out_date_")) %>%
  mutate(
    across(
      starts_with("out_date_"),
      ~cut(.x, breaks = c(0,1,2,3,5,10,20,50,100,Inf), right = FALSE)
    )
  ) %>%
  pivot_longer(everything()) %>%
  group_by(name, value) %>%
  count() %>%
  ungroup() %>%
  pivot_wider(names_from = value, values_from = n) %>%
  # replace NAs with zeros
  mutate(across(where(is.numeric), ~ replace_na(.x, 0L))) 

# rename variables and add cohort
  results <- results %>% 
    mutate(cohort = cohort, .before = 1) %>% 
    rename(events_0 = `[0,1)`,
            events_1 = `[1,2)`,
            events_2 = `[2,3)`,
            events_3_4 = `[3,5)`,
            events_5_9 = `[5,10)`,
            events_10_19 = `[10,20)`
            events_20_49 = `[20,50),
            events_50_99 = `[50,100)
            )
  
  results_all <- rbind(results_all, results)

  }

  roundmid_any <- function(x, to=1){
    # like ceiling_any, but centers on (integer) midpoint of the rounding points
    ceiling(x/to)*to - (floor(to/2)*(x!=0))
  }
  
  # save unrounded results
  results_all %>%
    mutate(total = rowSums(across(where(is.numeric)))) %>%
    write.csv("output/summarise_events_counts.csv", row.names = FALSE)  
  
  # save midpoint6 results
  results_all %>%
    mutate(across(where(is.numeric), ~roundmid_any(.x, to = 6))) %>%
    mutate(total = rowSums(across(where(is.numeric)))) %>%
    write.csv("output/summarise_events_counts_midpoint6.csv", row.names = FALSE)  
  