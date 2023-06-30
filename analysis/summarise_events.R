library(tidyverse)  

# read the data and create cohort variable
df <- map_df(
  c("prevax", "unvax", "vax"),
  ~readr::read_csv(file = paste0("output/input_",.x,".csv.gz")) %>% 
    mutate(cohort = .x)
)

# bin the number of results into categories and count the number of patients 
# in each category (within cohort and outcome)
results <- df %>%
  select(cohort, starts_with("out_date_")) %>%
  mutate(
    across(
      starts_with("out_date_"),
      ~cut(.x, breaks = c(0,1,2,3,5,10,20,50,100,Inf), right = FALSE)
    )
  ) %>%
  pivot_longer(
    cols = -cohort
  ) %>%
  group_by(cohort, name, value) %>%
  count() %>%
  ungroup() %>%
  pivot_wider(names_from = value, values_from = n) %>%
  # replace NAs with zeros
  mutate(across(where(is.numeric), ~ replace_na(.x, 0L))) 
           
  roundmid_any <- function(x, to=1){
    # like ceiling_any, but centers on (integer) midpoint of the rounding points
    ceiling(x/to)*to - (floor(to/2)*(x!=0))
  }
  
  # save unrounded results
  results %>%
    mutate(total = rowSums(across(where(is.numeric)))) %>%
    write.csv("output/summarise_events_counts.csv", row.names = FALSE)  
  
  # save midpoint6 results
  results %>%
    mutate(across(where(is.numeric), ~roundmid_any(.x, to = 6))) %>%
    mutate(total = rowSums(across(where(is.numeric)))) %>%
    write.csv("output/summarise_events_counts_midpoint6.csv", row.names = FALSE)  
  