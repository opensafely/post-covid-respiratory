library(readr)
library(tidyr)

# Specify event prefix ---------------------------------------------------------
print('Specify event prefix')

var_name <- "out_date_" 

# Generate empty dataset for results -------------------------------------------
print('Generate empty datasets for results')

results <- data.frame(cohort = character(),
                      variable = character(),
                      statistic = character(),
                      value = numeric())

# Generate summary data for each cohort ----------------------------------------
print('Generate summary data for each cohort')

for (cohort in c("prevax", "unvax", "vax")) {
  
  print(paste0('Cohort: ',cohort))
  
  # Load cohort data -----------------------------------------------------------
  print('Load cohort data')
  
  df <- readr::read_csv(file = paste0("output/input_",cohort,".csv.gz") )
  print(paste0("Dataset has been read successfully with N = ", nrow(df), " rows"))
  
  # Restrict to relevant variables ---------------------------------------------
  print('Restrict to relevant variables')
  
  df <- df[,c("patient_id",colnames(df)[grepl(var_name,colnames(df))])]
  
  # Repeat for each variable ---------------------------------------------------
  
  for (j in colnames(df)[grepl(var_name,colnames(df))]) {
    
    tmp <- get(j, df)
    
    # Check variables are numeric ----------------------------------------------
    print(paste0('Check ',j,' is numeric'))
    
    if (is.numeric(tmp)==TRUE) {
      
      # Calculate summary statisics ----------------------------------------------
      print(paste0('Calculate summary statisics for ', j))
      
      results[nrow(results)+1,] <- c(cohort,j,"1%",quantile(tmp, 0.01, na.rm = TRUE))
      results[nrow(results)+1,] <- c(cohort,j,"5%",quantile(tmp, 0.05, na.rm = TRUE))
      results[nrow(results)+1,] <- c(cohort,j,"10%",quantile(tmp, 0.1, na.rm = TRUE))
      results[nrow(results)+1,] <- c(cohort,j,"25%",quantile(tmp, 0.25, na.rm = TRUE))
      results[nrow(results)+1,] <- c(cohort,j,"50%",quantile(tmp, 0.5, na.rm = TRUE))
      results[nrow(results)+1,] <- c(cohort,j,"75%",quantile(tmp, 0.75, na.rm = TRUE))
      results[nrow(results)+1,] <- c(cohort,j,"90%",quantile(tmp, 0.9, na.rm = TRUE))
      results[nrow(results)+1,] <- c(cohort,j,"95%",quantile(tmp, 0.95, na.rm = TRUE))
      results[nrow(results)+1,] <- c(cohort,j,"99%",quantile(tmp, 0.99, na.rm = TRUE))
      results[nrow(results)+1,] <- c(cohort,j,"Max",max(tmp, na.rm = TRUE))
      results[nrow(results)+1,] <- c(cohort,j,"Missing",sum(is.na(tmp)))
      
    } else{
      
      print(paste0(j,' is not numeric'))
      
    }
    
  }
  
}

 # Pivot to wide format -------------------------------------------------------
  print('Pivot to wide format')
  
  results <- tidyr::pivot_wider(results, names_from = "statistic", values_from = "value")

  # Save results -----------------------------------------------------------------
  print('Save results')
  
  write.csv(results, paste0("output/summarise_events_percentiles.csv"), row.names = FALSE)  
  
# alternative summary  -----------------------------------------------------------------  
library(tidyverse)  

# read the data and create cohort variable
df <- map_df(
  c("prevax", "unvax", "vax"),
  ~readr::read_csv(file = paste0("output/input_",.x,".csv.gz")) %>% 
    mutate(cohort = .x)
)

# bin the number of results into categories and count the number of patients 
# in each category (within cohort and outcome)
results2 <- df %>%
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
  results2 %>%
    mutate(total = rowSums(across(where(is.numeric)))) %>%
    write.csv("output/summarise_events_counts.csv", row.names = FALSE)  
  
  # save midpoint6 results
  results2 %>%
    mutate(across(where(is.numeric), ~roundmid_any(.x, to = 6))) %>%
    mutate(total = rowSums(across(where(is.numeric)))) %>%
    write.csv("output/summarise_events_counts_midpoint6.csv", row.names = FALSE)  
  