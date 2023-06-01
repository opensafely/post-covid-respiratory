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

for (cohort in c("prevax","vax","unvax")) {
  
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
      
      results[nrow(results)+1,] <- c(cohort,j,"Min",paste0(min(tmp, na.rm = TRUE), collapse = ";"))
      results[nrow(results)+1,] <- c(cohort,j,"Median",paste0(median(tmp, na.rm = TRUE), collapse = ";"))
      results[nrow(results)+1,] <- c(cohort,j,"Mean",paste0(mean(tmp, na.rm = TRUE), collapse = ";"))
      results[nrow(results)+1,] <- c(cohort,j,"Max",paste0(max(tmp, na.rm = TRUE), collapse = ";"))
      results[nrow(results)+1,] <- c(cohort,j,"Missing",paste0(sum(is.na(tmp)), collapse = ";"))
      
    } else{
      
      print(paste0(j,' is not numeric'))
      
    }
    
  }
  
  # Pivot to wide format -------------------------------------------------------
  print('Pivot to wide format')
  
  results <- tidyr::pivot_wider(results, names_from = "statistic", values_from = "value")
  
}

# Save results -----------------------------------------------------------------
print('Save results')

write.csv(results, paste0("output/summarise_events.csv"), row.names = FALSE)
