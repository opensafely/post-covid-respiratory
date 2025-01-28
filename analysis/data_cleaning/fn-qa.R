# Function to apply quality assurance
qa <- function(input, study_dates, consort, cohort, threshold) {
  # Load libraries ---------------------------------------------------------------
  print('Load libraries')
  
  library(dplyr)
  library(tictoc)
  library(readr)
  library(tidyr)
  library(stringr)
  library(ggplot2)
  library(jsonlite)
  library(here)
  library(arrow)
  
  print('Source common functions')
  source("analysis/utility.R")
  
  print('Quality assurance: Year of birth is after year of death or patient only has year of death')
  
  input <- input[!((input$qa_num_birth_year > (format(input$cens_date_death, format="%Y")) & 
                      is.na(input$qa_num_birth_year)== FALSE & is.na(input$cens_date_death) == FALSE) | 
                     (is.na(input$qa_num_birth_year)== TRUE & is.na(input$cens_date_death) == FALSE)),]
  consort[nrow(consort)+1,] <- c("Quality assurance: Year of birth is after year of death or patient only has year of death",
                                 nrow(input))
  
  print('Quality assurance: Year of birth is before 1793 or year of birth exceeds current date')
  
  input <- input[!((input$qa_num_birth_year < 1793 | 
                      (input$qa_num_birth_year >format(Sys.Date(),"%Y"))) & 
                     is.na(input$qa_num_birth_year) == FALSE),]
  consort[nrow(consort)+1,] <- c("Quality assurance: Year of birth is before 1793 or year of birth exceeds current date",
                                 nrow(input))
  
  print('Quality assurance: Date of death is invalid (on or before 1/1/1900 or after current date)')
  
  input <- input[!((input$cens_date_death <= as.Date(study_dates$earliest_expec) | 
                      input$cens_date_death > format(Sys.Date(),"%Y-%m-%d")) & is.na(input$cens_date_death) == FALSE),]
  consort[nrow(consort)+1,] <- c("Quality assurance: Date of death is invalid (on or before 1/1/1900 or after current date)",
                                 nrow(input))
  
  print('Quality assurance: Pregnancy/birth codes for men')
  
  input <- input[!(input$qa_bin_pregnancy == TRUE & input$cov_cat_sex=="Male"),]
  consort[nrow(consort)+1,] <- c("Quality assurance: Pregnancy/birth codes for men",
                                 nrow(input))
  
  print('Quality assurance: HRT or COCP meds for men')
  
  input <- input[!(input$cov_cat_sex=="Male" & input$qa_bin_hrtcocp==TRUE),]
  consort[nrow(consort)+1,] <- c("Quality assurance: HRT or COCP meds for men",
                                 nrow(input))
  
  print('Quality assurance: Prostate cancer codes for women')
  
  input <- input[!(input$qa_bin_prostate_cancer == TRUE & 
                     input$cov_cat_sex=="Female"),]
  consort[nrow(consort)+1,] <- c("Quality assurance: Prostate cancer codes for women",
                                 nrow(input))
  
  # Save consort data
  print('Saving consort data after Quality Assurance')
  
  consort$N <- as.numeric(consort$N)
  consort$removed <- dplyr::lag(consort$N, default = dplyr::first(consort$N)) - consort$N
  
  write.csv(consort, file = paste0("output/consort_", cohort, "_qa.csv"), row.names = FALSE)
  
  # Perform redaction
  print('Performing redaction')
  
  consort$removed <- NULL
  consort$N_midpoint6 <- roundmid_any(consort$N, to=threshold)
  consort$removed_derived <- dplyr::lag(consort$N_midpoint6, default = dplyr::first(consort$N_midpoint6)) - consort$N_midpoint6
  consort$N <- NULL
  
  # Save rounded consort data
  print('Saving rounded consort data')
  
  write.csv(consort, file = paste0("output/consort_", cohort, "_qa_midpoint6.csv"), row.names = FALSE)
  
  # Save the dataset after Quality Assurance
  print('Saving dataset after Quality Assurance')
  
  input <- input[, c("patient_id", "cens_date_death", "index_date",
                     colnames(input)[grepl("end_date_", colnames(input))],
                     colnames(input)[grepl("sub_", colnames(input))],
                     colnames(input)[grepl("exp_", colnames(input))],
                     colnames(input)[grepl("out_", colnames(input))],
                     colnames(input)[grepl("cov_", colnames(input))],
                     colnames(input)[grepl("inex_", colnames(input))],
                     colnames(input)[grepl("cens_", colnames(input))],
                     colnames(input)[grepl("vax_date_", colnames(input))],
                     colnames(input)[grepl("vax_cat_", colnames(input))])]
  
  saveRDS(input, file = paste0("output/input_", cohort, "_qa.rds"), compress = TRUE)
  
  return(list(input = input, consort = consort))
}
