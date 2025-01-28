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

# Specify redaction threshold --------------------------------------------------
print('Specify redaction threshold')

threshold <- 6

# Source common functions ------------------------------------------------------
print('Source common functions')

source("analysis/utility.R")
source("analysis/data_cleaning/fn-qa.R")
source("analysis/data_cleaning/fn-inex.R")
source("analysis/data_cleaning/fn-ref.R")
# Specify command arguments ----------------------------------------------------
print('Specify command arguments')

args <- commandArgs(trailingOnly=TRUE)

if(length(args)==0){
  cohort <- "prevax"
} else {
  cohort <- args[[1]]
}

# Load json file containing vax study dates ------------------------------------
print('Load json file containing vax study dates')

study_dates <- fromJSON("output/study_dates.json")

# Specify relevant dates -------------------------------------------------------
print('Specify relevant dates')

vax_start_date <- as.Date(study_dates$vax1_earliest, format="%Y-%m-%d")
mixed_vax_threshold <- as.Date("2021-05-07")
start_date_delta <- as.Date(study_dates$delta_date, format="%Y-%m-%d")

# Load cohort data -------------------------------------------------------------
print('Load cohort data')

input <- read_rds(file.path("output", paste0("input_",cohort,".rds")))
print(paste0(cohort,  " cohort: ", nrow(input), " rows in the input file"))

# Set reference levels for factors----------------------------------------------

print('Call reference function')

input <- ref(input)$input

# Quality assurance ------------------------------------------------------------

print('Call quality assurance function')
qa_results <- qa(input, study_dates)
input <- qa_results$input
consort <- qa_results$consort

# Inclusion criteria -----------------------------------------------------------

print('Call inclusion criteria function')
input <- inex(input, consort, cohort, vax_start_date, mixed_vax_threshold, start_date_delta)$input
consort <- inex(input, consort, cohort, vax_start_date, mixed_vax_threshold, start_date_delta)$consort

# Save consort data after Inclusion criteria
print('Saving consort data after Inclusion criteria')

consort$N <- as.numeric(consort$N)
consort$removed <- dplyr::lag(consort$N, default = dplyr::first(consort$N)) - consort$N

write.csv(consort, file = paste0("output/consort_", cohort, ".csv"), row.names = FALSE)

# Perform redaction
print('Performing redaction')

consort$removed <- NULL
consort$N_midpoint6 <- roundmid_any(consort$N, to=threshold)
consort$removed_derived <- dplyr::lag(consort$N_midpoint6, default = dplyr::first(consort$N_midpoint6)) - consort$N_midpoint6
consort$N <- NULL

# Save rounded consort data
print('Saving rounded consort data after Inclusion criteria')

write.csv(consort, file = paste0("output/consort_", cohort, "_midpoint6.csv"), row.names = FALSE)

# Save the dataset 
print('Saving dataset after Inclusion criteria')

input <- input[, c("patient_id", "index_date",
                  colnames(input)[grepl("end_date_", colnames(input))],
                  colnames(input)[grepl("sub_", colnames(input))],
                  colnames(input)[grepl("exp_", colnames(input))],
                  colnames(input)[grepl("out_", colnames(input))],
                  colnames(input)[grepl("cov_", colnames(input))],
                  colnames(input)[grepl("cens_", colnames(input))],
                  colnames(input)[grepl("vax_date_", colnames(input))],
                  colnames(input)[grepl("vax_cat_", colnames(input))])]

saveRDS(input, file = paste0("output/input_", cohort, ".rds"), compress = TRUE)