# Load libraries --------------------------------------------------------------
print('Load libraries')

library(magrittr)
library(dplyr)
library(tidyverse)
library(lubridate)
library(data.table)
library(readr)
library(tidyr)
library(stringr)
library(ggplot2)
library(jsonlite)
library(here)
library(arrow)

# Define clean dataset output folder -------------------------------------------
print("Creating output/dataset_clean output folder")

dataclean_dir <- "output/dataset_clean/"
fs::dir_create(here::here(dataclean_dir))

# Specify redaction threshold --------------------------------------------------
print('Specify redaction threshold')

threshold <- 6

# Load json file containing vax study dates ------------------------------------
print('Load json file containing vax study dates')

study_dates <- fromJSON("output/study_dates.json")

# Specify relevant dates -------------------------------------------------------
print('Specify relevant dates')

vax_start_date <- as.Date(study_dates$vax1_earliest, format="%Y-%m-%d")
mixed_vax_threshold <- as.Date(study_dates$mixed_vax_threshold, format="%Y-%m-%d")
start_date_delta <- as.Date(study_dates$delta_date, format="%Y-%m-%d")

# Source common functions ------------------------------------------------------
print('Source common functions')

source("analysis/utility.R")
source("analysis/dataset_clean/fn-preproc.R")
source("analysis/dataset_clean/fn-ref.R")
source("analysis/dataset_clean/fn-inex.R")
source("analysis/dataset_clean/fn-qa.R")

# Specify command arguments ----------------------------------------------------
print('Specify command arguments')

args <- commandArgs(trailingOnly=TRUE)
print(length(args))
if(length(args)==0){
  cohort <- "prevax"
} else {
  cohort <- args[[1]]
}

if (length(args) < 2) { # Whether to create describe*.txt files
  describe_flag <- "describe_print" #describe_print or no_describe_print
} else {
  describe_flag <- args[[2]]
}

# Define describe (desc_*.txt) output folder ----------------------------------

if (describe_flag == "describe_print") {
  print("Creating output/describe directory")
  describe_dir <- "output/describe/"
  fs::dir_create(here::here(describe_dir))
}

# Describe preprocessing data -------------------------------------------------

  ## Describe raw data
input_raw <- preproc1(cohort)$input_raw

  ### Overwrite vaccination information for dummy data and vax cohort only ---------
  
if(Sys.getenv("OPENSAFELY_BACKEND") %in% c("", "expectations") &&
cohort %in% c("vax")) {
source("analysis/dataset_clean/modify_dummy_data.R")
message("Vaccine information overwritten successfully")
}

if (describe_flag == "describe_print") {
    sink(paste0(describe_dir, "desc_raw_", cohort, ".txt"))
    print(Hmisc::describe(input_raw))
    sink()
    message ("Cohort ", cohort, " description written successfully!")
} else {
    message("No description written, change input flag if description is desired.")
}

  ## Describe Venn diagram data
input_venn <- preproc2(cohort, input_raw)$input_venn

if (describe_flag == "describe_print") {
    sink(paste0(describe_dir, "desc_venn_", cohort, ".txt"))
    print(Hmisc::describe(input_venn))
    sink()
    message("Venn diagram data saved successfully")
} else {
    message("No description written, change input flag if description is desired.")
}
  ## Save Venn diagram data
saveRDS(input_venn, file = paste0(dataclean_dir, "venn_", cohort, ".rds"), compress = TRUE)
message("Venn diagram data saved successfully")

  ## Describe preprocess data
input <- preproc2(cohort, input_raw)$input_preproc
message(paste0("Preprocess dataset has been read successfully with N = ", nrow(input), " rows"))

if (describe_flag == "describe_print") {
    sink(paste0(describe_dir, "desc_preproc_", cohort, ".txt"))
    print(Hmisc::describe(input))
    sink()
    message (paste0("Cohort ", cohort, " with valid patient IDs description written successfully!"))
} else {
    message ("No description written, change input flag if description is desired.")
}

# Set reference levels for factors----------------------------------------------
print('Call reference function')

input <- ref(input)$input

# Inclusion criteria -----------------------------------------------------------
print('Call inclusion criteria function')

inex_results <- inex(input, cohort, vax_start_date, mixed_vax_threshold, start_date_delta)
input <- inex_results$input
consort <- inex_results$consort

# Quality assurance ------------------------------------------------------------
print('Call quality assurance function')

qa_results <- qa(input, consort, study_dates)
input <- qa_results$input
consort <- qa_results$consort

# Save consort data after Inclusion criteria
print('Saving consort data after Inclusion criteria')

consort$N <- as.numeric(consort$N)
consort$removed <- dplyr::lag(consort$N, default = dplyr::first(consort$N)) - consort$N

write.csv(consort, file = paste0(dataclean_dir, "consort_", cohort, ".csv"), row.names = FALSE)

# Perform redaction
print('Performing redaction')

consort$removed <- NULL
consort$N_midpoint6 <- roundmid_any(consort$N, to=threshold)
consort$removed_derived <- dplyr::lag(consort$N_midpoint6, default = dplyr::first(consort$N_midpoint6)) - consort$N_midpoint6
consort$N <- NULL

# Save rounded consort data
print('Saving rounded consort data after Inclusion criteria')

write.csv(consort, file = paste0(dataclean_dir, "consort_", cohort, "_midpoint6.csv"), row.names = FALSE)

# Save the dataset 
print('Saving dataset after Inclusion criteria')

input <- input[, c("patient_id", "index_date",
                  colnames(input)[grepl("end_date_", colnames(input))],
                  colnames(input)[grepl("sub_", colnames(input))],
                  colnames(input)[grepl("exp_", colnames(input))],
                  colnames(input)[grepl("out_", colnames(input))],
                  colnames(input)[grepl("cov_", colnames(input))],
                  colnames(input)[grepl("cens_", colnames(input))],
                  colnames(input)[grepl("strat_", colnames(input))],
                  colnames(input)[grepl("vax_date_", colnames(input))],
                  colnames(input)[grepl("vax_cat_", colnames(input))])]

saveRDS(input, file = paste0(dataclean_dir, "input_", cohort, "_clean.rds"), compress = TRUE)