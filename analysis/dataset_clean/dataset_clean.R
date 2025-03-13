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

vax1_earliest <- as.Date(study_dates$vax1_earliest, format="%Y-%m-%d")
mixed_vax_threshold <- as.Date(study_dates$mixed_vax_threshold, format="%Y-%m-%d")
delta_date <- as.Date(study_dates$delta_date, format="%Y-%m-%d")

# Source common functions ------------------------------------------------------
print('Source common functions')

source("analysis/utility.R")
lapply(list.files("analysis/dataset_clean", full.names = TRUE, pattern = "fn-"), source)

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
  describe_flag <- "describe_TRUE" #describe_TRUE or describe_FALSE
} else {
  describe_flag <- args[[2]]
}

# Define describe (desc_*.txt) output folder ----------------------------------

if (describe_flag == "describe_TRUE") {
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

if (describe_flag == "describe_TRUE") {
    sink(paste0(describe_dir, "desc_raw_", cohort, ".txt"))
    print(Hmisc::describe(input_raw))
    sink()
    message ("Cohort ", cohort, " description written successfully!")
} else {
    message("No description written, change input flag if description is desired.")
}

  ## Read and save Venn diagram data
  
input_venn <- preproc2(cohort, input_raw, describe_flag, describe_dir)$input_venn
saveRDS(input_venn, file = paste0(dataclean_dir, "venn_", cohort, ".rds"), compress = TRUE)
message("Venn diagram data saved successfully")

# Read preprocess data --------------------------------------------------------

input <- preproc2(cohort, input_raw, describe_flag, describe_dir)$input_preproc
message(paste0("Preprocess dataset has been read successfully with N = ", nrow(input), " rows"))

# Specify flow table ----------------------------------------------------------
print('Specify flow table')

flow <- data.frame(Description = "Input", 
                      N = nrow(input),
                      stringsAsFactors = FALSE)
# Inclusion criteria -----------------------------------------------------------
print('Call inclusion criteria function')

inex_results <- inex(input, flow, cohort, vax1_earliest, mixed_vax_threshold, delta_date)
input <- inex_results$input
flow <- inex_results$flow

# Quality assurance ------------------------------------------------------------
print('Call quality assurance function')

qa_results <- qa(input, flow, study_dates)
input <- qa_results$input
flow <- qa_results$flow

# Set reference levels for factors----------------------------------------------
print('Call reference function')

input <- ref(input)$input

# Save flow data after Inclusion criteria
print('Saving flow data after Inclusion criteria')

flow$N <- as.numeric(flow$N)
flow$removed <- dplyr::lag(flow$N, default = dplyr::first(flow$N)) - flow$N

write.csv(flow, file = paste0(dataclean_dir, "flow_", cohort, ".csv"), row.names = FALSE)

# Perform redaction
print('Performing redaction')

flow$removed <- NULL
flow$N_midpoint6 <- roundmid_any(flow$N, to=threshold)
flow$removed_derived <- dplyr::lag(flow$N_midpoint6, default = dplyr::first(flow$N_midpoint6)) - flow$N_midpoint6
flow$N <- NULL

# Save rounded flow data
print('Saving rounded flow data after Inclusion criteria')

write.csv(flow, file = paste0(dataclean_dir, "flow_", cohort, "_midpoint6.csv"), row.names = FALSE)

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