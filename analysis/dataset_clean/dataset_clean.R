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
source("analysis/dataset_clean/fn-ref.R")
source("analysis/dataset_clean/fn-qa.R")
source("analysis/dataset_clean/fn-inex.R")

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
  describe_flag <- "no_describe_print" #describe_print or no_describe_print
} else {
  describe_flag <- args[[2]]
}

# Define describe (desc_*.txt) output folder ----------------------------------

if (describe_flag == "describe_print") {
  print("Creating output/describe directory")
  describe_dir <- "output/describe/"
  fs::dir_create(here::here(describe_dir))
}

# Get column names -------------------------------------------------------------
print('Get column names')

all_cols <- fread(paste0("output/dataset_definition/input_",cohort,".csv.gz"), 
                  header = TRUE, sep = ",", nrows = 0, 
                  stringsAsFactors = FALSE) %>%
  names()
message("Column names found")
print(all_cols)

# Define column classes ------------------------------------------------------
print('Define colum classes')

cat_cols <- c("patient_id", grep("_cat", all_cols, value = TRUE))
bin_cols <- c(grep("_bin", all_cols, value = TRUE))
num_cols <- c(grep("_num", all_cols, value = TRUE),
              grep("vax_jcvi_age_", all_cols, value = TRUE))
date_cols <- grep("_date", all_cols, value = TRUE)
message("Column classes identified")

col_classes <- setNames(
  c(rep("c", length(cat_cols)),
    rep("l", length(bin_cols)),
    rep("d", length(num_cols)),
    rep("D", length(date_cols))
  ), 
  all_cols[match(c(cat_cols, bin_cols, num_cols, date_cols), all_cols)]
)
message("Column classes defined")

# Load cohort dataset ---------------------------------------------------------- 
print('Load cohort dataset')

df <- read_csv(paste0("output/dataset_definition/input_",cohort,".csv.gz"), 
               col_types = col_classes)
message(paste0("Dataset has been read successfully with N = ", nrow(df), " rows"))

# Format dataset columns ---------------------------------------------------------------
print('Format dataset columns')

df <- df %>%
  mutate(across(all_of(date_cols),
                ~ floor_date(as.Date(., format="%Y-%m-%d"), unit = "days")),
         across(contains('_birth_year'), 
                ~ format(as.Date(., origin = "1970-01-01"), "%Y")),
         across(all_of(num_cols), ~ as.numeric(.)), 
         across(all_of(cat_cols), ~ as.factor(.))) 
message("Dataset columns formatted")

# Overwrite vaccination information for dummy data and vax cohort only ---------

if(Sys.getenv("OPENSAFELY_BACKEND") %in% c("", "expectations") &&
   cohort %in% c("vax")) {
  source("analysis/dataset_clean/modify_dummy_data.R")
  message("Vaccine information overwritten successfully")
}

# Describe pre-processing data -------------------------------------------------

if (describe_flag == "describe_print") {
    sink(paste0(describe_dir, "desc_preproc_", cohort, ".txt"))
    print(Hmisc::describe(df))
    sink()
    message ("Cohort ", cohort, " description written successfully!")
} else {
    message("No description written, change input flag if description is desired.")
}

# Remove records with missing patient id ---------------------------------------

df <- df[!is.na(df$patient_id),]

message("All records with valid patient IDs retained.")

# Restrict columns and save Venn diagram input dataset -------------------------

df1 <- df %>% select(starts_with(c("patient_id","tmp_out_date","out_date")))

# Describe Venn diagram data ---------------------------------------------------

if (describe_flag == "describe_print") {
    sink(paste0(describe_dir, "desc_venn_", cohort, ".txt"))
    print(Hmisc::describe(df1))
    sink()
    message("Venn diagram data saved successfully")
} else {
    message("No description written, change input flag if description is desired.")
}

saveRDS(df1, file = paste0(dataclean_dir, "venn_", cohort, ".rds"), compress = TRUE)

message("Venn diagram data saved successfully")

# Restrict columns and save analysis dataset -----------------------------------

input <- df %>% 
  select(patient_id,
         starts_with("index_date"),
         starts_with("end_date_"),
         contains("sub_"),                   # Subgroups
         contains("exp_"),                   # Exposures
         contains("out_"),                   # Outcomes
         contains("cov_"),                   # Covariates
         contains("inex_"),                  # Inclusion/exclusion
         contains("cens_"),                  # Censor
         contains("qa_"),                    # Quality assurance
         contains("strat_"),                 # Strata
         contains("vax_date_eligible"),      # Vaccination eligibility
         contains("vax_date_"),              # Vaccination dates and vax type 
         contains("vax_cat_")                # Vaccination products
  )

input[,colnames(df)[grepl("tmp_",colnames(df))]] <- NULL

message(paste0("Analysis dataset has been read successfully with N = ", nrow(df), " rows"))

# Describe pre-cleaning data ------------------------------------------------------

if (describe_flag == "describe_print") {
    sink(paste0(describe_dir, "desc_input_preclean_", cohort, ".txt"))
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

saveRDS(input, file = paste0(dataclean_dir, "input_", cohort, ".rds"), compress = TRUE)