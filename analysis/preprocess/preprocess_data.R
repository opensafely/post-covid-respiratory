# Load libraries ---------------------------------------------------------------

library(magrittr)
library(dplyr)
library(tidyverse)
library(lubridate)
library(data.table)
library(readr)

# Specify command arguments ----------------------------------------------------

args <- commandArgs(trailingOnly=TRUE)
print(length(args))
if(length(args)==0){
  cohort_name <- "prevax_extf"
} else {
  cohort_name <- args[[1]]
}

# Get column names -------------------------------------------------------------

all_cols <- fread(paste0("output/input_",cohort_name,".csv.gz"), 
                  header = TRUE, sep = ",", nrows = 0, 
                  stringsAsFactors = FALSE) %>%
  names()

message("Column names found")

# Identify column classes ------------------------------------------------------

cat_cols <- c("patient_id", grep("_cat", all_cols, value = TRUE))

bin_cols <- c(grep("_bin", all_cols, value = TRUE), 
              grep("prostate_cancer_", all_cols, value = TRUE),
              "has_follow_up_previous_6months", "has_died", "registered_at_start",
              "tmp_cocp","tmp_hrt")

num_cols <- c(grep("_num", all_cols, value = TRUE),
              grep("vax_jcvi_age_", all_cols, value = TRUE))

date_cols <- grep("_date", all_cols, value = TRUE)

message("Column classes identified")

# Define column classes --------------------------------------------------------

col_classes <- setNames(
  c(rep("c", length(cat_cols)),
    rep("l", length(bin_cols)),
    rep("d", length(num_cols)),
    rep("D", length(date_cols))
  ), 
  all_cols[match(c(cat_cols, bin_cols, num_cols, date_cols), all_cols)]
)

message("Column classes defined")

# Read cohort dataset ---------------------------------------------------------- 

df <- read_csv(paste0("output/input_",cohort_name,".csv.gz"), 
               col_types = col_classes)

message(paste0("Dataset has been read successfully with N = ", nrow(df), " rows"))

# Add death_date and deregistration_date from prelim data ----------------------

prelim_data <- read_csv("output/index_dates.csv.gz")
prelim_data <- prelim_data[,c("patient_id","death_date","deregistration_date")]
prelim_data$patient_id <- as.character(prelim_data$patient_id)
prelim_data$death_date <- as.Date(prelim_data$death_date)
prelim_data$deregistration_date <- as.Date(prelim_data$deregistration_date)

df <- df %>% inner_join(prelim_data,by="patient_id")

message("Death and deregistration dates added!")

# Format columns ---------------------------------------------------------------

df <- df %>%
  mutate(across(c(contains("_date")),
                ~ floor_date(as.Date(., format="%Y-%m-%d"), unit = "days")),
         across(contains('_birth_year'),
                ~ format(as.Date(., origin = "1970-01-01"), "%Y")),
         across(contains('_num') & !contains('date'), ~ as.numeric(.)),
         across(contains('_cat'), ~ as.factor(.)),
         across(contains('_bin'), ~ as.logical(.)))

# Overwrite vaccination information for dummy data and vax cohort only ---------

if(Sys.getenv("OPENSAFELY_BACKEND") %in% c("", "expectations") &&
   cohort_name %in% c("vax")) {
  source("analysis/modify_dummy_vax_data.R")
  message("Vaccine information overwritten successfully")
}

# Describe data ----------------------------------------------------------------

sink(paste0("output/describe_",cohort_name,".txt"))
print(Hmisc::describe(df))
sink()
message ("Cohort ",cohort_name, " description written successfully!")

# QC for consultation variable and set max to 365 (i.e., one per day) ----------

df <- df %>%
  mutate(cov_num_consulation_rate = replace(cov_num_consulation_rate, 
                                            cov_num_consulation_rate > 365, 365))

# Define COVID-19 severity -----------------------------------------------------

df <- df %>%
  mutate(sub_cat_covid19_hospital = 
           ifelse(!is.na(exp_date_covid19_confirmed) &
                    !is.na(sub_date_covid19_hospital) &
                    sub_date_covid19_hospital - exp_date_covid19_confirmed >= 0 &
                    sub_date_covid19_hospital - exp_date_covid19_confirmed < 29, "hospitalised",
                  ifelse(!is.na(exp_date_covid19_confirmed), "non_hospitalised", 
                         ifelse(is.na(exp_date_covid19_confirmed), "no_infection", NA)))) %>%
  mutate(across(sub_cat_covid19_hospital, factor))

df <- df[!is.na(df$patient_id),]
df[,c("sub_date_covid19_hospital")] <- NULL

message("COVID19 severity determined successfully")

# Restrict columns and save analysis dataset -----------------------------------

df1 <- df %>% 
  select(patient_id,
         death_date,
         starts_with("index_date_"),
         has_follow_up_previous_6months,
         deregistration_date,
         starts_with("end_date_"),
         contains("sub_"), # Subgroups
         contains("exp_"), # Exposures
         contains("out_"), # Outcomes
         contains("cov_"), # Covariates
         contains("qa_"), # Quality assurance
         contains("step"), # diabetes steps
         contains("vax_date_eligible"), # Vaccination eligibility
         contains("vax_date_"), # Vaccination dates and vax type 
         contains("vax_cat_") # Vaccination products
  )

df1[,colnames(df)[grepl("tmp_",colnames(df))]] <- NULL

# Save input -------------------------------------------------------------------

saveRDS(df1, file = paste0("output/input_",cohort_name,".rds"), compress = TRUE)
message(paste0("Input data saved successfully with N = ", nrow(df1), " rows"))

# Describe data ----------------------------------------------------------------

sink(paste0("output/describe_input_",cohort_name,"_stage0.txt"))
print(Hmisc::describe(df1))
sink()

# Restrict columns and save Venn diagram input dataset -------------------------

df2 <- df %>% select(starts_with(c("patient_id","tmp_out_date","out_date")))

# Describe data ----------------------------------------------------------------

sink(paste0("output/describe_venn_",cohort_name,".txt"))
print(Hmisc::describe(df2))
sink()

saveRDS(df2, file = paste0("output/venn_",cohort_name,".rds"), compress = TRUE)

message("Venn diagram data saved successfully")
tictoc::toc()