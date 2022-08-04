# Load packages ----------------------------------------------------------------

library(magrittr)
library(data.table)

# Make directory ---------------------------------------------------------------

fs::dir_create(here::here("output", "model_input"))

# Specify arguments ------------------------------------------------------------

args <- commandArgs(trailingOnly=TRUE)

if(length(args)==0){
  name <- "VAX_ATE_2TP"
}else{
  name <- args[[1]]
}

# Load active analyses ---------------------------------------------------------

active_analyses <- readr::read_rds("lib/active_analyses.rds")
active_analyses <- active_analyses[active_analyses$name==name,]
  
# Load data --------------------------------------------------------------------

input <- readr::read_rds(paste0("output/input_",active_analyses$cohort,"_stage1.rds"))

# Restrict to required variables -----------------------------------------------

input <- input[,unique(c("patient_id",
                         "index_date",
                         "death_date",
                         active_analyses$exposure, 
                         active_analyses$outcome,
                         unlist(strsplit(active_analyses$strata, split = ";")),
                         unlist(strsplit(active_analyses$covariate_other, split = ";")),
                         "sub_cat_covid19_hospital",
                         "sub_bin_covid19_confirmed_history",
                         "cov_cat_sex",
                         "cov_num_age",
                         "cov_cat_ethnicity",
                         active_analyses$priorhistory_var,
                         "cov_cat_smoking_status"))]

# Make model input: main -------------------------------------------------------

df <- input[input$sub_bin_covid19_confirmed_history==FALSE,]

saveRDS(df, file = paste0("output/model_input/",active_analyses$name,"-main.rds"))
print(paste0("Saved: output/model_input/",active_analyses$name,"-main.rds"))
rm(df)

# Make model input: sub_covid_hospitalised -------------------------------------

## TO BE COMPLETED ONCE CESORING OF HOSPITALISED CASES AGREED

# Make model input: sub_covid_nonhospitalised ----------------------------------

## TO BE COMPLETED ONCE CESORING OF HOSPITALISED CASES AGREED

# Make model input: sub_covid_history ------------------------------------------

if (active_analyses$sub_covid_history==TRUE) {
  
  df <- input[input$sub_bin_covid19_confirmed_history==TRUE,]
  
  df[,c("sub_bin_covid19_confirmed_history")] <- NULL
  saveRDS(df, file = paste0("output/model_input/",active_analyses$name,"-sub_covid_history.rds"))
  print(paste0("Saved: output/model_input/",active_analyses$name,"-sub_covid_history.rds"))
  rm(df)
  
}

# Make model input: sub_sex_female ---------------------------------------------

if (active_analyses$sub_sex_female==TRUE) {
  
  df <- input[input$sub_bin_covid19_confirmed_history==TRUE & 
                input$cov_cat_sex=="Female",]
  
  df[,c("sub_bin_covid19_confirmed_history","cov_cat_sex")] <- NULL
  saveRDS(df, file = paste0("output/model_input/",active_analyses$name,"-sub_sex_female.rds"))
  print(paste0("Saved: output/model_input/",active_analyses$name,"-sub_sex_female.rds"))
  rm(df)
  
}

# Make model input: sub_sex_male -----------------------------------------------

if (active_analyses$sub_sex_male==TRUE) {
  
  df <- input[input$sub_bin_covid19_confirmed_history==TRUE & 
                input$cov_cat_sex=="Male",]
  
  df[,c("sub_bin_covid19_confirmed_history","cov_cat_sex")] <- NULL
  saveRDS(df, file = paste0("output/model_input/",active_analyses$name,"-sub_sex_male.rds"))
  print(paste0("Saved: output/model_input/",active_analyses$name,"-sub_sex_male.rds"))
  rm(df)
  
}

# Make model input: sub_age_18_39 ----------------------------------------------

if (active_analyses$sub_age_18_39==TRUE) {
  
  df <- input[input$sub_bin_covid19_confirmed_history==TRUE & 
                input$cov_num_age>=18 &
                input$cov_num_age<40,]
  
  df[,c("sub_bin_covid19_confirmed_history")] <- NULL
  saveRDS(df, file = paste0("output/model_input/",active_analyses$name,"-sub_age_18_39.rds"))
  print(paste0("Saved: output/model_input/",active_analyses$name,"-sub_age_18_39.rds"))
  rm(df)
  
}

# Make model input: sub_age_40_59 ----------------------------------------------

if (active_analyses$sub_age_40_59==TRUE) {
  
  df <- input[input$sub_bin_covid19_confirmed_history==TRUE & 
                input$cov_num_age>=40 &
                input$cov_num_age<60,]
  
  df[,c("sub_bin_covid19_confirmed_history")] <- NULL
  saveRDS(df, file = paste0("output/model_input/",active_analyses$name,"-sub_age_40_59.rds"))
  print(paste0("Saved: output/model_input/",active_analyses$name,"-sub_age_40_59.rds"))
  rm(df)
  
}
# Make model input: sub_age_60_79 ----------------------------------------------

if (active_analyses$sub_age_60_79==TRUE) {
  
  df <- input[input$sub_bin_covid19_confirmed_history==TRUE & 
                input$cov_num_age>=60 &
                input$cov_num_age<80,]
  
  df[,c("sub_bin_covid19_confirmed_history")] <- NULL
  saveRDS(df, file = paste0("output/model_input/",active_analyses$name,"-sub_age_60_79.rds"))
  print(paste0("Saved: output/model_input/",active_analyses$name,"-sub_age_60_79.rds"))
  rm(df)
  
}

# Make model input: sub_age_80_110 ---------------------------------------------

if (active_analyses$sub_age_80_110==TRUE) {
  
  df <- input[input$sub_bin_covid19_confirmed_history==TRUE & 
                input$cov_num_age>=80 &
                input$cov_num_age<111,]
  
  df[,c("sub_bin_covid19_confirmed_history")] <- NULL
  saveRDS(df, file = paste0("output/model_input/",active_analyses$name,"-sub_age_80_110.rds"))
  print(paste0("Saved: output/model_input/",active_analyses$name,"-sub_age_80_110.rds"))
  rm(df)
  
}

# Make model input: sub_ethnicity_white ----------------------------------------

if (active_analyses$sub_ethnicity_white==TRUE) {
  
  df <- input[input$sub_bin_covid19_confirmed_history==TRUE & 
                input$cov_cat_ethnicity=="White",]
  
  df[,c("sub_bin_covid19_confirmed_history","cov_cat_ethnicity")] <- NULL
  saveRDS(df, file = paste0("output/model_input/",active_analyses$name,"-sub_ethnicity_white.rds"))
  print(paste0("Saved: output/model_input/",active_analyses$name,"-sub_ethnicity_white.rds"))
  rm(df)
  
}

# Make model input: sub_ethnicity_black ----------------------------------------

if (active_analyses$sub_ethnicity_black==TRUE) {
  
  df <- input[input$sub_bin_covid19_confirmed_history==TRUE & 
                input$cov_cat_ethnicity=="Black",]
  
  df[,c("sub_bin_covid19_confirmed_history","cov_cat_ethnicity")] <- NULL
  saveRDS(df, file = paste0("output/model_input/",active_analyses$name,"-sub_ethnicity_black.rds"))
  print(paste0("Saved: output/model_input/",active_analyses$name,"-sub_ethnicity_black.rds"))
  rm(df)
  
}

# Make model input: sub_ethnicity_mixed ----------------------------------------

if (active_analyses$sub_ethnicity_mixed==TRUE) {
  
  df <- input[input$sub_bin_covid19_confirmed_history==TRUE & 
                input$cov_cat_ethnicity=="Mixed",]
  
  df[,c("sub_bin_covid19_confirmed_history","cov_cat_ethnicity")] <- NULL
  saveRDS(df, file = paste0("output/model_input/",active_analyses$name,"-sub_ethnicity_mixed.rds"))
  print(paste0("Saved: output/model_input/",active_analyses$name,"-sub_ethnicity_mixed.rds"))
  rm(df)
  
}

# Make model input: sub_ethnicity_asian ----------------------------------------

if (active_analyses$sub_ethnicity_asian==TRUE) {
  
  df <- input[input$sub_bin_covid19_confirmed_history==TRUE & 
                input$cov_cat_ethnicity=="South Asian",]
  
  df[,c("sub_bin_covid19_confirmed_history","cov_cat_ethnicity")] <- NULL
  saveRDS(df, file = paste0("output/model_input/",active_analyses$name,"-sub_ethnicity_asian.rds"))
  print(paste0("Saved: output/model_input/",active_analyses$name,"-sub_ethnicity_asian.rds"))
  rm(df)
  
}

# Make model input: sub_ethnicity_other ----------------------------------------

if (active_analyses$sub_ethnicity_other==TRUE) {
  
  df <- input[input$sub_bin_covid19_confirmed_history==TRUE & 
                input$cov_cat_ethnicity=="Other",]
  
  df[,c("sub_bin_covid19_confirmed_history","cov_cat_ethnicity")] <- NULL
  saveRDS(df, file = paste0("output/model_input/",active_analyses$name,"-sub_ethnicity_other.rds"))
  print(paste0("Saved: output/model_input/",active_analyses$name,"-sub_ethnicity_other.rds"))
  rm(df)
  
}

# Make model input: sub_priorhistory_true --------------------------------------

if (active_analyses$priorhistory_var!="" & active_analyses$sub_priorhistory_true==TRUE) {
  
  df <- input[input$sub_bin_covid19_confirmed_history==TRUE,]
  df <- dplyr::rename(df, "priorhistory" = active_analyses$priorhistory_var)
  df <- df[df$priorhistory==TRUE,]
  
  df[,c("sub_bin_covid19_confirmed_history","priorhistory")] <- NULL
  saveRDS(df, file = paste0("output/model_input/",active_analyses$name,"-sub_priorhistory_true.rds"))
  print(paste0("Saved: output/model_input/",active_analyses$name,"-sub_priorhistory_true.rds"))
  rm(df)
  
}

# Make model input: sub_priorhistory_false -------------------------------------

if (active_analyses$priorhistory_var!="" & active_analyses$sub_priorhistory_false==TRUE) {
  
  df <- input[input$sub_bin_covid19_confirmed_history==TRUE,]
  df <- dplyr::rename(df, "priorhistory" = active_analyses$priorhistory_var)
  df <- df[df$priorhistory==FALSE,]
  
  df[,c("sub_bin_covid19_confirmed_history","priorhistory")] <- NULL
  saveRDS(df, file = paste0("output/model_input/",active_analyses$name,"-sub_priorhistory_false.rds"))
  print(paste0("Saved: output/model_input/",active_analyses$name,"-sub_priorhistory_false.rds"))
  rm(df)
  
}

# Make model input: sub_smokingstatus_ever -------------------------------------

if (active_analyses$sub_smokingstatus_ever==TRUE) {
  
  df <- input[input$sub_bin_covid19_confirmed_history==TRUE & 
                input$cov_cat_smoking_status=="Ever smoker",]
  
  df[,c("sub_bin_covid19_confirmed_history","cov_cat_smoking_status")] <- NULL
  saveRDS(df, file = paste0("output/model_input/",active_analyses$name,"-sub_smokingstatus_ever.rds"))
  print(paste0("Saved: output/model_input/",active_analyses$name,"-sub_smokingstatus_ever.rds"))
  rm(df)
  
}

# Make model input: sub_smokingstatus_never ------------------------------------

if (active_analyses$sub_smokingstatus_never==TRUE) {
  
  df <- input[input$sub_bin_covid19_confirmed_history==TRUE & 
                input$cov_cat_smoking_status=="Never smoker",]
  
  df[,c("sub_bin_covid19_confirmed_history","cov_cat_smoking_status")] <- NULL
  saveRDS(df, file = paste0("output/model_input/",active_analyses$name,"-sub_smokingstatus_never.rds"))
  print(paste0("Saved: output/model_input/",active_analyses$name,"-sub_smokingstatus_never.rds"))
  rm(df)
  
}

# Make model input: sub_smokingstatus_current ----------------------------------

if (active_analyses$sub_smokingstatus_current==TRUE) {
  
  df <- input[input$sub_bin_covid19_confirmed_history==TRUE & 
                input$cov_cat_smoking_status=="Current smoker",]
  
  df[,c("sub_bin_covid19_confirmed_history","cov_cat_smoking_status")] <- NULL
  saveRDS(df, file = paste0("output/model_input/",active_analyses$name,"-sub_smokingstatus_current.rds"))
  print(paste0("Saved: output/model_input/",active_analyses$name,"-sub_smokingstatus_current.rds"))
  rm(df)
  
}

