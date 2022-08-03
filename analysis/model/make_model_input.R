# Load packages ----------------------------------------------------------------

library(magrittr)
library(data.table)

# Make directory ---------------------------------------------------------------

fs::dir_create(here::here("output", "model_input"))

# Specify arguments ------------------------------------------------------------

args <- commandArgs(trailingOnly=TRUE)

if(length(args)==0){
  
}else{
  arg <- args[[1]]
}

# Load active analyses ---------------------------------------------------------

active_analyses <- readr::read_rds("lib/active_analyses.rds")
i = 1

# Load data --------------------------------------------------------------------

input <- readr::read_rds(paste0("../post-covid-vaccinated/output/input_",active_analyses$cohort[i],"_stage1.rds")) # For testing purposes

# Restrict to required variables -----------------------------------------------

input <- input[,unique(c("patient_id",
                         "index_date",
                         "death_date",
                         active_analyses$exposure[i], 
                         active_analyses$outcome[i],
                         unlist(strsplit(active_analyses$strata[i], split = ";")),
                         unlist(strsplit(active_analyses$covariate_other[i], split = ";")),
                         "sub_cat_covid19_hospital",
                         "sub_bin_covid19_confirmed_history",
                         unlist(strsplit(active_analyses$subgroup_other[i], split = ";"))))]

# Make main analysis input -----------------------------------------------------

df <- input[input$sub_bin_covid19_confirmed_history==FALSE,]
saveRDS(df, file = paste0("output/model_input/",active_analyses$name[i],"-main.rds"))
rm(df)

# Make covid history subgroup --------------------------------------------------

if (active_analyses$subgroup_covid_history[i]==TRUE) {
  df <- input[input$sub_bin_covid19_confirmed_history==TRUE,]
  saveRDS(df, file = paste0("output/model_input/",active_analyses$name[i],"-covid_history.rds"))
  print(paste0("Saved: output/model_input/",active_analyses$name[i],"-covid_history.rds"))
  rm(df)
}

# Make covid hospitalised subgroups --------------------------------------------

## To be completed - requires special censoring criteria

# Make covid non-hospitalised subgroups ----------------------------------------

## To be completed - requires special censoring criteria

# Make all other subgroups -----------------------------------------------------

subgroups <- unlist(strsplit(active_analyses$subgroup_other, split = ";"))

for (x in subgroups) {
  values <- unique(as.character(input[,x]))
  for (y in values) {
    df <- input[input$sub_bin_covid19_confirmed_history==FALSE,]
    df <- dplyr::rename(df, "subgroup" = tidyselect::all_of(x))
    df <- df[df$subgroup==y,]
    y <- tolower(gsub(" ","_",y))
    saveRDS(df, file = paste0("output/model_input/",active_analyses$name[i],"-",x,"-",y,".rds"))
    print(paste0("Saved: output/model_input/",active_analyses$name[i],"-",x,"-",y,".rds"))
    rm(df)
  }
  
}
