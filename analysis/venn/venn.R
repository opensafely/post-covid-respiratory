# Load libraries ---------------------------------------------------------------
print('Load libraries')

library(data.table)
library(readr)
library(dplyr)

# Specify redaction threshold --------------------------------------------------

threshold <- 6

# Source common functions ------------------------------------------------------
print('Source common functions')

source("analysis/utility.R")

# Specify arguments ------------------------------------------------------------
print('Specify arguments')

args <- commandArgs(trailingOnly=TRUE)

if(length(args)==0){
  cohort <- "prevax"
  analyses <- "main_preex_FALSE"
} else {
  cohort <- args[[1]]
  analyses <- args[[2]]
}

# Identify outcomes ------------------------------------------------------------
print('Identify outcomes')

active_analyses <- readr::read_rds("lib/active_analyses.rds")

outcomes <- gsub("out_date_","",
                 unique(active_analyses[active_analyses$cohort==cohort &
                                          active_analyses$analysis==analyses,]$outcome))

# Load Venn data ---------------------------------------------------------------
print('Load Venn data')

venn <- readr::read_rds(paste0("output/dataset_clean/venn-cohort_",cohort,".rds"))



# Create empty output table ----------------------------------------------------
print('Create empty output table')

df <- data.frame(outcome = character(),
                 only_gp = numeric(),
                 only_apc = numeric(),
                 only_death = numeric(),
                 gp_apc = numeric(),
                 gp_death = numeric(),
                 apc_death = numeric(),
                 gp_apc_death = numeric(),
                 total_gp = numeric(),
                 total_apc = numeric(),
                 total_death = numeric(),
                 total = numeric(),
                 stringsAsFactors = FALSE)

# Populate Venn table for each outcome -----------------------------------------
print('Populate Venn table for each outcome')

for (outcome in outcomes) {
    
    print(paste0("Outcome: ", outcome))
    
    # Load model input data ------------------------------------------------------
    print('Load model input data')
    
    model_input <- readr::read_rds(paste0("output/model/model_input-cohort_",cohort,"-",analyses,"-",outcome,".rds"))  
    model_input <- model_input[!is.na(model_input$out_date),c("patient_id","out_date")]
    
    # Filter Venn data based on model input --------------------------------------
    print('Filter Venn data based on model input')
    
    tmp <- venn[venn$patient_id %in% model_input$patient_id,
                c("patient_id",colnames(venn)[grepl(outcome,colnames(venn))])]
    
    colnames(tmp) <- gsub(paste0("tmp_out_date_",outcome,"_"),"",colnames(tmp))
    
    # Identify and add missing columns -------------------------------------------
    print('Identify and add missing columns')
    
    complete <- data.frame(patient_id = tmp$patient_id,
                           gp = as.Date(NA),
                           apc = as.Date(NA),
                           death = as.Date(NA))
    
    complete[,setdiff(colnames(tmp),"patient_id")] <- NULL
    notused <- NULL
    
    if (ncol(complete)>1) {
      tmp <- merge(tmp, complete, by = c("patient_id"))
      notused <- setdiff(colnames(complete),"patient_id")
    }
    
    # Calculate the number contributing to each source combination ---------------
    print('Calculate the number contributing to each source combination')
    
    tmp$gp_contributing <- !is.na(tmp$gp) & 
      is.na(tmp$apc) & 
      is.na(tmp$death)
    
    tmp$apc_contributing <- is.na(tmp$gp) & 
      !is.na(tmp$apc) & 
      is.na(tmp$death)
    
    tmp$death_contributing <- is.na(tmp$gp) & 
      is.na(tmp$apc) & 
      !is.na(tmp$death)
    
    tmp$gp_apc_contributing <- !is.na(tmp$gp) & 
      !is.na(tmp$apc) & 
      is.na(tmp$death)
    
    tmp$apc_death_contributing <- is.na(tmp$gp) & 
      !is.na(tmp$apc) & 
      !is.na(tmp$death)
    
    tmp$gp_death_contributing <- !is.na(tmp$gp) & 
      is.na(tmp$apc) & 
      !is.na(tmp$death)
    
    tmp$gp_apc_death_contributing <- !is.na(tmp$gp) & 
      !is.na(tmp$apc) & 
      !is.na(tmp$death)
    
    # Record the number contributing to each source combination ------------------
    print('Record the number contributing to each source combination')
    
    df[nrow(df)+1,] <- c(outcome,
                         only_gp = nrow(tmp %>% filter(gp_contributing==T)),
                         only_apc = nrow(tmp %>% filter(apc_contributing==T)),
                         only_death = nrow(tmp %>% filter(death_contributing==T)),
                         gp_apc = nrow(tmp %>% filter(gp_apc_contributing==T)),
                         gp_death = nrow(tmp %>% filter(gp_death_contributing==T)),
                         apc_death = nrow(tmp %>% filter(apc_death_contributing==T)),
                         gp_apc_death = nrow(tmp %>% filter(gp_apc_death_contributing==T)),
                         total_gp = nrow(tmp %>% filter(!is.na(gp))),
                         total_apc = nrow(tmp %>% filter(!is.na(apc))),
                         total_death = nrow(tmp %>% filter(!is.na(death))),
                         total = nrow(tmp))
    
    # Replace source combinations with NA if not in study definition -------------
    print('Replace source combinations with NA if not in study definition')
    
    source_combos <- c("only_gp","only_apc","only_death","gp_apc","gp_death","apc_death","gp_apc_death","total_gp","total_apc","total_death")
    source_consid <- source_combos
    
    if (!is.null(notused)) {
      for (i in notused) {
        
        # Add variables to consider for Venn plot to vector
        source_consid <- source_combos[!grepl(i,source_combos)]
        
        # Replace unused sources with NA in summary table
        for (j in setdiff(source_combos,source_consid)) {
          df[df$outcome==outcome,j] <- NA
        }
        
      }
    }
    
  }

# Record cohort ----------------------------------------------------------------
print('Record cohort and pre-ex group')

df$cohort <- cohort
df$analyses <- analyses

# Save Venn data -----------------------------------------------------------------
print('Save Venn data')

write.csv(df, paste0("output/venn/venn_",cohort,"-",analyses,".csv"))

# Perform redaction ------------------------------------------------------------
print('Perform redaction')

df[,setdiff(colnames(df),c("outcome"))] <- lapply(df[,setdiff(colnames(df),c("outcome"))],
                                                  FUN=function(y){roundmid_any(as.numeric(y), to=threshold)})

# Save rounded Venn data -------------------------------------------------------
print('Save rounded Venn data')

write.csv(df, paste0("output/venn/venn_",cohort,"-",analyses,"_rounded.csv"))