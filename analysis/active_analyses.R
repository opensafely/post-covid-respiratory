# Create output directory ------------------------------------------------------

fs::dir_create(here::here("lib"))




# Create empty data frame ------------------------------------------------------

df <- data.frame(population = character(),
                 cohort = character(),
                 exposure = character(), 
                 outcome = character(), 
                 ipw = logical(), 
                 strata = character(),
                 covariate_sex = character(),
                 covariate_age = character(),
                 covariate_other = character(),
                 cox_start = character(),
                 cox_stop = character(),
                 study_start = character(),
                 study_stop = character(),
                 cut_points = character(),
                 controls_per_case = numeric(),
                 total_event_threshold = numeric(),
                 episode_event_threshold = numeric(),
                 covariate_threshold = numeric(),
                 age_spline = logical(),
                 analysis = character(),
                 stringsAsFactors = FALSE)

# Set constant values ----------------------------------------------------------

ipw <- TRUE
age_spline <- TRUE
exposure <- "exp_date_covid19_confirmed"
strata <- "cov_cat_region"
covariate_sex <- "cov_cat_sex"
covariate_age <- "cov_num_age"
cox_start <- "index_date"
cox_stop <- "end_date"
controls_per_case <- 20L
total_event_threshold <- 50L
episode_event_threshold <- 5L
covariate_threshold <- 5L
##Dates
#study_dates <- fromJSON("output/study_dates.json")

prevax_start <- "2020-01-01"
prevax_stop<- "2021-06-01"
vax_unvax_start<-"2021-06-01"
vax_unvax_stop <-"2021-12-14"
##Cut points 
prevax_cuts <- "28;197;535"
vax_unvax_cuts <- "28;197"
all_covars <- paste0("cov_cat_ethnicity;cov_cat_deprivation;cov_num_consultation_rate;cov_bin_healthcare_worker;",
                     "cov_cat_smoking_status;cov_bin_carehome_status;cov_bin_obesity;",
                     "cov_bin_history_pneumonia_snomed;cov_bin_history_asthma_snomed;cov_bin_history_pulmonary_fibrosis_snomed;",
                     "cov_bin_ami;cov_bin_all_stroke;cov_bin_dementia;cov_bin_liver_disease;",
                     "cov_bin_chronic_kidney_disease;cov_bin_cancer;cov_bin_hypertension;cov_bin_diabetes")

#Specific covars below are only confounders for the specific outcomes below
#specific_covars <- 
#specific_outcomes <- 



# Specify populations and cohorts --------------------------------------------------------------

populations <- c("no_pre-existing","pre-existing")

cohorts <- c("vax","unvax","prevax")

# Specify outcomes -------------------------------------------------------------

outcomes_runall <- c("out_date_breathless", 
                      "out_date_cough",
                      "out_date_urti",
                      "out_date_pneumonia",
                      "out_date_asthma_exac",
                      "out_date_copd_exac")
#outcomes_runmain <- 
                      


# Add active analyses ----------------------------------------------------------

for (p in populations) {

    for (c in cohorts) {
        
        for (i in outcomes_runall) { 
            
            ## analysis: main ----------------------------------------------------------
            df[nrow(df)+1,] <- c(population = p,
                                cohort = c,
                                exposure = exposure, 
                                outcome = i,
                                ipw = ipw, 
                                strata = strata,
                                covariate_sex = covariate_sex,
                                covariate_age = covariate_age,
                                covariate_other = all_covars,
                                cox_start = cox_start,
                                cox_stop = cox_stop,
                                study_start = ifelse(c=="prevax", prevax_start, vax_unvax_start),
                                study_stop = ifelse(c=="prevax", prevax_stop, vax_unvax_stop),
                                cut_points = ifelse(c=="prevax", prevax_cuts, vax_unvax_cuts),
                                controls_per_case = controls_per_case,
                                total_event_threshold = total_event_threshold,
                                episode_event_threshold = episode_event_threshold,
                                covariate_threshold = covariate_threshold,
                                age_spline = TRUE,
                                analysis = "main")

            ## analysis: sub_covid_hospitalised ----------------------------------------
            df[nrow(df)+1,] <- c(population = p,
                                cohort = c,
                                exposure = exposure, 
                                outcome = i,
                                ipw = ipw, 
                                strata = strata,
                                covariate_sex = covariate_sex,
                                covariate_age = covariate_age,
                                covariate_other = all_covars,
                                cox_start = cox_start,
                                cox_stop = cox_stop,
                                study_start = ifelse(c=="prevax", prevax_start, vax_unvax_start),
                                study_stop = ifelse(c=="prevax", prevax_stop, vax_unvax_stop),
                                cut_points = ifelse(c=="prevax", prevax_cuts, vax_unvax_cuts),
                                controls_per_case = controls_per_case,
                                total_event_threshold = total_event_threshold,
                                episode_event_threshold = episode_event_threshold,
                                covariate_threshold = covariate_threshold,
                                age_spline = TRUE,
                                analysis = "sub_covid_hospitalised")

            ## analysis: sub_covid_nonhospitalised ----------------------------------------
            df[nrow(df)+1,] <- c(population = p,
                                cohort = c,
                                exposure = exposure, 
                                outcome = i,
                                ipw = ipw, 
                                strata = strata,
                                covariate_sex = covariate_sex,
                                covariate_age = covariate_age,
                                covariate_other = all_covars,
                                cox_start = cox_start,
                                cox_stop = cox_stop,
                                study_start = ifelse(c=="prevax", prevax_start, vax_unvax_start),
                                study_stop = ifelse(c=="prevax", prevax_stop, vax_unvax_stop),
                                cut_points = ifelse(c=="prevax", prevax_cuts, vax_unvax_cuts),
                                controls_per_case = controls_per_case,
                                total_event_threshold = total_event_threshold,
                                episode_event_threshold = episode_event_threshold,
                                covariate_threshold = covariate_threshold,
                                age_spline = TRUE,
                                analysis = "sub_covid_nonhospitalised")    

            ## analysis: sub_covid_history ----------------------------------------
            if (c!="prevax") {

            df[nrow(df)+1,] <- c(population = p,
                                cohort = c,
                                exposure = exposure, 
                                outcome = i,
                                ipw = ipw, 
                                strata = strata,
                                covariate_sex = covariate_sex,
                                covariate_age = covariate_age,
                                covariate_other = all_covars,
                                cox_start = cox_start,
                                cox_stop = cox_stop,
                                study_start = vax_unvax_start,
                                study_stop = vax_unvax_stop,
                                cut_points = vax_unvax_cuts,
                                controls_per_case = controls_per_case,
                                total_event_threshold = total_event_threshold,
                                episode_event_threshold = episode_event_threshold,
                                covariate_threshold = covariate_threshold,
                                age_spline = TRUE,
                                analysis = "sub_covid_history")
            }

            ## analysis: sub_sex_female----------------------------------------------------
            df[nrow(df)+1,] <- c(population = p,
                                cohort = c,
                                exposure = exposure, 
                                outcome = i,
                                ipw = ipw, 
                                strata = strata,
                                covariate_sex = "NULL",
                                covariate_age = covariate_age,
                                covariate_other = all_covars,
                                cox_start = cox_start,
                                cox_stop = cox_stop,
                                study_start = ifelse(c=="prevax", prevax_start, vax_unvax_start),
                                study_stop = ifelse(c=="prevax", prevax_stop, vax_unvax_stop),
                                cut_points = ifelse(c=="prevax", prevax_cuts, vax_unvax_cuts),
                                controls_per_case = controls_per_case,
                                total_event_threshold = total_event_threshold,
                                episode_event_threshold = episode_event_threshold,
                                covariate_threshold = covariate_threshold,
                                age_spline = TRUE,
                                analysis = "sub_sex_female")

            ## analysis: sub_sex_male----------------------------------------------------
            df[nrow(df)+1,] <- c(population = p,
                                cohort = c,
                                exposure = exposure, 
                                outcome = i,
                                ipw = ipw, 
                                strata = strata,
                                covariate_sex = "NULL",
                                covariate_age = covariate_age,
                                covariate_other = all_covars,
                                cox_start = cox_start,
                                cox_stop = cox_stop,
                                study_start = ifelse(c=="prevax", prevax_start, vax_unvax_start),
                                study_stop = ifelse(c=="prevax", prevax_stop, vax_unvax_stop),
                                cut_points = ifelse(c=="prevax", prevax_cuts, vax_unvax_cuts),
                                controls_per_case = controls_per_case,
                                total_event_threshold = total_event_threshold,
                                episode_event_threshold = episode_event_threshold,
                                covariate_threshold = covariate_threshold,
                                age_spline = TRUE,
                                analysis = "sub_sex_male")

            ## analysis: sub_age_18_39----------------------------------------------------
            df[nrow(df)+1,] <- c(population = p,
                                cohort = c,
                                exposure = exposure, 
                                outcome = i,
                                ipw = ipw, 
                                strata = strata,
                                covariate_sex = covariate_sex,
                                covariate_age = covariate_age,
                                covariate_other = all_covars,
                                cox_start = cox_start,
                                cox_stop = cox_stop,
                                study_start = ifelse(c=="prevax", prevax_start, vax_unvax_start),
                                study_stop = ifelse(c=="prevax", prevax_stop, vax_unvax_stop),
                                cut_points = ifelse(c=="prevax", prevax_cuts, vax_unvax_cuts),
                                controls_per_case = controls_per_case,
                                total_event_threshold = total_event_threshold,
                                episode_event_threshold = episode_event_threshold,
                                covariate_threshold = covariate_threshold,
                                age_spline = FALSE,
                                analysis = "sub_age_18_39")

            ## analysis: sub_age_40_59----------------------------------------------------
            df[nrow(df)+1,] <- c(population = p,
                                cohort = c,
                                exposure = exposure, 
                                outcome = i,
                                ipw = ipw, 
                                strata = strata,
                                covariate_sex = covariate_sex,
                                covariate_age = covariate_age,
                                covariate_other = all_covars,
                                cox_start = cox_start,
                                cox_stop = cox_stop,
                                study_start = ifelse(c=="prevax", prevax_start, vax_unvax_start),
                                study_stop = ifelse(c=="prevax", prevax_stop, vax_unvax_stop),
                                cut_points = ifelse(c=="prevax", prevax_cuts, vax_unvax_cuts),
                                controls_per_case = controls_per_case,
                                total_event_threshold = total_event_threshold,
                                episode_event_threshold = episode_event_threshold,
                                covariate_threshold = covariate_threshold,
                                age_spline = FALSE,
                                analysis = "sub_age_40_59")

            ## analysis: sub_age_60_79----------------------------------------------------
            df[nrow(df)+1,] <- c(population = p,
                                cohort = c,
                                exposure = exposure, 
                                outcome = i,
                                ipw = ipw, 
                                strata = strata,
                                covariate_sex = covariate_sex,
                                covariate_age = covariate_age,
                                covariate_other = all_covars,
                                cox_start = cox_start,
                                cox_stop = cox_stop,
                                study_start = ifelse(c=="prevax", prevax_start, vax_unvax_start),
                                study_stop = ifelse(c=="prevax", prevax_stop, vax_unvax_stop),
                                cut_points = ifelse(c=="prevax", prevax_cuts, vax_unvax_cuts),
                                controls_per_case = controls_per_case,
                                total_event_threshold = total_event_threshold,
                                episode_event_threshold = episode_event_threshold,
                                covariate_threshold = covariate_threshold,
                                age_spline = FALSE,
                                analysis = "sub_age_60_79")

            ## analysis: sub_age_80_110----------------------------------------------------
            df[nrow(df)+1,] <- c(population = p,
                                cohort = c,
                                exposure = exposure, 
                                outcome = i,
                                ipw = ipw, 
                                strata = strata,
                                covariate_sex = covariate_sex,
                                covariate_age = covariate_age,
                                covariate_other = all_covars,
                                cox_start = cox_start,
                                cox_stop = cox_stop,
                                study_start = ifelse(c=="prevax", prevax_start, vax_unvax_start),
                                study_stop = ifelse(c=="prevax", prevax_stop, vax_unvax_stop),
                                cut_points = ifelse(c=="prevax", prevax_cuts, vax_unvax_cuts),
                                controls_per_case = controls_per_case,
                                total_event_threshold = total_event_threshold,
                                episode_event_threshold = episode_event_threshold,
                                covariate_threshold = covariate_threshold,
                                age_spline = FALSE,
                                analysis = "sub_age_80_110")
            
            ## analysis: sub_ethnicity_white -----------------------------------------------
            df[nrow(df)+1,] <- c(population = p,
                                cohort = c,
                                exposure = exposure, 
                                outcome = i,
                                ipw = ipw, 
                                strata = strata,
                                covariate_sex = covariate_sex,
                                covariate_age = covariate_age,
                                covariate_other = gsub("cov_cat_ethnicity;","",all_covars), #-ethnicity
                                cox_start = cox_start,
                                cox_stop = cox_stop,
                                study_start = ifelse(c=="prevax", prevax_start, vax_unvax_start),
                                study_stop = ifelse(c=="prevax", prevax_stop, vax_unvax_stop),
                                cut_points = ifelse(c=="prevax", prevax_cuts, vax_unvax_cuts),
                                controls_per_case = controls_per_case,
                                total_event_threshold = total_event_threshold,
                                episode_event_threshold = episode_event_threshold,
                                covariate_threshold = covariate_threshold,
                                age_spline = TRUE,
                                analysis = "sub_ethnicity_white")
            
            ## analysis: sub_ethnicity_black -----------------------------------------------
            df[nrow(df)+1,] <- c(population = p,
                                cohort = c,
                                exposure = exposure, 
                                outcome = i,
                                ipw = ipw, 
                                strata = strata,
                                covariate_sex = covariate_sex,
                                covariate_age = covariate_age,
                                covariate_other = gsub("cov_cat_ethnicity;","",all_covars), #-ethnicity
                                cox_start = cox_start,
                                cox_stop = cox_stop,
                                study_start = ifelse(c=="prevax", prevax_start, vax_unvax_start),
                                study_stop = ifelse(c=="prevax", prevax_stop, vax_unvax_stop),
                                cut_points = ifelse(c=="prevax", prevax_cuts, vax_unvax_cuts),
                                controls_per_case = controls_per_case,
                                total_event_threshold = total_event_threshold,
                                episode_event_threshold = episode_event_threshold,
                                covariate_threshold = covariate_threshold,
                                age_spline = TRUE,
                                analysis = "sub_ethnicity_black")
            
            ## analysis: sub_ethnicity_mixed -----------------------------------------------
            df[nrow(df)+1,] <- c(population = p,
                                cohort = c,
                                exposure = exposure, 
                                outcome = i,
                                ipw = ipw, 
                                strata = strata,
                                covariate_sex = covariate_sex,
                                covariate_age = covariate_age,
                                covariate_other = gsub("cov_cat_ethnicity;","",all_covars), #-ethnicity
                                cox_start = cox_start,
                                cox_stop = cox_stop,
                                study_start = ifelse(c=="prevax", prevax_start, vax_unvax_start),
                                study_stop = ifelse(c=="prevax", prevax_stop, vax_unvax_stop),
                                cut_points = ifelse(c=="prevax", prevax_cuts, vax_unvax_cuts),
                                controls_per_case = controls_per_case,
                                total_event_threshold = total_event_threshold,
                                episode_event_threshold = episode_event_threshold,
                                covariate_threshold = covariate_threshold,
                                age_spline = TRUE,
                                analysis = "sub_ethnicity_mixed")
            
            ## analysis: sub_ethnicity_asian -----------------------------------------------
            df[nrow(df)+1,] <- c(population = p,
                                cohort = c,
                                exposure = exposure, 
                                outcome = i,
                                ipw = ipw, 
                                strata = strata,
                                covariate_sex = covariate_sex,
                                covariate_age = covariate_age,
                                covariate_other = gsub("cov_cat_ethnicity;","",all_covars), #-ethnicity
                                cox_start = cox_start,
                                cox_stop = cox_stop,
                                study_start = ifelse(c=="prevax", prevax_start, vax_unvax_start),
                                study_stop = ifelse(c=="prevax", prevax_stop, vax_unvax_stop),
                                cut_points = ifelse(c=="prevax", prevax_cuts, vax_unvax_cuts),
                                controls_per_case = controls_per_case,
                                total_event_threshold = total_event_threshold,
                                episode_event_threshold = episode_event_threshold,
                                covariate_threshold = covariate_threshold,
                                age_spline = TRUE,
                                analysis = "sub_ethnicity_asian")
            
            ## analysis: sub_ethnicity_other -----------------------------------------------
            df[nrow(df)+1,] <- c(population = p,
                                cohort = c,
                                exposure = exposure, 
                                outcome = i,
                                ipw = ipw, 
                                strata = strata,
                                covariate_sex = covariate_sex,
                                covariate_age = covariate_age,
                                covariate_other = gsub("cov_cat_ethnicity;","",all_covars), #-ethnicity
                                cox_start = cox_start,
                                cox_stop = cox_stop,
                                study_start = ifelse(c=="prevax", prevax_start, vax_unvax_start),
                                study_stop = ifelse(c=="prevax", prevax_stop, vax_unvax_stop),
                                cut_points = ifelse(c=="prevax", prevax_cuts, vax_unvax_cuts),
                                controls_per_case = controls_per_case,
                                total_event_threshold = total_event_threshold,
                                episode_event_threshold = episode_event_threshold,
                                covariate_threshold = covariate_threshold,
                                age_spline = TRUE,
                                analysis = "sub_ethnicity_other")
            
            ## analysis: sub_smoking_never -----------------------------------------------
            df[nrow(df)+1,] <- c(population = p,
                                cohort = c,
                                exposure = exposure, 
                                outcome = i,
                                ipw = ipw, 
                                strata = strata,
                                covariate_sex = covariate_sex,
                                covariate_age = covariate_age,
                                covariate_other = gsub("cov_cat_smoking_status;","",all_covars), #-smoking status
                                cox_start = cox_start,
                                cox_stop = cox_stop,
                                study_start = ifelse(c=="prevax", prevax_start, vax_unvax_start),
                                study_stop = ifelse(c=="prevax", prevax_stop, vax_unvax_stop),
                                cut_points = ifelse(c=="prevax", prevax_cuts, vax_unvax_cuts),
                                controls_per_case = controls_per_case,
                                total_event_threshold = total_event_threshold,
                                episode_event_threshold = episode_event_threshold,
                                covariate_threshold = covariate_threshold,
                                age_spline = TRUE,
                                analysis = "sub_smoking_never")
            
            ## analysis: sub_smoking_ever -----------------------------------------------
            df[nrow(df)+1,] <- c(population = p,
                                cohort = c,
                                exposure = exposure, 
                                outcome = i,
                                ipw = ipw, 
                                strata = strata,
                                covariate_sex = covariate_sex,
                                covariate_age = covariate_age,
                                covariate_other = gsub("cov_cat_smoking_status;","",all_covars), #-smoking status
                                cox_start = cox_start,
                                cox_stop = cox_stop,
                                study_start = ifelse(c=="prevax", prevax_start, vax_unvax_start),
                                study_stop = ifelse(c=="prevax", prevax_stop, vax_unvax_stop),
                                cut_points = ifelse(c=="prevax", prevax_cuts, vax_unvax_cuts),
                                controls_per_case = controls_per_case,
                                total_event_threshold = total_event_threshold,
                                episode_event_threshold = episode_event_threshold,
                                covariate_threshold = covariate_threshold,
                                age_spline = TRUE,
                                analysis = "sub_smoking_ever")
            
            ## analysis: sub_smoking_current -----------------------------------------------
            df[nrow(df)+1,] <- c(population = p,
                                cohort = c,
                                exposure = exposure, 
                                outcome = i,
                                ipw = ipw, 
                                strata = strata,
                                covariate_sex = covariate_sex,
                                covariate_age = covariate_age,
                                covariate_other = gsub("cov_cat_smoking_status;","",all_covars), #-smoking status
                                cox_start = cox_start,
                                cox_stop = cox_stop,
                                study_start = ifelse(c=="prevax", prevax_start, vax_unvax_start),
                                study_stop = ifelse(c=="prevax", prevax_stop, vax_unvax_stop),
                                cut_points = ifelse(c=="prevax", prevax_cuts, vax_unvax_cuts),
                                controls_per_case = controls_per_case,
                                total_event_threshold = total_event_threshold,
                                episode_event_threshold = episode_event_threshold,
                                covariate_threshold = covariate_threshold,
                                age_spline = TRUE,
                                analysis = "sub_smoking_current")
    
        }

    }

}



df$name <- paste0("cohort_",df$cohort, "-", 
                  df$analysis, "-", 
                  gsub("out_date_","",df$outcome), "-", 
                  df$population)



# Check names are unique and save active analyses list -------------------------

if (length(unique(df$name))==nrow(df)) {
  saveRDS(df, file = "lib/active_analyses.rds", compress = "gzip")
} else {
  stop(paste0("ERROR: names must be unique in active analyses table"))
}

