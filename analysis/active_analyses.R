# Create output directory ------------------------------------------------------

fs::dir_create(here::here("lib"))

# Create empty data frame ------------------------------------------------------

df <- data.frame(name = character(), # Flexibility for tmp files to be run
                 cohort = character(),
                 exposure = character(), # Input for cox-ipw
                 outcome = character(), # Input for cox-ipw
                 strata = character(), # Input for cox-ipw
                 covariate_sex = character(), # Input for cox-ipw 
                 covariate_age = character(), # Input for cox-ipw
                 covariate_other = character(), # Input for cox-ipw
                 ipw = logical(), # Input for cox-ipw
                 cutpoints = character(), # Input for cox-ipw
                 main = logical(),
                 sub_covid_hospitalised = logical(),
                 sub_covid_nonhospitalised = logical(),
                 sub_covid_history = logical(),
                 sub_sex_female = logical(),
                 sub_sex_male = logical(),
                 sub_age_18_39 = logical(),
                 sub_age_40_59 = logical(),
                 sub_age_60_79 = logical(),
                 sub_age_80_110 = logical(),
                 sub_ethnicity_white = logical(),
                 sub_ethnicity_black = logical(),
                 sub_ethnicity_mixed = logical(),
                 sub_ethnicity_asian = logical(),
                 sub_ethnicity_other = logical(),
                 priorhistory_var = character(),
                 sub_priorhistory_true = logical(),
                 sub_priorhistory_false = logical(),
                 sub_smokingstatus_ever = logical(),
                 sub_smokingstatus_never = logical(),
                 sub_smokingstatus_current = logical(),
                 stringsAsFactors = FALSE)

# For example, add outcome AMI with all subgroups ------------------------------

df[nrow(df)+1,] <- c(name = "VAX_AMI",
                     cohort = "vaccinated",
                     exposure = "exp_date_covid19_confirmed",
                     outcome = "out_date_ami",
                     strata = "cov_cat_region",
                     covariate_sex = "cov_cat_sex",
                     covariate_age = "cov_num_age",
                     covariate_other = "cov_num_consulation_rate;cov_bin_healthcare_worker;cov_cat_ethnicity;cov_cat_deprivation;cov_cat_smoking_status;cov_bin_carehome_status;cov_bin_lipid_medications;cov_bin_antiplatelet_medications;cov_bin_anticoagulation_medications;cov_bin_combined_oral_contraceptive_pill;cov_bin_hormone_replacement_therapy;cov_bin_ami;cov_bin_all_stroke;cov_bin_other_arterial_embolism;cov_bin_vte;cov_bin_hf;cov_bin_angina;cov_bin_dementia;cov_bin_liver_disease;cov_bin_chronic_kidney_disease;cov_bin_cancer;cov_bin_hypertension;cov_bin_diabetes;cov_bin_obesity;cov_bin_depression;cov_bin_chronic_obstructive_pulmonary_disease",
                     ipw = TRUE,
                     cutpoints = "28;197",
                     main = TRUE,
                     sub_covid_hospitalised = TRUE,
                     sub_covid_nonhospitalised = TRUE,
                     sub_covid_history = TRUE,
                     sub_sex_female = TRUE,
                     sub_sex_male = TRUE,
                     sub_age_18_39 = TRUE,
                     sub_age_40_59 = TRUE,
                     sub_age_60_79 = TRUE,
                     sub_age_80_110 = TRUE,
                     sub_ethnicity_white = TRUE,
                     sub_ethnicity_black = TRUE,
                     sub_ethnicity_mixed = TRUE,
                     sub_ethnicity_asian = TRUE,
                     sub_ethnicity_other = TRUE,
                     priorhistory_var = "sub_bin_ate",
                     sub_priorhistory_true = TRUE,
                     sub_priorhistory_false = TRUE,
                     sub_smokingstatus_ever = TRUE,
                     sub_smokingstatus_never = TRUE,
                     sub_smokingstatus_current = TRUE)

# For example, test outcome AMI using prior history of AMI rather than ATE -----

df[nrow(df)+1,] <- c(name = "VAX_AMI_AMIpriorhistory",
                     cohort = "vaccinated",
                     exposure = "exp_date_covid19_confirmed",
                     outcome = "out_date_ami",
                     strata = "cov_cat_region",
                     covariate_sex = "cov_cat_sex",
                     covariate_age = "cov_num_age",
                     covariate_other = "cov_num_consulation_rate;cov_bin_healthcare_worker;cov_cat_ethnicity;cov_cat_deprivation;cov_cat_smoking_status;cov_bin_carehome_status;cov_bin_lipid_medications;cov_bin_antiplatelet_medications;cov_bin_anticoagulation_medications;cov_bin_combined_oral_contraceptive_pill;cov_bin_hormone_replacement_therapy;cov_bin_ami;cov_bin_all_stroke;cov_bin_other_arterial_embolism;cov_bin_vte;cov_bin_hf;cov_bin_angina;cov_bin_dementia;cov_bin_liver_disease;cov_bin_chronic_kidney_disease;cov_bin_cancer;cov_bin_hypertension;cov_bin_diabetes;cov_bin_obesity;cov_bin_depression;cov_bin_chronic_obstructive_pulmonary_disease",
                     ipw = TRUE,
                     cutpoints = "28;197",
                     main = FALSE,
                     sub_covid_hospitalised = FALSE,
                     sub_covid_nonhospitalised = FALSE,
                     sub_covid_history = FALSE,
                     sub_sex_female = FALSE,
                     sub_sex_male = FALSE,
                     sub_age_18_39 = FALSE,
                     sub_age_40_59 = FALSE,
                     sub_age_60_79 = FALSE,
                     sub_age_80_110 = FALSE,
                     sub_ethnicity_white = FALSE,
                     sub_ethnicity_black = FALSE,
                     sub_ethnicity_mixed = FALSE,
                     sub_ethnicity_asian = FALSE,
                     sub_ethnicity_other = FALSE,
                     priorhistory_var = "sub_bin_ami",
                     sub_priorhistory_true = TRUE,
                     sub_priorhistory_false = TRUE,
                     sub_smokingstatus_ever = FALSE,
                     sub_smokingstatus_never = FALSE,
                     sub_smokingstatus_current = FALSE)


# Check names are unique and save active analyses list -------------------------

if (length(unique(df$name))==nrow(df)) {
  saveRDS(df, file = "lib/active_analyses.rds")
} else {
  stop(paste0("ERROR: names must be unique in active analyses table"))
}