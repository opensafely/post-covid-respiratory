# Create output directory ------------------------------------------------------

fs::dir_create(here::here("lib"))

# Create empty data frame ------------------------------------------------------

df <- data.frame(name = character(), # Flexibility for tmp files to be run
                 cohort = character(),
                 exposure = character(),
                 outcome = character(),
                 strata = character(), # Input for cox-ipw; variables to be used as strata
                 covariate_other = character(), # Input for cox-ipw; covariates other than age and sex
                 covariate_protect = character(), # Input for cox-ipw; stop model if these covariates are removed
                 ipw = logical(), # Input for cox-ipw; TRUE to perform IPW
                 cutpoints = character(),
                 subgroup_covid_history = logical(),
                 subgroup_covid_hospitalised = logical(),
                 subgroup_covid_nonhospitalised = logical(),
                 subgroup_agegroups = character(),
                 subgroup_other = character(), # Semi-colon seperated list of variables to perform subgroups for
                 stringsAsFactors = FALSE)

# Add outcome 'acute myocardial infarction' ------------------------------------

df[nrow(df)+1,] <- c(name = "VAX_AMI_2TP",
                     cohort = "vaccinated",
                     exposure = "exp_date_covid19_confirmed",
                     outcome = "out_date_ami",
                     strata = "cov_cat_region",
                     covariate_other = "cov_num_consulation_rate;cov_bin_healthcare_worker;cov_num_age;cov_cat_ethnicity;cov_cat_deprivation;cov_cat_region;cov_cat_smoking_status;cov_bin_carehome_status;cov_bin_lipid_medications;cov_bin_antiplatelet_medications;cov_bin_anticoagulation_medications;cov_bin_combined_oral_contraceptive_pill;cov_bin_hormone_replacement_therapy;cov_bin_ami;cov_bin_all_stroke;cov_bin_other_arterial_embolism;cov_bin_vte;cov_bin_hf;cov_bin_angina;cov_bin_dementia;cov_bin_liver_disease;cov_bin_chronic_kidney_disease;cov_bin_cancer;cov_bin_hypertension;cov_bin_diabetes;cov_bin_obesity;cov_bin_depression;cov_bin_chronic_obstructive_pulmonary_disease;cov_cat_sex",
                     covariate_protect = "cov_cat_ethnicity;cov_cat_region;cov_cat_sex;cov_num_age",
                     ipw = TRUE,
                     cutpoints = "28;197",
                     subgroup_covid_history = TRUE,
                     subgroup_covid_hospitalised = TRUE,
                     subgroup_covid_nonhospitalised = TRUE,
                     subgroups = "sub_cat_agegroup;cov_cat_ethnicity;cov_cat_sex;sub_bin_ate")

# Save active analyses list ----------------------------------------------------

saveRDS(df, file = "lib/active_analyses.rds")
