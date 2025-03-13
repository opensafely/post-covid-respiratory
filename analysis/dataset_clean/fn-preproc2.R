# Second function to preprocess data

preproc2 <- function(cohort, input_raw, describe_flag, describe_dir) {
  # Remove records with missing patient id ---------------------------------------

  input_valid_id <- input_raw[!is.na(input_raw$patient_id),]

  message("All records with valid patient IDs retained.")

  # Restrict columns and save Venn diagram input dataset -------------------------

  input_venn <- input_valid_id %>% select(starts_with(c("patient_id","tmp_out_date","out_date")))

  # Restrict columns and save preprocess dataset ---------------------------------

  input_preproc <- input_valid_id %>% 
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

  input_preproc[,colnames(input_preproc)[grepl("tmp_",colnames(input_preproc))]] <- NULL

  # Describe Venn diagram data
  if (describe_flag == "describe_TRUE") {
    sink(paste0(describe_dir, "desc_venn_", cohort, ".txt"))
    print(Hmisc::describe(input_venn))
    sink()
    message("Venn diagram data saved successfully")
} else {
    message("No description written, change input flag if description is desired.")
}

  # Describe preprocess data

  if (describe_flag == "describe_TRUE") {
    sink(paste0(describe_dir, "desc_preproc_", cohort, ".txt"))
    print(Hmisc::describe(input_preproc))
    sink()
    message (paste0("Cohort ", cohort, " with valid patient IDs description written successfully!"))
} else {
    message ("No description written, change input flag if description is desired.")
}

  return(list(input_venn = input_venn, input_preproc = input_preproc))
}

