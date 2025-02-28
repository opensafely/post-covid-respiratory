# Function to preprocess data

preproc1 <- function(cohort) {

  # Get column names -------------------------------------------------------------
  print('Get column names')

  file_path <- paste0("output/dataset_definition/input_", cohort, ".csv.gz")
  all_cols <- fread(file_path, 
              header = TRUE, 
              sep = ",", 
              nrows = 0, 
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
  
  input_raw <- read_csv(file_path, col_types = col_classes)
  message(paste0("Dataset has been read successfully with N = ", nrow(input_raw), " rows"))
  
  # Format dataset columns ---------------------------------------------------------------
  print('Format dataset columns')
  
  input_raw <- input_raw %>%
  mutate(across(all_of(date_cols),
              ~ floor_date(as.Date(., format="%Y-%m-%d"), unit = "days")),
      across(contains('_birth_year'), 
              ~ format(as.Date(., origin = "1970-01-01"), "%Y")),
      across(all_of(num_cols), ~ as.numeric(.)), 
      across(all_of(cat_cols), ~ as.factor(.))) 
  message("Dataset columns formatted")

  return(list(input_raw = input_raw))
}

preproc2 <- function(cohort, input_raw) {
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

  return(list(input_venn = input_venn, input_preproc = input_preproc))
}

