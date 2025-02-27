# Function to set reference levels for factors
ref <- function(input) {
  
  # Handle missing values --------------------------------------------------------
  print('Handle missing values')
  
  input$cov_cat_smoking <- replace(input$cov_cat_smoking, 
                                          is.na(input$cov_cat_smoking),
                                          "M")
  
  input <- input %>% 
    mutate(strat_cat_region = as.character(strat_cat_region)) %>%
    mutate(strat_cat_region = replace_na(strat_cat_region, "Missing")) %>%
    mutate(strat_cat_region = as.factor(strat_cat_region))
  
  # Set reference levels for factors ---------------------------------------------
  print('Set reference levels for factors')
  
  cat_factors <- colnames(input)[grepl("_cat_",colnames(input))]
  input[,cat_factors] <- lapply(input[,cat_factors], function(x) factor(x, ordered = FALSE))
  
  # Set reference level for variable: sub_cat_covidhospital -------------------
  print('Set reference level for variable: sub_cat_covidhospital')
  
  input$sub_cat_covidhospital <- ordered(input$sub_cat_covidhospital, 
                                            levels = c("non_hospitalised",
                                                       "hospitalised",
                                                       "no_infection"))
  
  # Set reference level for variable: cov_cat_ethnicity --------------------------
  print('Set reference level for variable: cov_cat_ethnicity')
  
  levels(input$cov_cat_ethnicity) <- list("Missing" = "0", "White" = "1", 
                                          "Mixed" = "2", "South Asian" = "3", 
                                          "Black" = "4", "Other" = "5")
  
  input$cov_cat_ethnicity <- ordered(input$cov_cat_ethnicity, 
                                     levels = c("White","Mixed",
                                                "South Asian","Black",
                                                "Other","Missing"))
  
  # Set reference level for variable: cov_cat_imd -------------------------------
  print('Set reference level for variable: cov_cat_imd')
  
  input$cov_cat_imd <- ordered(input$cov_cat_imd, 
                               levels = c("1 (most deprived)","2","3","4","5 (least deprived)"))
  
  # Set reference level for variable: strat_cat_region -----------------------------
  print('Set reference level for variable: strat_cat_region')
  
  input$strat_cat_region <- relevel(input$strat_cat_region, ref = "East")
  
  # Set reference level for variable: cov_cat_smoking ---------------------
  print('Set reference level for variable: cov_cat_smoking')
  
  levels(input$cov_cat_smoking) <- list("Ever smoker" = "E", "Missing" = "M", "Never smoker" = "N", "Current smoker" = "S")
  
  input$cov_cat_smoking <- ordered(input$cov_cat_smoking, levels = c("Never smoker","Ever smoker","Current smoker","Missing"))
  
  # Set reference level for variable: cov_cat_sex --------------------------------
  print('Set reference level for variable: cov_cat_sex')
  
  levels(input$cov_cat_sex) <- list("Female" = "female", "Male" = "male")
  
  
  input$cov_cat_sex <- factor(input$cov_cat_sex, 
                              levels = c("Female", "Male", "Unknown"))
  input$cov_cat_sex <- relevel(input$cov_cat_sex, ref = "Female")
  
  # Define cov_cat_age_group

  input$cov_cat_age_group <- numerical_to_categorical(input$cov_num_age,c(18,30,40,50,60,70,80,90)) 

  input$cov_cat_age_group <- ifelse(input$cov_cat_age_group == "<=17", "", input$cov_cat_age_group) 


  # Set reference level for variable: vax_cat_jcvi_group -------------------------
  print('Set reference level for variable: vax_cat_jcvi_group')
  
  input$vax_cat_jcvi_group <- ordered(input$vax_cat_jcvi_group, 
                                      levels = c("12","11","10",
                                                 "09","08","07",
                                                 "06","05","04",
                                                 "03","02","01","99"))
  
  # Set reference level for binaries ---------------------------------------------
  print('Set reference level for binaries')
  
  bin_factors <- colnames(input)[grepl("cov_bin_",colnames(input))]
  
  input[,bin_factors] <- lapply(input[,bin_factors], 
                                function(x) factor(x, levels = c("FALSE","TRUE")))
  
  return(list(input = input))
}


