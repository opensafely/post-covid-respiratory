# Modified from https://github.com/opensafely/waning-ve-2dose-1year/blob/main/analysis/dummy_data_vax.R
# And https://github.com/opensafely/post-covid-vaccinated/blob/main/analysis/modify_dummy_vax_data.R

modify_dummy <- function(df, cohort) {

  # Set seed -------------------------------------------------------------------
  set.seed(1)

  # Modifying vax-specific variables

  if (cohort == "vax") {

    df <- df %>%

      # Change first jab date so that they have roughly correct distribution
      mutate(
        vax_date_Pfizer_1      = as.Date(vax_date_eligible) + days(round(rnorm(nrow(.), mean = 10, sd = 3))),
        vax_date_AstraZeneca_1 = as.Date(vax_date_eligible) + days(round(rnorm(nrow(.), mean = 10, sd = 3))),
        vax_date_Moderna_1     = as.Date(vax_date_eligible) + days(round(rnorm(nrow(.), mean = 10, sd = 3)))
      ) %>%

      #Pick one vaccine type
      mutate(
        vaccine_1_type = sample(
          x       = c("Pfizer", "AstraZeneca", "Moderna", "None"),
          size    = nrow(.),
          replace = TRUE,
          prob    = c(0.4, 0.4, 0.05, 0.1)
        ),

        # jabs missingness probabilities
        missing_pfizer_2  = rbernoulli(nrow(.), p = 0.05),
        missing_az_2      = rbernoulli(nrow(.), p = 0.05),
        missing_moderna_2 = rbernoulli(nrow(.), p = 0.05),
        missing_pfizer_3  = rbernoulli(nrow(.), p = 0.90),
        missing_az_3      = rbernoulli(nrow(.), p = 0.90),
        missing_moderna_3 = rbernoulli(nrow(.), p = 0.90)
      ) %>%

      #Set first jab date according to type and set others to NA
      mutate(across(vax_date_Pfizer_1,
                    ~if_else(
                      vaccine_1_type %in% "Pfizer",
                      .x,
                      NA_Date_))) %>%
      mutate(across(vax_date_AstraZeneca_1,
                    ~if_else(
                      vaccine_1_type %in% "AstraZeneca",
                      .x,
                      NA_Date_))) %>%
      mutate(across(vax_date_Moderna_1,
                    ~if_else(
                      vaccine_1_type %in% "Moderna",
                      .x,
                      NA_Date_))) %>%

      mutate(across(matches("vax_date\\w+_1"),
                    ~ if_else(
                      vaccine_1_type %in% "None",
                      NA_Date_,
                      .x
                    ))) %>%

      #Change date for the second jab
      mutate(
        vax_date_Pfizer_2      = vax_date_Pfizer_1      + days(round(rnorm(nrow(.), mean = 10*7, sd = 3))),
        vax_date_AstraZeneca_2 = vax_date_AstraZeneca_1 + days(round(rnorm(nrow(.), mean = 10*7, sd = 3))),
        vax_date_Moderna_2     = vax_date_Moderna_1     + days(round(rnorm(nrow(.), mean = 10*7, sd = 3))),
      ) %>%

      # Set 2nd vaccine type
      mutate(vaccine_2_type =  ifelse(runif(nrow(df),0,1)>0.95 & vaccine_1_type!="None",
                                       sample(
                                        x = c("Pfizer", "AstraZeneca", "Moderna",  "None"),
                                        size = nrow(.),
                                        replace = TRUE,
                                        prob = c(0.4, 0.4, 0.05, 0.1)
                                        ), vaccine_1_type)) %>%

      #Set second jab date according to type and set others to NA
      mutate(across(vax_date_Pfizer_2,
                    ~if_else(
                      vaccine_2_type %in% "Pfizer",
                      .x,
                      NA_Date_))) %>%
      mutate(across(vax_date_AstraZeneca_2,
                    ~if_else(
                      vaccine_2_type %in% "AstraZeneca",
                      .x,
                      NA_Date_))) %>%
      mutate(across(vax_date_Moderna_2,
                    ~if_else(
                      vaccine_2_type %in% "Moderna",
                      .x,
                      NA_Date_))) %>%

      mutate(across(matches("vax_date\\w+_2"),
                    ~ if_else(
                      vaccine_2_type %in% "None",
                      NA_Date_,
                      .x
                    ))) %>%

      # Set to NA if jab is missing
      mutate(across(vax_date_Pfizer_2,
                    ~if_else(
                      missing_pfizer_2,
                      NA_Date_,
                      .x))) %>%
      mutate(across(vax_date_AstraZeneca_2,
                    ~if_else(
                      missing_az_2,
                      NA_Date_,
                      .x))) %>%
      mutate(across(vax_date_Moderna_2,
                    ~if_else(
                      missing_moderna_2,
                      NA_Date_,
                      .x))) %>%

      #Set 3rd jab type
      mutate(vaccine_3_type =  ifelse(vaccine_2_type != "None",
                                       sample(
                                         x       = c("Pfizer", "AstraZeneca" ,"Moderna",  "None"),
                                         size    = nrow(.),
                                         replace = TRUE,
                                         prob    = c(0.6, 0.1, 0.3, 0.1)
                                         ), vaccine_2_type)) %>%

      #Change 3rd jab date
      mutate(
        vax_date_Pfizer_3      = vax_date_Pfizer_2      + days(round(rnorm(nrow(.), mean = 6*4*7, sd = 7))),
        vax_date_AstraZeneca_3 = vax_date_AstraZeneca_2 + days(round(rnorm(nrow(.), mean = 6*4*7, sd = 7))),
        vax_date_Moderna_3     = vax_date_Moderna_2     + days(round(rnorm(nrow(.), mean = 6*4*7, sd = 7))),
      ) %>%

      #Set 3rd jab date according to type and set others to NA
      mutate(across(vax_date_Pfizer_3,
                    ~if_else(
                      vaccine_3_type %in% "Pfizer",
                      .x,
                      NA_Date_))) %>%
      mutate(across(vax_date_AstraZeneca_3,
                    ~if_else(
                      vaccine_3_type %in% "AstraZeneca",
                      .x,
                      NA_Date_))) %>%
      mutate(across(vax_date_Moderna_3,
                    ~if_else(
                      vaccine_3_type %in% "Moderna",
                      .x,
                      NA_Date_))) %>%

      mutate(across(matches("vax_date\\w+_3"),
                    ~ if_else(
                      vaccine_3_type %in% "None",
                      NA_Date_,
                      .x
                    ))) %>%

      # Set to NA if jab is missing
      mutate(across(vax_date_Pfizer_3,
                    ~if_else(
                      missing_pfizer_3,
                      NA_Date_,
                      .x))) %>%
      mutate(across(vax_date_AstraZeneca_3,
                    ~if_else(
                      missing_az_3,
                      NA_Date_,
                      .x))) %>%
      mutate(across(vax_date_Moderna_3,
                    ~if_else(
                      missing_moderna_3,
                      NA_Date_,
                      .x))) %>%

      select(-starts_with("missing"),-matches("vaccine_\\d_type"))

  } else if (cohort == "unvax") { # Modifying unvax-specific variables

    df <- df %>%

    ## JCVI distribution

      mutate(vax_cat_jcvi_group = sample(
      x       = c("12","11","10",
                  "09","08","07",
                  "06","05","04",
                  "03","02","01","99"), # 8.25 for each group, 1.6% for missing
      size    = nrow(.),
      replace = TRUE,
      prob    = c(0.082,0.082,0.082,0.082,0.082,0.082,0.082,0.082,0.082,0.082,0.082,0.082, 0.016)))

  }

  ## Modifying variables across cohorts

  ## inex_bin_alive ----------------------------------------------------------

  df <- df %>%

    ## Alive on the index date
    mutate(inex_bin_alive = rbernoulli(nrow(.), p = 0.99)) %>% 

    # could also modify cens_date_death to match

    ## Registered for a minimum of 6 months prior to index date
    mutate(inex_bin_6m_reg = rbernoulli(nrow(.), p = 0.99)) %>% 

    ## Age distribution
    mutate(cov_num_age = sample(
                        c(sample(  1:17,          round(nrow(.)*0.02),  replace=TRUE), # Number <18
                          sample(110:120,         round(nrow(.)*0.02),  replace=TRUE), # Number >110
                          sample( 18:110, nrow(.)-round(nrow(.)*0.02)*2,replace=TRUE)))) %>% # Remainder

    ## Recalculate birth year based on new age
    mutate(qa_num_birth_year = as.numeric(format(as.Date(index_date), "%Y"))-cov_num_age) %>%

    ## Sex
    mutate(cov_cat_sex = sample(
                  x       = c("female",
                              "male",
                              "intersex",
                              "unknown"), # %49% Female, 49% Male, 1% Intersex, 1% missing
                  size    = nrow(.),
                  replace = TRUE,
                  prob    = c(0.49, 0.49, 0.01, 0.01))) %>%

    ## Region
    mutate(strat_cat_region = sample(
                  x       = c("East",
                              "East Midlands",
                              "London",
                              "North East",
                              "North West",
                              "South East",
                              "South West",
                              "West Midlands",
                              "Yorkshire and The Humber",
                              ""), # 11% for each area, %1 Missing
                  size    = nrow(.),
                  replace = TRUE,
                  prob    = c(0.11, 0.11, 0.11, 0.11, 0.11, 0.11, 0.11, 0.11, 0.11, 0.01))) %>%

    ## IMD
    mutate(cov_cat_imd   = sample(
                  x       = c("1 (most deprived)",
                            "2",
                            "3",
                            "4",
                            "5 (least deprived)",
                            NA), # 19.5% for each area, 2.5% missing
                  size    = nrow(.),
                  replace = TRUE,
                  prob    = c(0.195, 0.195, 0.195, 0.195, 0.195, 0.025)
    ))

}