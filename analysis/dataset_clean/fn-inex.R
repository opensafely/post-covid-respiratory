# Function to apply inclusion criteria

inex <- function(input, flow, cohort, vax_start_date, mixed_vax_threshold, delta_date) {

  ## Apply inclusion criteria to all cohorts --------------------------------------
  print('Inclusion criteria: Alive at index')
  
  input <- subset(input, input$inex_bin_alive == TRUE) # Subset input if alive at index.
  flow[nrow(flow)+1,] <- c("Inclusion criteria: Alive at index",
                                 nrow(input))
  
  print('Inclusion criteria: Known age 18 or over at index')
  
  input <- subset(input, input$cov_num_age >= 18) # Subset input if age between 18 and 110 at index.
  flow[nrow(flow)+1,] <- c("Inclusion criteria: Known age 18 or over at index",
                                 nrow(input))
  
  print('Inclusion criteria: Known age 110 or under at index')
  
  input <- subset(input, input$cov_num_age <= 110) # Subset input if age between 18 and 110 on 01/06/2021.
  flow[nrow(flow)+1,] <- c("Inclusion criteria: Known age 110 or under at index",
                                 nrow(input))
  
  print('Inclusion criteria: Known sex at index')

  input <- subset(input, cov_cat_sex %in% c("female", "male"))
  flow[nrow(flow)+1,] <- c("Inclusion criteria: Known sex at index",
                                 nrow(input))
  
  print('Inclusion criteria: Known IMD at index')

  input <- subset(input, cov_cat_imd %in% c("1 (most deprived)","2","3","4","5 (least deprived)"))
  flow[nrow(flow)+1,] <- c("Inclusion criteria: Known IMD at index",
                                 nrow(input))
  
  print('Inclusion criteria: Known region at index')

  input <- subset(input, strat_cat_region %in% c(
    "East", "East Midlands", "London", 
    "North East", "North West", 
    "South East", "South West", 
    "West Midlands", "Yorkshire and The Humber"
  ))
  flow[nrow(flow)+1,] <- c("Inclusion criteria: Known region at index",
                                 nrow(input))
    
  print('Inclusion criteria: Continuous registration with the same practice for at least six months up to and including the index date')
  
  input <- subset(input, input$inex_bin_6m_reg == TRUE)
  flow[nrow(flow)+1,] <- c("Inclusion criteria: Continuous registration with the same practice for at least six months up to and including the index date",
                                 nrow(input))

  ## Apply cohort specific inclusion criteria -------------------------------------
  print('Apply cohort specific inclusion criteria')
  
  if (cohort == "vax") {
    print('Inclusion criteria: Record of two vaccination doses prior to the study end date')

    input <- subset(input, !is.na(vax_date_covid_1) & !is.na(vax_date_covid_2))
    flow[nrow(flow)+1,] <- c("Inclusion criteria: Record of two vaccination doses prior to the study end date",
                                    nrow(input))

    print('Inclusion criteria: Did not receive a vaccination prior to 08-12-2020 (i.e., the start of the vaccination program)')

    input <- subset(input, input$vax_date_covid_1 >= vax_start_date)
    flow[nrow(flow)+1,] <- c("Inclusion criteria: Did not receive a vaccination prior to 08-12-2020 (i.e., the start of the vaccination program)",
                                    nrow(input))

    print('Inclusion criteria: Did not recieve a second dose vaccination before their first dose vaccination')

    input <- subset(input, input$vax_date_covid_2 >= input$vax_date_covid_1) 
    flow[nrow(flow)+1,] <- c("Inclusion criteria: Did not recieve a second dose vaccination before their first dose vaccination",
                                    nrow(input))

    print('Inclusion criteria: Did not recieve a second dose vaccination less than three weeks after their first dose')

    input <- subset(input, (input$vax_date_covid_2 - input$vax_date_covid_1) >= 21) 
    flow[nrow(flow)+1,] <- c("Inclusion criteria: Did not recieve a second dose vaccination before their first dose vaccination",
                                    nrow(input))

    print('Inclusion criteria: Did not recieve a mixed vaccine products before 07-05-2021')
    input <- input %>%
      mutate(
        AZ_date = case_when(
          vax_date_AstraZeneca_1 < mixed_vax_threshold ~ 1,
          vax_date_AstraZeneca_2 < mixed_vax_threshold ~ 1,
          vax_date_AstraZeneca_3 < mixed_vax_threshold ~ 1,
          TRUE ~ 0
        ),
        Moderna_date = case_when(
          vax_date_Moderna_1 < mixed_vax_threshold ~ 1,
          vax_date_Moderna_2 < mixed_vax_threshold ~ 1,
          vax_date_Moderna_3 < mixed_vax_threshold ~ 1,
          TRUE ~ 0
        ),
        Pfizer_date = case_when(
          vax_date_Pfizer_1 < mixed_vax_threshold ~ 1,
          vax_date_Pfizer_2 < mixed_vax_threshold ~ 1,
          vax_date_Pfizer_3 < mixed_vax_threshold ~ 1,
          TRUE ~ 0
        )
      ) %>%
      rowwise() %>%
      mutate(vax_mixed = sum(c_across(c(AZ_date, Moderna_date, Pfizer_date)), na.rm = TRUE)) %>%
      ungroup() %>%
      dplyr::filter(vax_mixed < 2) %>%
      select(-AZ_date, -Moderna_date, -Pfizer_date, -vax_mixed)

    flow[nrow(flow)+1,] <- c("Inclusion criteria: Did not recieve a mixed vaccine products before 07-05-2021",
                                    nrow(input))

    print('Inclusion criteria: Index date is before cohort end date')

    # Will remove anyone who is not fully vaccinated by the cohort end date

    input <- subset(input, index_date <= end_date_exposure & index_date >= delta_date)
    flow[nrow(flow)+1,] <- c("Inclusion criteria: Index date is before cohort end date",
                                    nrow(input))
                                    
  } else if (cohort == "unvax"){

    print('Inclusion criteria: Does not have a record of one or more vaccination prior index date')

    # i.e. Have a record of a first vaccination prior to index date
    # (no more vax 2 and 3 variables available in this dataset)
    # a.Determine the vaccination status on index start date

    input <- subset(input, (input$vax_date_covid_1 <= input$index_date & !is.na(input$vax_date_covid_1)) | is.na(input$vax_date_covid_1))
    flow[nrow(flow)+1,] <- c("Inclusion criteria: Does not have a record of one or more vaccination prior index date",
                                    nrow(input))

    print('Inclusion criteria: Not missing JCVI group')

    input <- subset(input, vax_cat_jcvi_group %in% c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12"))
    flow[nrow(flow)+1,] <- c("Inclusion criteria: Not missing JCVI group",
                                    nrow(input))

    print('Inclusion criteria: Index date is not before cohort end date - will remove anyone whose eligibility date + 84 days is after study end date (only those with unknown JCVI group)')
    
    input <- subset(input, index_date <= end_date_exposure & index_date >= delta_date)
    flow[nrow(flow)+1,] <- c("Inclusion criteria: Index date is not before cohort end date - will remove anyone whose eligibility date + 84 days is after study end date (only those with unknown JCVI group)",
                                    nrow(input))

  }
  
  return(list(input = input, flow = flow))
}
