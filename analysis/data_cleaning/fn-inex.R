# Function to apply inclusion criteria

inex <- function(input, consort, cohort, vax_start_date, mixed_vax_threshold, start_date_delta) {
  
  print('Inclusion criteria: Alive at index')
  
  input <- subset(input, input$inex_bin_alive == TRUE) # Subset input if alive at index.
  consort[nrow(consort)+1,] <- c("Inclusion criteria: Alive at index",
                                 nrow(input))
  
  print('Inclusion criteria: Known age 18 or over at index')
  
  input <- subset(input, input$cov_num_age >= 18) # Subset input if age between 18 and 110 at index.
  consort[nrow(consort)+1,] <- c("Inclusion criteria: Known age 18 or over at index",
                                 nrow(input))
  
  print('Inclusion criteria: Known age 110 or under at index')
  
  input <- subset(input, input$cov_num_age <= 110) # Subset input if age between 18 and 110 on 01/06/2021.
  consort[nrow(consort)+1,] <- c("Inclusion criteria: Known age 110 or under at index",
                                 nrow(input))
  
  print('Inclusion criteria: Known sex at index')
  
  input <- input[!is.na(input$cov_cat_sex),] # removes NAs, if any
  input$cov_cat_sex <- relevel(input$cov_cat_sex, ref = "Female")
  consort[nrow(consort)+1,] <- c("Inclusion criteria: Known sex at index",
                                 nrow(input))
  
  print('Inclusion criteria: Known IMD at index')
  
  input <- input[!is.na(input$cov_cat_imd),] # removes NAs, if any
  input$cov_cat_imd <- ordered(input$cov_cat_imd, 
                               levels = c("1 (most deprived)","2","3","4","5 (least deprived)"))
  consort[nrow(consort)+1,] <- c("Inclusion criteria: Known IMD at index",
                                 nrow(input))
  
  print('Inclusion criteria: Continuous registration with the same practice for at least six months up to and including the index date')
  
  input <- subset(input, input$inex_bin_6m_reg == TRUE)
  consort[nrow(consort)+1,] <- c("Inclusion criteria: Continuous registration with the same practice for at least six months up to and including the index date",
                                 nrow(input))
  
  print('Inclusion criteria: Known region at index')
  
  input <- input %>% mutate(cov_cat_region = as.character(cov_cat_region)) %>%
    filter(cov_cat_region != "Missing")%>%
    mutate(cov_cat_region = as.factor(cov_cat_region))
  input$cov_cat_region <- relevel(input$cov_cat_region, ref = "East")
  consort[nrow(consort)+1,] <- c("Inclusion criteria: Known region at index",
                                 nrow(input))
# Apply cohort specific inclusion criteria -------------------------------------
  print('Apply cohort specific inclusion criteria')
  
  if (cohort == "vax") {
    print('Inclusion criteria: Record of two vaccination doses prior to the study end date')

    input$vax_gap <- input$vax_date_covid_2 - input$vax_date_covid_1 #Determine the vaccination gap in days : gap is NA if any vaccine date is missing
    input <- input[!is.na(input$vax_gap),] # Subset the fully vaccinated group
    consort[nrow(consort)+1,] <- c("Inclusion criteria: Record of two vaccination doses prior to the study end date",
                                    nrow(input))

    print('Inclusion criteria: Did not receive a vaccination prior to 08-12-2020 (i.e., the start of the vaccination program)')

    input <- subset(input, input$vax_date_covid_1 >= vax_start_date & input$vax_date_covid_2 >= vax_start_date)
    consort[nrow(consort)+1,] <- c("Inclusion criteria: Did not receive a vaccination prior to 08-12-2020 (i.e., the start of the vaccination program)",
                                    nrow(input))

    print('Inclusion criteria: Did not recieve a second dose vaccination before their first dose vaccination')

    input <- subset(input, input$vax_gap >= 0) # Keep those with positive vaccination gap
    consort[nrow(consort)+1,] <- c("Inclusion criteria: Did not recieve a second dose vaccination before their first dose vaccination",
                                    nrow(input))

    print('Inclusion criteria: Did not recieve a second dose vaccination less than three weeks after their first dose')

    input <- subset(input, input$vax_gap >= 21) # Keep those with at least 3 weeks vaccination gap
    consort[nrow(consort)+1,] <- c("Inclusion criteria: Did not recieve a second dose vaccination before their first dose vaccination",
                                    nrow(input))

    print('Inclusion criteria: Did not recieve a mixed vaccine products before 07-05-2021')

    # Trick to run the mixed vaccine code on dummy data with limited levels -> To ensure that the levels are the same in vax_cat_product variables

    input <- input %>%
      mutate(
        AZ_date = as.numeric(ifelse(vax_date_AstraZeneca_1 < mixed_vax_threshold, 1,
                          ifelse(vax_date_AstraZeneca_2 < mixed_vax_threshold, 1,
                                 ifelse(vax_date_AstraZeneca_3 < mixed_vax_threshold, 1, 0)))),
        Moderna_date = as.numeric(ifelse(vax_date_Moderna_1 < mixed_vax_threshold, 1,
                              ifelse(vax_date_Moderna_2 < mixed_vax_threshold, 1,
                                     ifelse(vax_date_Moderna_3 < mixed_vax_threshold, 1, 0)))),
        Pfizer_date = as.numeric(ifelse(vax_date_Pfizer_1 < mixed_vax_threshold, 1,
                              ifelse(vax_date_Pfizer_2 < mixed_vax_threshold, 1,
                                     ifelse(vax_date_Pfizer_3 < mixed_vax_threshold, 1, 0))))
      ) %>%
      rowwise() %>%
      mutate(vax_mixed = sum(c_across(c(AZ_date, Moderna_date, Pfizer_date)), na.rm = TRUE)) %>%
      ungroup() %>%
      dplyr::filter(vax_mixed < 2)

    consort[nrow(consort)+1,] <- c("Inclusion criteria: Did not recieve a mixed vaccine products before 07-05-2021",
                                    nrow(input))

    print('Inclusion criteria: Index date is before cohort end date')

    # Will remove anyone who is not fully vaccinated by the cohort end date

    input <- input %>% filter (!is.na(index_date) & index_date <= end_date_exposure & index_date >= start_date_delta)
    consort[nrow(consort)+1,] <- c("Inclusion criteria: Index date is before cohort end date",
                                    nrow(input))
                                    
  } else if (cohort == "unvax"){

    print('Inclusion criteria: Does not have a record of one or more vaccination prior index date')

    # i.e. Have a record of a first vaccination prior to index date
    # (no more vax 2 and 3 variables available in this dataset)
    # a.Determine the vaccination status on index start date

    input$prior_vax1 <- ifelse(input$vax_date_covid_1 <= input$index_date, 1,0)
    input$prior_vax1[is.na(input$prior_vax1)] <- 0
    input <- subset(input, input$prior_vax1 == 0) # Exclude people with prior vaccination
    consort[nrow(consort)+1,] <- c("Inclusion criteria: Does not have a record of one or more vaccination prior index date",
                                    nrow(input))

    print('Inclusion criteria: Not missing JCVI group')

    input <- subset(input, vax_cat_jcvi_group == "01" | vax_cat_jcvi_group == "02" | vax_cat_jcvi_group == "03" | vax_cat_jcvi_group == "04" |
                        vax_cat_jcvi_group == "05" | vax_cat_jcvi_group == "06" | vax_cat_jcvi_group == "07" | vax_cat_jcvi_group == "08" |
                        vax_cat_jcvi_group == "09" | vax_cat_jcvi_group == "10" | vax_cat_jcvi_group == "11" | vax_cat_jcvi_group == "12")
    consort[nrow(consort)+1,] <- c("Inclusion criteria: Not missing JCVI group",
                                    nrow(input))

    print('Inclusion criteria: Index date is not before cohort end date - will remove anyone whose eligibility date + 84 days is after study end date (only those with unknown JCVI group)')

    input <- input %>% filter (!is.na(index_date) & index_date <= end_date_exposure & index_date >= start_date_delta)
    consort[nrow(consort)+1,] <- c("Inclusion criteria: Index date is not before cohort end date - will remove anyone whose eligibility date + 84 days is after study end date (only those with unknown JCVI group)",
                                    nrow(input))

  }
  
  return(list(input = input, consort = consort))
}
