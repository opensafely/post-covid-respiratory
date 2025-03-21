# Function to apply quality assurance
qa <- function(input, flow, study_dates) {
  print('Quality assurance: Year of birth is missing')

  input <- input[!is.na(input$qa_num_birth_year), ]
  flow[nrow(flow)+1,] <- c("Quality assurance: Year of birth is missing",
                                 nrow(input))

  print('Quality assurance: Year of birth is after year of death')

  input <- input[((!is.na(input$cens_date_death)) & 
                  (format(input$cens_date_death, "%Y") >= input$qa_num_birth_year)) |
                 (is.na(input$cens_date_death)), ]
  flow[nrow(flow)+1,] <- c("Quality assurance: Year of birth is after year of death",
                                 nrow(input))
  
  print('Quality assurance: Year of birth exceeds current date')
  
  input <- input[(input$qa_num_birth_year <= format(Sys.Date(), "%Y")), ]
  flow[nrow(flow)+1,] <- c("Quality assurance: Year of birth exceeds current date",
                                 nrow(input))
  
  print('Quality assurance: Date of death is invalid (after current date)')
  
  input <- input[(!is.na(input$cens_date_death) &  
  (input$cens_date_death <= format(Sys.Date(),"%Y-%m-%d"))) | 
  is.na(input$cens_date_death),]
  flow[nrow(flow)+1,] <- c("Quality assurance: Date of death is invalid (after current date)",
                                 nrow(input))

  print('Quality assurance: Pregnancy/birth codes for men')
  
  input <- input[!(input$qa_bin_pregnancy == TRUE & input$cov_cat_sex=="male") | is.na(input$cov_cat_sex),]
  flow[nrow(flow)+1,] <- c("Quality assurance: Pregnancy/birth codes for men",
                                 nrow(input))
  
  print('Quality assurance: HRT or COCP meds for men')
  
  input <- input[!(input$cov_cat_sex=="male" & input$qa_bin_hrtcocp==TRUE) | is.na(input$cov_cat_sex),]
  flow[nrow(flow)+1,] <- c("Quality assurance: HRT or COCP meds for men",
                                 nrow(input))
  
  print('Quality assurance: Prostate cancer codes for women')
  
  input <- input[!(input$qa_bin_prostate_cancer == TRUE & 
                     input$cov_cat_sex=="female") | is.na(input$cov_cat_sex),]
  flow[nrow(flow)+1,] <- c("Quality assurance: Prostate cancer codes for women",
                                 nrow(input))

  return(list(input = input, flow = flow))
}
