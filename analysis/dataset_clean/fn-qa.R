# Function to apply quality assurance
qa <- function(input, flow, study_dates) {
  print(
    'Quality assurance: Year of birth is before year of death (if year of death is available)'
  )

  input <- input[
    ((!is.na(input$cens_date_death)) &
      (year(input$cens_date_death) >= input$qa_num_birth_year)) |
      (is.na(input$cens_date_death)),
  ]
  flow[nrow(flow) + 1, ] <- c(
    "Quality assurance: Year of birth is before year of death (if year of death is available)",
    nrow(input)
  )

  print(
    'Quality assurance: Year of birth is before today (implemented using last data collection date)'
  )

  input <- input[
    !is.na(input$qa_num_birth_year) &
      (input$qa_num_birth_year <= format(Sys.Date(), "%Y")),
  ]
  flow[nrow(flow) + 1, ] <- c(
    "Quality assurance: Year of birth is before today (implemented using last data collection date)",
    nrow(input)
  )

  print(
    'Quality assurance: Date of death is before today (if year of death is available and implemented using last data collection date)'
  )

  input <- input[
    (!is.na(input$cens_date_death) &
      (input$cens_date_death <= format(Sys.Date(), "%Y-%m-%d"))) |
      is.na(input$cens_date_death),
  ]
  flow[nrow(flow) + 1, ] <- c(
    "Quality assurance: Date of death is before today (if year of death is available and implemented using last data collection date)",
    nrow(input)
  )

  print(
    'Quality assurance: Men do not have records that contain pregnancy and/or birth codes'
  )

  input <- input[
    !(input$cov_cat_sex == "male" & input$qa_bin_pregnancy == TRUE),
  ]
  flow[nrow(flow) + 1, ] <- c(
    "Quality assurance: Men do not have records that contain pregnancy and/or birth codes",
    nrow(input)
  )

  print(
    'Quality assurance: Men do not have records that contain HRT or COCP medication codes'
  )

  input <- input[
    !(input$cov_cat_sex == "male" & input$qa_bin_hrtcocp == TRUE),
  ]
  flow[nrow(flow) + 1, ] <- c(
    "Quality assurance: Men do not have records that contain HRT or COCP medication codes",
    nrow(input)
  )

  print(
    'Quality assurance: Women do not have records that contain prostate cancer codes'
  )

  input <- input[
    !(input$cov_cat_sex == "female" & input$qa_bin_prostate_cancer == TRUE),
  ]
  flow[nrow(flow) + 1, ] <- c(
    "Quality assurance: Women do not have records that contain prostate cancer codes",
    nrow(input)
  )

  return(list(input = input, flow = flow))
}
