library(tidyverse)  

# Generate empty dataset for results -------------------------------------------
print('Generate empty datasets for results')

max_events <- list()

# Generate summary data for each cohort ----------------------------------------
print('Generate summary data for each cohort')

# Create output directory ------------------------------------------------------
outdir <- here::here("output", "preprocess")
fs::dir_create(outdir)

for (cohort in c("prevax", "unvax", "vax")) {
  
  # Load cohort data -----------------------------------------------------------
  print('Load cohort data')
  
  df <- readr::read_csv(file = paste0("output/input_",cohort,".csv.gz") )
  print(paste0("Dataset has been read successfully with N = ", nrow(df), " rows"))
  
  # reshape data to get percentiles
  df_n <- df %>%
    select(patient_id, starts_with("out_n_")) %>%
    pivot_longer(
      cols = -patient_id,
      names_transform = ~str_remove(.x, "out_n_")
      ) %>%
    # get the 99.999th percentile
    # this means we can say we have all events for 99.999% of the patients
    group_by(name) %>%
    summarise(n = ceiling(unname(quantile(x=value, probs = 0.99999)))) %>%
    ungroup() 
  
  # add percentil results to list
  max_events[[cohort]] <- as.list(df_n$n)
  names(max_events[[cohort]]) <- df_n$name
  rm(df_n)
  
  # save ids and 5th outcome date for anyone with >5 events for any outcome
  df_dates <- df %>%
    # only keep the dates of the 5th events
    select(patient_id, matches("out_date_\\w+_5")) %>%
    mutate(across(matches("out_date_\\w+_5"), as.Date)) %>%
    # only keep rows where at least one date is nonmissing
    filter_at(vars(starts_with("out_date_")), any_vars(!is.na(.)))
  write_csv(
    df_dates,
    file.path(outdir, glue::glue("out_date_5_{cohort}.csv.gz"))
  )
  rm(df_dates)
  
}

# save df_n_all to read into study definition
jsonlite::write_json(
  max_events, 
  path = file.path(outdir, "max_events.json"),
  auto_unbox=TRUE, 
  pretty =TRUE
  )
