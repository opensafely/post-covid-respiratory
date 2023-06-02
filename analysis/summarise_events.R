# Code by Alex Whitmarsh

library(Hmisc)
library(readr)

outcomes <- c("out_date_breathless", "out_date_cough", "out_date_urti", "out_date_asthma_exac", "out_date_copd_exac")

#read datasets
prevax <- readr::read_csv("output/input_prevax.csv.gz") %>%
  select_if(names(.) %in% outcomes)
vax <- readr::read_csv("output/input_vax.csv.gz") %>%
  select_if(names(.) %in% outcomes)
unvax <- readr::read_csv("output/input_unvax.csv.gz") %>%
  select_if(names(.) %in% outcomes)

#summary data 
describe_data <- function(data) {
  file_name <- paste0("output/summarize_events-", deparse(substitute(data)), ".txt")
  sink(file_name)
  print(Hmisc::describe(data))
  sink()
  message(paste0("Counts of ", deparse(substitute(data)), " written to ", file_name, " successfully!"))
}

describe_data(prevax)
describe_data(vax)
describe_data(unvax)