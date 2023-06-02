library(data.table)
library(dplyr)
library(Hmisc)
library(readr)

output_dir <- "C:/Users/rs22981/GitHub/post-covid-respiratory/output/"

outcomes <- c("out_date_breathless", "out_date_cough", "out_date_urti", "out_date_asthma_exac", "out_date_copd_exac")

#read datasets
prevax <- read_csv("output/input_prevax.csv.gz") %>%
  select_if(names(.) %in% outcomes)
vax <- read_csv("output/input_vax.csv.gz") %>%
  select_if(names(.) %in% outcomes)
unvax <- read_csv("output/input_unvax.csv.gz") %>%
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