# Load packages ----------------------------------------------------------------
print('Load packages')

library(magrittr)
library(tidyverse)

# Source common functions ------------------------------------------------------
print('Source common functions')

source("analysis/utility.R")

# Define model output folder ---------------------------------------
print("Creating output/model output folder")

# setting up the sub directory
makeout_dir <- "output/make_output/"
output_folder <- "output/describe/"
# check if sub directory exists, create if not
fs::dir_create(here::here(output_folder))

# Find all model files ---------------------------------------------------------
print('Collecting list of models')
file_list <- list.files(
    path = makeout_dir,
    pattern = "^model_output-.*-midpoint6\\.csv$",
    full.names = TRUE
)

# Read and combine all CSV files into one data frame---------------------------
print('Loading and combining models')
df <- file_list %>%
    lapply(read_csv, show_col_types = FALSE) %>%
    bind_rows()

# Define bounds for checking --------------------------------------------------
lb <- 0.25
ub <- 32

# Find relevant rows ----------------------------------------------------------
print('Filtering dataframe')
df <- df[df$hr < lb | df$hr > ub | df$conf_low < lb | df$conf_high > ub, ] # HR range restriction
df <- df[!(df$term %in% c("days_pre", "days0_1")), ] # Term restriction
df <- df[!is.na(df$name), ] #NA filtering

unconverged_list <- unique(df$name)

# Save list of models ---------------------------------------------------------
print("Saving .txt list of models")
writeLines(
    paste(unconverged_list, collapse = ",\n"),
    paste0(output_folder, "unconverged_models.txt")
)

# Save related dataframe -------------------------------------------------------
print("Save dataframe of unconverged models")

readr::write_csv(
    df,
    paste0(output_folder, "unconverged_models.csv"),
    na = "-"
)
