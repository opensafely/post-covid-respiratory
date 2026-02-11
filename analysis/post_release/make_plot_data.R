# Load model output --------------------------------------------------------------------
print("Load model output")

# List all CSV files matching the pattern
file_list <- list.files(
  path = path_release,
  pattern = "^model_output-.*-midpoint6\\.csv$",
  full.names = TRUE
)

# Read and combine all CSV files into one data frame
df <- file_list %>%
  lapply(read_csv, show_col_types = FALSE) %>%
  bind_rows()

df <- df %>%
  filter(
    !name %in% "cohort_vax-sub_sex_male_preex_FALSE-copd"
  ) 

readr::write_csv(df, "output/post_release/plot_model_output.csv")
