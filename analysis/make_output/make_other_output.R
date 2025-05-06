# Load packages ----------------------------------------------------------------
print('Load packages')

library(magrittr)
library(data.table)

# Define make_aer_input output folder ------------------------------------------
print("Creating output/make_output output folder")

makeout_dir <- "output/make_output/"
fs::dir_create(here::here(makeout_dir))

# Specify arguments ------------------------------------------------------------
print('Specify arguments')

args <- commandArgs(trailingOnly = TRUE)

if (length(args) == 0) {
  output <- "table1" # the action to apply
  cohorts <- "prevax;vax;unvax" # The iterative label
} else {
  output <- args[[1]]
  cohorts <- args[[2]]
}
if (length(args) < 3) {
  subgroup <- "" # an optional subgroup label (e.g. preex_FALSE)
} else {
  subgroup <- args[[3]]
}

# Separate cohorts -------------------------------------------------------------
print('Separate cohorts')

cohorts <- stringr::str_split(as.vector(cohorts), ";")[[1]]

# Generate output saving string ------------------------------------------------
print('Generate saving')

if (subgroup == "All" | subgroup == "") {
  out_str <- ""
  load_str <- ""
} else {
  out_str <- paste0("_", subgroup) # trying to match previous make_other_output conventions, but could change to "-" for convenience
  load_str <- paste0("-", subgroup) # for loading in previous files
}

# Create blank table -----------------------------------------------------------
print('Create blank table')

df <- NULL

# Add output from each cohort --------------------------------------------------
print('Add output from each cohort')

for (i in cohorts) {
  # load input
  tmp <- readr::read_csv(paste0(
    "output/",
    output,
    "/",
    output,
    "-cohort_",
    i,
    load_str,
    "-midpoint6.csv"
  ))

  # create column for cohort
  tmp$cohort <- i

  # combine dataframes
  if (output == "table1") {
    # if there's a common column, combine by merging
    colnames(tmp)[-1:-2] <- paste0(colnames(tmp)[-1:-2], "_", i)
    if (i == cohorts[1]) {
      df <- tmp
    } else {
      df <- merge(
        df,
        tmp,
        by = c(colnames(df)[1:2]),
        all = T
      )
    }
  } else {
    # table2 processing
    df <- rbind(df, tmp, fill = TRUE)
  }
}

# add subgroup string ----------------------------------------------------------
print('Add subgroup string')

if (out_str != "") {
  df$subgroup <- subgroup
}

# Save output ------------------------------------------------------------------
print('Save output')

readr::write_csv(
  df,
  paste0(makeout_dir, "/", output, out_str, "_output_midpoint6.csv")
)
