# Load packages ----------------------------------------------------------------
print('Load packages')

library(magrittr)
library(data.table)

# Source common functions ------------------------------------------------------
print("Source common functions")

source("analysis/utility.R")

# Define make_aer_input output folder ------------------------------------------
print("Creating output/make_output output folder")

makeout_dir <- "output/make_output/"
fs::dir_create(here::here(makeout_dir))

# Specify arguments ------------------------------------------------------------
print('Specify arguments')

args <- commandArgs(trailingOnly = TRUE)

if (length(args) == 0) {
  output <- "table1"
  cohorts <- "prevax;vax;unvax"
} else {
  output <- args[[1]]
  cohorts <- args[[2]]
}

if (length(args) < 3) {
  subgroup <- "preex_TRUE"
} else {
  subgroup <- args[[3]]
}


# Separate cohorts -------------------------------------------------------------
print('Separate cohorts')

cohorts <- stringr::str_split(as.vector(cohorts), ";")[[1]]

# Create blank table -----------------------------------------------------------
print('Create blank table')

df <- NULL

# Add output from each cohort --------------------------------------------------
print('Add output from each cohort')

if (subgroup == "") {
  for (i in cohorts) {
    tmp <- readr::read_csv(paste0(
      "output/",
      output,
      "/",
      output,
      "-cohort_",
      i,
      "-midpoint6.csv"
    ))
    tmp$cohort <- i
    df <- rbind(df, tmp)
  }
  out_str <- ""
} else {
  if (length(subgroup) == 1) {
    for (i in cohorts) {
      tmp <- readr::read_csv(paste0(
        "output/",
        output,
        "/",
        output,
        "-cohort_",
        i,
        "-",
        subgroup,
        "-midpoint6.csv"
      ))
      tmp$cohort <- i
      df <- rbind(df, tmp)
    }
    out_str <- paste0("_", subgroup)
  } else {
    for (i in cohorts) {
      for (j in subgroup) {
        tmp <- readr::read_csv(paste0(
          "output/",
          output,
          "/",
          output,
          "-cohort_",
          i,
          "-",
          j,
          "-midpoint6.csv"
        ))
        tmp$cohort <- i
        df <- rbind(df, tmp)
      }
    }
    out_str <- list(glue("_{subgroup}")) ## CHECK
  }
}

# Save output ------------------------------------------------------------------
print('Save output')

readr::write_csv(
  df,
  paste0(makeout_dir, "/", output, out_str, "_output_midpoint6.csv")
)
