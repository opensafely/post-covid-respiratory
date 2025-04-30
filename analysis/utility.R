# Rounding function for redaction ----

roundmid_any <- function(x, to = 6) {
  # centers on (integer) midpoint of the rounding points
  x <- as.numeric(x)
  ceiling(x / to) * to - (floor(to / 2) * (x != 0))
}

# Function to make display numbers ----

display <- function(x, to = 1) {
  ifelse(
    x >= 100,
    sprintf("%.0f", x),
    ifelse(x >= 10, sprintf("%.1f", x), sprintf("%.2f", x))
  )
}

# Function for describing data ----

describe_data <- function(df, name) {
  fs::dir_create(here::here("output/describe/"))
  sink(paste0("output/describe/", name, ".txt"))
  print(Hmisc::describe(df))
  sink()
  message(paste0("output/describe/", name, ".txt written successfully."))
}

# Function for creating a median (iqr_low-iqr_high) string ----

create_median_iqr_string <- function(x)
  return(paste0(
    quantile(x)[3],
    " (",
    quantile(x)[2],
    "-",
    quantile(x)[4],
    ")"
  ))

# Function to convert numerical data to categorical data, following chosen bounds

numerical_to_categorical <- function(
  x,
  bounds = c(1, 100),
  zero_flag = FALSE,
  lower_limit = FALSE,
  upper_limit = FALSE,
  inclusive_bounds = FALSE
) {
  # x <- the numeric input vector
  # bounds <- a vector of bounds (must be ordered low->high)
  # zero_flag <- if TRUE, include an additional category for zero-values
  # lower_limit <- if TRUE, the first value in bounds is a hard lower bound
  #                if FALSE, create a category between 0 and the first value
  # upper_limit <- if TRUE, the last value in bounds is a hard upper bound
  #                if FALSE, create a category for greater than the last value
  # inclusive_bounds <- whether the bounds are inclusive or exclusive (assuming discrete values)
  #                     N.B. will assign borderline cases to upper boundary
  N <- length(bounds)
  gap <- ifelse(inclusive_bounds, 0, 1)
  y <- x

  if (!lower_limit) {
    y <- ifelse(x <= bounds[1] - gap, sprintf("<=%d", bounds[1] - gap), y)
  }
  if (zero_flag) {
    y <- ifelse(x == 0, sprintf("0"), y)
  }
  for (i1 in 1:(N - 1)) {
    y <- ifelse(
      x >= bounds[i1] & x <= bounds[i1 + 1] - gap,
      sprintf("%d-%d", bounds[i1], bounds[i1 + 1] - gap),
      y
    )
  }
  if (!upper_limit) {
    y <- ifelse(x >= bounds[N], sprintf("%d+", bounds[N]), y)
  }
  return(y)
}

# Function to verify dataframe formatting is correct

check_vitals <- function(df) {
  # Confirm patient ID is complete

  if (nrow(df) != nrow(df[!is.na(df$patient_id), ])) {
    stop("Patient ID is not present for everyone in the dataset")
  }

  # Confirm vital dates are present in dataset and have date format

  for (i in c("index_date", "exp_date", "out_date")) {
    if (!(i %in% colnames(df))) {
      stop(paste0(i, " is not in dataset"))
    } else {
      if (sapply(df[, i], lubridate::is.Date) == FALSE) {
        stop(paste0(i, " does not have a date format"))
      }
    }
  }

  # Confirm strat_cat_region is present in the dataset

  if (!("strat_cat_region" %in% colnames(df))) {
    stop("strat_cat_region is not in dataset")
  }

  # Confirm cov_bin_* variables are two level factors

  for (i in colnames(df)[grepl("cov_bin_", colnames(df))]) {
    if (sapply(df[, i], is.factor) == FALSE) {
      stop(paste0(i, " is not a factor"))
    } else {
      if (length(sapply(df[, i], levels)) != 2) {
        stop(paste0(
          i,
          " does not have two levels (levels = ",
          length(sapply(df[, i], levels)),
          ")"
        ))
      }
    }
  }

  # Confirm cov_cat_* variables are factors

  for (i in colnames(df)[grepl("cov_cat_", colnames(df))]) {
    if (sapply(df[, i], is.factor) == FALSE) {
      stop(paste0(i, " is not a factor"))
    }
  }
}

