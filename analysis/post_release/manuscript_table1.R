# Load data --------------------------------------------------------------------
print("Load data")

df <- readr::read_csv(path_table1, show_col_types = FALSE)

# Clean column names: remove brackets
colnames(df) <- gsub(" \\[.*?\\]", "", colnames(df))

# Collapse IMD deciles to quintiles ---------------------------------------
collapse_imd_quintiles <- function(df) {
  # Identify IMD rows
  imd_df <- df[df$Characteristic == "cov_cat_imd", ]
  all_row <- df[df$Characteristic == "All", ]

  # Extract decile number
  imd_df$imd_decile <- readr::parse_number(imd_df$Subcharacteristic)

  # Create quintiles
  imd_df$imd_quintile <- ceiling(imd_df$imd_decile / 2)

  # Label
  imd_df$Subcharacteristic <- ifelse(
    imd_df$Subcharacteristic == "All",
    "All",
    paste0("Q", imd_df$imd_quintile)
  )

  # Identify column types
  n_cols <- grep("^N_", names(imd_df), value = TRUE)
  pct_cols <- grep("^\\(\\%\\)_", names(imd_df), value = TRUE)
  diag_cols <- grep("^COVID-19 diagnoses", names(imd_df), value = TRUE)
  imd_df[n_cols] <- lapply(imd_df[n_cols], function(x) {
    as.numeric(gsub(",", "", x))
  })
  imd_df[diag_cols] <- lapply(imd_df[diag_cols], function(x) {
    as.numeric(gsub(",", "", x))
  })

  # Aggregate
  imd_df <- imd_df |>
    dplyr::group_by(Characteristic, Subcharacteristic) |>
    dplyr::summarise(
      across(all_of(n_cols), ~ sum(.x, na.rm = TRUE)),
      across(all_of(diag_cols), ~ sum(.x, na.rm = TRUE)),
      .groups = "drop"
    )
  # Get denominators
  all_values <- all_row[1, n_cols]

  # recompute %
  for (col in n_cols) {
    pct_name <- sub("^N_", "(%)_", col)

    imd_df[[pct_name]] <- round(
      100 * imd_df[[col]] / as.numeric(all_values[[col]]),
      1
    )
  }

  # Convert columns to keep consistent with original df
  imd_df[pct_cols] <- lapply(imd_df[pct_cols], function(x) {
    paste0(round(x, 1), "%")
  })

  imd_df <- dplyr::mutate(
    imd_df,
    across(
      names(df),
      ~ {
        target_class <- class(df[[cur_column()]])[1]

        if (target_class == "character") {
          as.character(.x)
        } else if (target_class %in% c("numeric", "double")) {
          as.numeric(.x)
        } else {
          .x
        }
      }
    )
  )

  # Remove old IMD rows and bind new ones
  df <- df[df$Characteristic != "cov_cat_imd", ]
  df <- dplyr::bind_rows(df, imd_df)

  return(df)
}

df <- collapse_imd_quintiles(df)

# Order Characteristics ----------------------------------------------------------------
print("Order characteristics")
characteristic_order <- c(
  "All",
  "cov_cat_sex",
  "cov_cat_age_group",
  "cov_cat_ethnicity",
  "cov_cat_imd",
  "cov_cat_smoking",
  "strat_cat_region",
  "cov_cat_consrate2019",
  "cov_bin_carehome",
  "cov_bin_hcworker",
  "cov_bin_asthma",
  "cov_bin_copd",
  "cov_bin_ami",
  "cov_bin_cancer",
  "cov_bin_ckd",
  "cov_bin_dementia",
  "cov_bin_depression",
  "cov_bin_diabetes",
  "cov_bin_hypertension",
  "cov_bin_liver_disease",
  "cov_bin_obesity",
  "cov_bin_ild",
  "cov_bin_pneumonia",
  "cov_bin_stroke_isch",
  "Age, years"
)

df$Characteristic <- factor(
  df$Characteristic,
  levels = characteristic_order
)

# Subcharacteristics ----------------------------------------------------------------
print("Order subcharacteristics")
ethnicity_order <- c(
  "Black",
  "Asian",
  "White",
  "Other",
  "Mixed",
  "Missing"
)

smoking_order <- c(
  "Never smoker",
  "Ever smoker",
  "Current smoker",
  "Missing"
)

df$Subcat_order[df$Characteristic == "cov_cat_ethnicity"] <-
  factor(
    df$Subcharacteristic[df$Characteristic == "cov_cat_ethnicity"],
    levels = ethnicity_order
  )

df$Subcat_order[df$Characteristic == "cov_cat_smoking"] <-
  factor(
    df$Subcharacteristic[df$Characteristic == "cov_cat_smoking"],
    levels = smoking_order
  )

df <- df[order(df$Characteristic, df$Subcat_order), ]
df$Subcat_order <- NULL

# Define ID columns
id_vars <- c("Characteristic", "Subcharacteristic")

# Subset preex_FALSE and preex_TRUE columns ------------------------------------
print("Split tables")

preex_false_cols <- grep("preex_FALSE$", names(df), value = TRUE)
preex_true_cols <- grep("preex_TRUE$", names(df), value = TRUE)

df_false <- df[, c(id_vars, preex_false_cols)]
df_true <- df[, c(id_vars, preex_true_cols)]

# Clean column names to standard form ------------------------------------------
clean_names <- function(df, preex_flag) {
  names(df) <- gsub(paste0("-", preex_flag, "$"), "", names(df)) # Remove suffix
  return(df)
}

df_false <- clean_names(df_false, "preex_FALSE")
df_true <- clean_names(df_true, "preex_TRUE")

# Combine N and % columns ------------------------------------------------------
combine_n_pct <- function(df) {
  for (group in c("prevax", "vax", "unvax")) {
    n_col <- paste0("N_", group)
    pct_col <- paste0("(%)_", group)
    combined_col <- paste0("N (%)_", group)

    if (all(c(n_col, pct_col) %in% names(df))) {
      df[[combined_col]] <- ifelse(
        tolower(df$Subcharacteristic) %in% c("all", "median (iqr)"),
        df[[n_col]], # show only N
        paste0(df[[n_col]], " ", df[[pct_col]]) # combine N and (%)
      )
    }
  }

  # Drop original N and (%) columns
  df <- df[,
    !names(df) %in%
      c(
        paste0("N_", c("prevax", "vax", "unvax")),
        paste0("(%)_", c("prevax", "vax", "unvax"))
      )
  ]
  return(df)
}

df_false <- combine_n_pct(df_false)
df_true <- combine_n_pct(df_true)

# Reorder columns to match: N (%), Diagnoses per cohort ------------------------
reorder_cols <- function(df) {
  df[, c(
    "Characteristic",
    "Subcharacteristic",
    "N (%)_prevax",
    "COVID-19 diagnoses_prevax",
    "N (%)_vax",
    "COVID-19 diagnoses_vax",
    "N (%)_unvax",
    "COVID-19 diagnoses_unvax"
  )]
}

df_false <- reorder_cols(df_false)
df_true <- reorder_cols(df_true)

# Save output ------------------------------------------------------------------
print("Save tables")

readr::write_csv(
  df_false,
  paste0(output_folder, "/table1_preex_false.csv"),
  na = "-"
)
readr::write_csv(
  df_true,
  paste0(output_folder, "/table1_preex_true.csv"),
  na = "-"
)
