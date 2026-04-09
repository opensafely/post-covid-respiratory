# Load data --------------------------------------------------------------------
print("Load data")

df <- readr::read_csv(
    path_table2_sub_smoking,
    show_col_types = FALSE
)

colnames(df) <- gsub("_midpoint6", "", colnames(df))

# Filter data ------------------------------------------------------------------
print("Filter data")

df <- df[
    grepl("sub_smoking", df$analysis),
    c(
        "cohort",
        "analysis",
        "outcome",
        "sample_size",
        "unexposed_events",
        "exposed_events",
        "unexposed_person_days",
        "exposed_person_days"
    )
]

df$events <- ifelse(
    grepl("main", df$analysis),
    df$unexposed_events,
    df$exposed_events
)
df$person_days <- ifelse(
    grepl("main", df$analysis),
    df$unexposed_person_days,
    df$exposed_person_days
)

df <- df[, c("cohort", "analysis", "outcome", "events", "person_days")]

# Add plot labels --------------------------------------------------------------
print("Add plot labels")

plot_labels <- readr::read_csv("lib/plot_labels.csv", show_col_types = FALSE)

df$outcome <- gsub("out_date_", "", df$outcome)
df <- merge(
    df,
    plot_labels[, c("term", "label")],
    by.x = "outcome",
    by.y = "term",
    all.x = TRUE
)
df <- dplyr::rename(df, "outcome_label" = "label")

df <- merge(
    df,
    plot_labels[, c("term", "label")],
    by.x = "analysis",
    by.y = "term",
    all.x = TRUE
)
df <- dplyr::rename(df, "smoking_status" = "label")

# Add other columns ------------------------------------------------------------
print("Add other columns")

df$event_personyears <- paste0(df$events, "/", round((df$person_days / 365.25)))
df$incidencerate <- round(df$events / ((df$person_days / 365.25) / 100000))

# Pivot table ------------------------------------------------------------------
print("Pivot table")

df <- df[, c(
    "analysis",
    "cohort",
    "outcome_label",
    "smoking_status",
    "event_personyears",
    "incidencerate"
)]

df$analysis <- gsub(".*(?=preex)", "", df$analysis, perl = TRUE)

# Define factor levels for sorting
df$outcome_label <- factor(
    df$outcome_label,
    levels = c(
        "Pneumonia",
        "Asthma",
        "Chronic obstructive pulmonary disease",
        "Interstitial lung disease"
    )
)

df$analysis <- factor(df$analysis, levels = c("preex_FALSE", "preex_TRUE"))

df$smoking_status <- factor(
    df$smoking_status,
    levels = c(
        "Never smoker",
        "Former smoker",
        "Current smoker"
    )
)

# Order the rows
df <- df[order(df$outcome_label, df$analysis, df$smoking_status), ]

df <- tidyr::pivot_wider(
    df,
    names_from = "cohort",
    values_from = c("event_personyears", "incidencerate")
)

# Reorder columns
column_order <- c(
    "analysis",
    "outcome_label",
    "smoking_status",
    "event_personyears_prevax",
    "incidencerate_prevax",
    "event_personyears_vax",
    "incidencerate_vax",
    "event_personyears_unvax",
    "incidencerate_unvax"
)

df <- df[, column_order]

# Tidy table -------------------------------------------------------------------
print("Tidy table")

df <- dplyr::rename(
    df,
    "Outcome" = "outcome_label",
    "Smoking status" = "smoking_status"
)


# Save table -------------------------------------------------------------------
print("Save table")

readr::write_csv(df, paste0(output_folder, "/table2_smoking.csv"), na = "-")

