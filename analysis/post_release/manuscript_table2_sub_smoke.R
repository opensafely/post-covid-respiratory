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
        "unexposed_events",
        "exposed_events",
        "unexposed_person_days",
        "exposed_person_days"
    )
]

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

df$event_personyears_unexposed <- paste0(
    df$unexposed_events,
    "/",
    round(df$unexposed_person_days / 365.25)
)

df$event_personyears_exposed <- paste0(
    df$exposed_events,
    "/",
    round(df$exposed_person_days / 365.25)
)

df$incidencerate_unexposed <- round(
    df$unexposed_events / ((df$unexposed_person_days / 365.25) / 100000)
)

df$incidencerate_exposed <- round(
    df$exposed_events / ((df$exposed_person_days / 365.25) / 100000)
)

# Pivot exposure groups --------------------------------------------------------
print("Pivot exposure groups")

df <- tidyr::pivot_longer(
    df,
    cols = c(
        event_personyears_unexposed,
        event_personyears_exposed,
        incidencerate_unexposed,
        incidencerate_exposed
    ),
    names_to = c(".value", "exposure_group"),
    names_pattern = "(.*)_(unexposed|exposed)"
)

df$exposure_group <- dplyr::recode(
    df$exposure_group,
    "unexposed" = "No COVID-19",
    "exposed" = "COVID-19"
)

# Pivot table ------------------------------------------------------------------
print("Pivot table")

df <- df[, c(
    "analysis",
    "cohort",
    "outcome_label",
    "smoking_status",
    "exposure_group",
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
    "exposure_group",
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
    "Smoking status" = "smoking_status",
    "COVID-19" = "exposure_group"
)


# Save table -------------------------------------------------------------------
print("Save table")

readr::write_csv(df, paste0(output_folder, "/table2_smoking.csv"), na = "-")
