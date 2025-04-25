# Load libraries ---------------------------------------------------------------
print('Load libraries')

library(readr)
library(dplyr)
library(magrittr)

# Define make_aer_input output folder ---------------------------------------------------------
print("Creating output/aer output folder")

aer_dir <- "output/aer/"
fs::dir_create(here::here(aer_dir))

# Specify redaction threshold --------------------------------------------------
print('Specify redaction threshold')

threshold <- 6

# Source common functions ------------------------------------------------------
print('Source common functions')

source("analysis/utility.R")

# Specify command arguments ----------------------------------------------------
print('Specify arguments')

args <- commandArgs(trailingOnly = TRUE)

if (length(args) == 0) {
    analysis <- "main"
} else {
    analysis <- args[[1]]
}

# Load active analyses ---------------------------------------------------------
print('Load active analyses')

active_analyses <- readr::read_rds("lib/active_analyses.rds")

# Format active analyses -------------------------------------------------------
print('Format active analyses')

active_analyses <- active_analyses[
    grepl(analysis, active_analyses$analysis),
]

active_analyses$outcome <- gsub("out_date_", "", active_analyses$outcome)

# Make empty AER input ---------------------------------------------------------
print('Make empty AER input')

input <- data.frame(
    aer_sex = character(),
    aer_age = character(),
    analysis = character(),
    cohort = character(),
    outcome = character(),
    unexposed_person_days = numeric(),
    unexposed_events = numeric(),
    total_exposed = numeric(),
    sample_size = numeric()
)

# Record number of events and person days for each active analysis -------------
print('Record number of events and person days for each active analysis')

for (i in 1:nrow(active_analyses)) {
    ## Load data -----------------------------------------------------------------
    print(paste0("Load data for ", active_analyses$name[i]))

    model_input <- read_rds(paste0(
        "output/model/model_input-",
        active_analyses$name[i],
        ".rds"
    ))
    model_input <- model_input[, c(
        "patient_id",
        "index_date",
        "exp_date",
        "out_date",
        "end_date_exposure",
        "end_date_outcome",
        "cov_cat_sex",
        "cov_num_age"
    )]

    for (sex in c("Female", "Male")) {
        for (age in c("18_39", "40_59", "60_79", "80_110")) {
            ## Identify AER groupings ------------------------------------------------
            print(paste0(
                "Identify AER groupings for sex: ",
                sex,
                "; ages: ",
                age
            ))

            min_age <- as.numeric(gsub("_.*", "", age))
            max_age <- as.numeric(gsub(".*_", "", age))

            ## Filter data -----------------------------------------------------------
            print("Filter data")

            df <- model_input[
                model_input$cov_cat_sex == sex &
                    model_input$cov_num_age >= as.numeric(min_age) &
                    model_input$cov_num_age < as.numeric(max_age),
            ]

            # ## Remove exposures and outcomes outside follow-up ----------------------- ! we have done this in make_model_input
            # print("Remove exposures and outcomes outside follow-up")

            # df <- df %>%
            #     dplyr::mutate(
            #         exposure = replace(
            #             exp_date,
            #             which(
            #                 exp_date > end_date_exposure | exp_date < index_date
            #             ),
            #             NA
            #         ),
            #         outcome = replace(
            #             out_date,
            #             which(
            #                 out_date > end_date_outcome | out_date < index_date
            #             ),
            #             NA
            #         )
            #     )

            ## Make exposed subset ---------------------------------------------------
            print('Make exposed subset')

            exposed <- df[
                !is.na(df$exp_date),
                c("patient_id", "exp_date", "end_date_outcome")
            ]

            exposed <- exposed %>%
                dplyr::mutate(
                    fup_start = exp_date,
                    fup_end = end_date_outcome
                )

            exposed <- exposed[exposed$fup_start <= exposed$fup_end, ]

            exposed <- exposed %>%
                dplyr::mutate(
                    person_days = as.numeric((fup_end - fup_start)) + 1
                )

            ## Make unexposed subset -------------------------------------------------
            print('Make unexposed subset')

            unexposed <- df[, c(
                "patient_id",
                "index_date",
                "exp_date",
                "out_date",
                "end_date_outcome"
            )]

            unexposed <- unexposed %>%
                dplyr::mutate(
                    fup_start = index_date,
                    fup_end = min(
                        exp_date - 1,
                        end_date_outcome,
                        na.rm = TRUE
                    ),
                    out_date = replace(out_date, which(out_date > fup_end), NA)
                )

            unexposed <- unexposed[unexposed$fup_start <= unexposed$fup_end, ]

            unexposed <- unexposed %>%
                dplyr::mutate(
                    person_days = as.numeric((fup_end - fup_start)) + 1
                )

            ## Append to AER input ---------------------------------------------------
            print('Append to AER input')

            input[nrow(input) + 1, ] <- c(
                aer_sex = sex,
                aer_age = age,
                analysis = active_analyses$analysis[i],
                cohort = active_analyses$cohort[i],
                outcome = active_analyses$outcome[i],
                unexposed_person_days = sum(unexposed$person_days),
                unexposed_events = nrow(unexposed[
                    !is.na(unexposed$out_date),
                ]),
                total_exposed = nrow(exposed),
                sample_size = nrow(df)
            )
        }
    }
}

# Save AER input ---------------------------------------------------------------
print('Save AER input')

write.csv(
    input,
    paste0(aer_dir, "aer_input-", analysis, ".csv"),
    row.names = FALSE
)

# Perform redaction ------------------------------------------------------------
print('Perform redaction')

input$unexposed_events_midpoint6 <- roundmid_any(
    as.numeric(input$unexposed_events),
    to = threshold
)
input$total_exposed_midpoint6 <- roundmid_any(
    as.numeric(input$total_exposed),
    to = threshold
)
input$sample_size_midpoint6 <- roundmid_any(
    as.numeric(input$sample_size),
    to = threshold
)
input[, c("unexposed_events", "total_exposed", "sample_size")] <- NULL

# Save rounded AER input -------------------------------------------------------
print('Save rounded AER input')

write.csv(
    input,
    paste0(aer_dir, "aer_input-", analysis, "-midpoint6.csv"),
    row.names = FALSE
)
