source("analysis/post_release/fn-lifetable.R")
# Define post_release output folder ------------------------------------------
output_folder <- "output/post_release" # Folder to save the transformed datasets

# Ensure output folder exists
if (!dir.exists(output_folder)) {
    dir.create(output_folder)
}

# Specify parameters -----------------------------------------------------------
print('Specify parameters')

analysis_list <- c("main_preex_FALSE", "main_preex_TRUE")

for (analysis in analysis_list) {
    print(paste("Running analysis:", analysis))

    # Load model output ------------------------------------------------------------
    print('Load model output')

    model_output <- read_csv("output/plot_model_output.csv")

    # Format and restrict to relevant models ---------------------------------------
    print('Restrict to relevant models')

    model_output <- model_output[, c(
        "outcome",
        "cohort",
        "analysis",
        "model",
        "term",
        "hr"
    )]
    model_output$hr <- as.numeric(model_output$hr)

    model_output <- model_output[
        stringr::str_detect(model_output$term, "^days") &
            model_output$analysis == analysis &
            model_output$model == "mdl_max_adj" &
            model_output$hr != "[Redacted]" &
            !is.na(model_output$hr),
    ]

    # Add start and end for time periods to model output ---------------------------
    print('Add start and end for time periods to model output')

    model_output$time_period_start <- as.numeric(gsub(
        "_.*",
        "",
        gsub("days", "", model_output$term)
    ))
    model_output$time_period_end <- as.numeric(gsub(
        ".*_",
        "",
        model_output$term
    ))

    # Load AER input ---------------------------------------------------------------
    print('Load AER input')

    aer_input <- read_csv(path_aer_input)
    aer_input <- aer_input[aer_input$analysis == analysis, ]

    # Run AER function -------------------------------------------------------------
    print('Run AER function')

    lifetables_compiled <- NULL

    for (i in 1:nrow(aer_input)) {
        if (
            nrow(model_output[
                model_output$outcome == aer_input$outcome[i] &
                    model_output$cohort == aer_input$cohort[i] &
                    model_output$analysis == aer_input$analysis[i],
            ]) >
                0
        ) {
            tmp <- lifetable(
                model_output = model_output,
                aer_input = aer_input[i, ],
                day0 = TRUE
            )

            tmp$day0 <- TRUE
            lifetables_compiled <- rbind(lifetables_compiled, tmp)

            tmp <- lifetable(
                model_output = model_output,
                aer_input = aer_input[i, ],
                day0 = FALSE
            )

            tmp$day0 <- FALSE
            lifetables_compiled <- rbind(lifetables_compiled, tmp)
        }
    }

    # Calculate prevax weightings --------------------------------------------------
    print('Calculate prevax weightings')

    prevax_weightings <- aer_input[
        aer_input$cohort == "prevax",
        c("analysis", "outcome", "aer_sex", "aer_age", "sample_size_midpoint6")
    ]

    prevax_weightings$weight <- prevax_weightings$sample_size_midpoint6 /
        sum(prevax_weightings$sample_size_midpoint6)
    prevax_weightings$sample_size_midpoint6 <- NULL

    # Calculate overall AER --------------------------------------------------------
    print('Calculate overall AER')

    lifetable_overall <- lifetables_compiled[, c(
        "analysis",
        "outcome",
        "cohort",
        "days",
        "aer_age",
        "aer_sex",
        "cumulative_difference_absolute_excess_risk",
        "day0"
    )]

    lifetable_overall <- merge(
        lifetable_overall,
        prevax_weightings,
        by = c("analysis", "outcome", "aer_sex", "aer_age")
    )

    lifetable_overall <- lifetable_overall %>%
        dplyr::group_by(analysis, outcome, cohort, days, day0) %>%
        dplyr::mutate(
            cumulative_difference_absolute_excess_risk = weighted.mean(
                cumulative_difference_absolute_excess_risk,
                weight
            )
        ) %>%
        dplyr::ungroup() %>%
        dplyr::select(
            analysis,
            outcome,
            cohort,
            days,
            day0,
            cumulative_difference_absolute_excess_risk
        ) %>%
        unique

    lifetable_overall$aer_sex <- "overall"
    lifetable_overall$aer_age <- "overall"

    # Compile aer_group and overall life tables -------------------------------------
    print('Compile aer_group and overall life tables')

    lifetables_compiled <- lifetables_compiled[, c(
        "analysis",
        "outcome",
        "cohort",
        "days",
        "day0",
        "aer_age",
        "aer_sex",
        "cumulative_difference_absolute_excess_risk"
    )]

    lifetables_compiled <- rbind(lifetables_compiled, lifetable_overall)

    # Save compiled life tables ----------------------------------------------------
    print('Save compiled life tables')

    write.csv(
        lifetables_compiled,
        paste0(
            output_folder,
            "/lifetables_compiled_",
            gsub("main_", "", analysis),
            ".csv"
        ),
        row.names = FALSE
    )
}
