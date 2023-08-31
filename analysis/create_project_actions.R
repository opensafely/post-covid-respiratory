library(tidyverse)
library(yaml)
library(here)
library(glue)
library(readr)
library(dplyr)


###########################
# Load information to use #
###########################

## defaults ----
defaults_list <- list(
  version = "3.0",
  expectations= list(population_size=10000L)
)

source(here::here("analysis", "repeat_events", "repeat_events_steps.R"))
active_analyses <- read_rds("lib/active_analyses.rds")
active_analyses <- active_analyses[order(active_analyses$analysis,active_analyses$cohort,active_analyses$outcome),]
cohorts <- unique(active_analyses$cohort)

# Determine which outputs are ready --------------------------------------------

# success <- readxl::read_excel("C:/Users/hk19914/OneDrive - University of Bristol/Documents - grp-EHR/Projects/post-covid-outcome-tracker.xlsx",
#                                sheet = "respiratory",
#                                col_types = c("text", "text", "text", "text", "text",
#                                              "text", "text", "text", "text", "text",
#                                              "text", "text", "text", "text", "text",
#                                              "text", "text", "text", "text", "text",
#                                              "text", "skip", "skip"))

# success <- tidyr::pivot_longer(success,
#                                 cols = setdiff(colnames(success),c("outcome","cohort", "population")),
#                                 names_to = "analysis")

# success$name <- paste0("cohort_",success$cohort, "-",success$analysis, "-",success$outcome, "-", success$population)

# success <- success[grepl("success",success$value, ignore.case = TRUE),]

# create action functions ----

############################
## generic action function #
############################
action <- function(
  name,
  run,
  dummy_data_file=NULL,
  arguments=NULL,
  needs=NULL,
  highly_sensitive=NULL,
  moderately_sensitive=NULL
){
  
  outputs <- list(
    moderately_sensitive = moderately_sensitive,
    highly_sensitive = highly_sensitive
  )
  outputs[sapply(outputs, is.null)] <- NULL
  
  action <- list(
    run = paste(c(run, arguments), collapse=" "),
    dummy_data_file = dummy_data_file,
    needs = needs,
    outputs = outputs
  )
  action[sapply(action, is.null)] <- NULL
  
  action_list <- list(name = action)
  names(action_list) <- name
  
  action_list
}


## create comment function ----
comment <- function(...){
  list_comments <- list(...)
  comments <- map(list_comments, ~paste0("## ", ., " ##"))
  comments
}


## create function to convert comment "actions" in a yaml string into proper comments
convert_comment_actions <-function(yaml.txt){
  yaml.txt %>%
    str_replace_all("\\\n(\\s*)\\'\\'\\:(\\s*)\\'", "\n\\1")  %>%
    #str_replace_all("\\\n(\\s*)\\'", "\n\\1") %>%
    str_replace_all("([^\\'])\\\n(\\s*)\\#\\#", "\\1\n\n\\2\\#\\#") %>%
    str_replace_all("\\#\\#\\'\\\n", "\n")
}


 #################################################
 ## Function for typical actions to analyse data #
 #################################################
 # Updated to a typical action running Cox models for one outcome
#  apply_model_function <- function(name, cohort, analysis, ipw, strata, 
#                                  covariate_sex, covariate_age, covariate_other, 
#                                  cox_start, cox_stop, study_start, study_stop,
#                                  cut_points, controls_per_case,
#                                  total_event_threshold, episode_event_threshold,
#                                  covariate_threshold, age_spline){
  
#   splice(
#     action(
#       name = glue("make_model_input-{name}"),
#       run = glue("r:latest analysis/model/make_model_input.R {name}"),
#       needs = list("stage1_data_cleaning_all"),
#       highly_sensitive = list(
#         model_input = glue("output/model_input-{name}.rds")
#       )
#     ),

#   action(
#       name = glue("cox_ipw-{name}"),
#       run = glue("cox-ipw:v0.0.21 --df_input=model_input-{name}.rds --ipw={ipw} --exposure=exp_date --outcome=out_date --strata={strata} --covariate_sex={covariate_sex} --covariate_age={covariate_age} --covariate_other={covariate_other} --cox_start={cox_start} --cox_stop={cox_stop} --study_start={study_start} --study_stop={study_stop} --cut_points={cut_points} --controls_per_case={controls_per_case} --total_event_threshold={total_event_threshold} --episode_event_threshold={episode_event_threshold} --covariate_threshold={covariate_threshold} --age_spline={age_spline} --df_output=model_output-{name}.csv"),
#       needs = list(glue("make_model_input-{name}")),
#       moderately_sensitive = list(
#         model_output = glue("output/model_output-{name}.csv"))
#     )
#   )
# }

# Create function to make Table 2 ----------------------------------------------

# table2 <- function(cohort){
  
#   table2_names <- gsub("out_date_","",unique(active_analyses[active_analyses$cohort=={cohort},]$name))
  
#   splice(
#     comment(glue("Table 2 - {cohort}")),
#     action(
#       name = glue("table2_{cohort}"),
#       run = "r:latest analysis/table2.R",
#       arguments = c(cohort),
#       needs = c(as.list(paste0("make_model_input-",table2_names))),
#       moderately_sensitive = list(
#         table2 = glue("output/table2_{cohort}.csv"),
#         table2_rounded = glue("output/table2_{cohort}_rounded.csv")
#       )
#     )
#   )
# }


##########################################################
## Define and combine all actions into a list of actions #
##########################################################
actions_list <- splice(

  comment("# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #",
          "DO NOT EDIT project.yaml DIRECTLY",
          "This file is created by create_project_actions.R",
          "Edit and run create_project_actions.R to update the project.yaml",
          "# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #"
  ),
  
  comment("Generate vaccination eligibility information"),
  action(
    name = glue("vax_eligibility_inputs"),
    run = "r:latest analysis/metadates.R",
    highly_sensitive = list(
      study_dates_json = glue("output/study_dates.json"),
      vax_jcvi_groups= glue("output/vax_jcvi_groups.csv"),
      vax_eligible_dates= ("output/vax_eligible_dates.csv")
    )
  ),

  comment("Generate dummy data for study_definition - population_prelim"),
  action(
    name = "generate_study_population_prelim",
    run = "cohortextractor:latest generate_cohort --study-definition study_definition_prelim --output-format feather",
    needs = list("vax_eligibility_inputs"),
    highly_sensitive = list(
      cohort = glue("output/input_prelim.feather")
    )
  ),
  
  comment("Generate dates for all study cohorts"),
  action(
    name = "generate_index_dates",
    run = "r:latest analysis/prelim.R",
    needs = list("vax_eligibility_inputs","generate_study_population_prelim"),
    highly_sensitive = list(
      index_dates = glue("output/index_dates.csv")
    )
  ),

  comment("Generate dummy data for study_definition - prevax"),
  action(
    name = "generate_study_population_prevax",
    run = "cohortextractor:latest generate_cohort --study-definition study_definition_prevax --output-format csv.gz",
    needs = list("vax_eligibility_inputs","generate_index_dates"),
    highly_sensitive = list(
      cohort = glue("output/input_prevax.csv.gz")
    )
  ),

  comment("Generate dummy data for study_definition - vax"),
  action(
    name = "generate_study_population_vax",
    run = "cohortextractor:latest generate_cohort --study-definition study_definition_vax --output-format csv.gz",
    needs = list("generate_index_dates","vax_eligibility_inputs"),
    highly_sensitive = list(
      cohort = glue("output/input_vax.csv.gz")
    )
  ),

  comment("Generate dummy data for study_definition - unvax"),
  action(
    name = "generate_study_population_unvax",
    run = "cohortextractor:latest generate_cohort --study-definition study_definition_unvax --output-format csv.gz",
    needs = list("vax_eligibility_inputs","generate_index_dates"),
    highly_sensitive = list(
      cohort = glue("output/input_unvax.csv.gz")
    )
  ),

  comment("Preprocess data -prevax"),
  action(
    name = "preprocess_data_prevax",
    run = "r:latest analysis/preprocess/preprocess_data.R prevax",
    needs = list( "generate_index_dates","generate_study_population_prevax"),
    moderately_sensitive = list(
      describe = glue("output/not-for-review/describe_input_prevax_stage0.txt"),
      describe_venn = glue("output/not-for-review/describe_venn_prevax.txt")
    ),
    highly_sensitive = list(
      cohort = glue("output/input_prevax.rds"),
      venn = glue("output/venn_prevax.rds")
    )
  ),
  
  comment("Preprocess data - vax"),
  action(
    name = "preprocess_data_vax",
    run = "r:latest analysis/preprocess/preprocess_data.R vax",
    needs = list("generate_index_dates","generate_study_population_vax"),
    moderately_sensitive = list(
      describe = glue("output/not-for-review/describe_input_vax_stage0.txt"),
      descrive_venn = glue("output/not-for-review/describe_venn_vax.txt")
    ),
    highly_sensitive = list(
      cohort = glue("output/input_vax.rds"),
      venn = glue("output/venn_vax.rds")
    )
  ), 
  
  comment("Preprocess data -unvax"),
  action(
    name = "preprocess_data_unvax",
    run = "r:latest analysis/preprocess/preprocess_data.R unvax",
    needs = list("generate_index_dates", "generate_study_population_unvax"),
    moderately_sensitive = list(
      describe = glue("output/not-for-review/describe_input_unvax_stage0.txt"),
      describe_venn = glue("output/not-for-review/describe_venn_unvax.txt")
    ),
    highly_sensitive = list(
      cohort = glue("output/input_unvax.rds"),
      venn = glue("output/venn_unvax.rds")
    )
  ),

  comment("Stage 1 - Data cleaning - prevax"),
  action(
    name = "stage1_data_cleaning_prevax",
    run = "r:latest analysis/preprocess/Stage1_data_cleaning.R prevax",
    needs = list("preprocess_data_prevax","vax_eligibility_inputs"),
    moderately_sensitive = list(
      refactoring = glue("output/not-for-review/meta_data_factors_prevax.csv"),
      QA_rules = glue("output/review/descriptives/QA_summary_prevax.csv"),
      IE_criteria = glue("output/review/descriptives/Cohort_flow_prevax.csv"),
      histograms = glue("output/not-for-review/numeric_histograms_prevax.svg")
    ),
    highly_sensitive = list(
      cohort = glue("output/input_prevax_stage1.rds")
    )
  ),

  action(
    name = glue("describe_file-input_prevax_stage1"),
    run = glue("r:latest analysis/describe_file.R input_prevax_stage1 rds"),
    needs = list("stage1_data_cleaning_prevax"),
    moderately_sensitive = list(
      describe_model_input = glue("output/describe-input_prevax_stage1.txt")
    )
  ),

  comment("Stage 1 - Data cleaning - vax"),
  action(
    name = "stage1_data_cleaning_vax",
    run = "r:latest analysis/preprocess/Stage1_data_cleaning.R vax",
    needs = list("preprocess_data_vax","vax_eligibility_inputs"),
    moderately_sensitive = list(
      refactoring = glue("output/not-for-review/meta_data_factors_vax.csv"),
      QA_rules = glue("output/review/descriptives/QA_summary_vax.csv"),
      IE_criteria = glue("output/review/descriptives/Cohort_flow_vax.csv"),
      histograms = glue("output/not-for-review/numeric_histograms_vax.svg")
    ),
    highly_sensitive = list(
      cohort = glue("output/input_vax_stage1.rds")
    )
  ),

  action(
    name = glue("describe_file-input_vax_stage1"),
    run = glue("r:latest analysis/describe_file.R input_vax_stage1 rds"),
    needs = list("stage1_data_cleaning_vax"),
    moderately_sensitive = list(
      describe_model_input = glue("output/describe-input_vax_stage1.txt")
    )
  ),

  comment("Stage 1 - Data cleaning - unvax"),
  action(
    name = "stage1_data_cleaning_unvax",
    run = "r:latest analysis/preprocess/Stage1_data_cleaning.R unvax",
    needs = list("preprocess_data_unvax","vax_eligibility_inputs"),
    moderately_sensitive = list(
      refactoring = glue("output/not-for-review/meta_data_factors_unvax.csv"),
      QA_rules = glue("output/review/descriptives/QA_summary_unvax.csv"),
      IE_criteria = glue("output/review/descriptives/Cohort_flow_unvax.csv"),
      histograms = glue("output/not-for-review/numeric_histograms_unvax.svg")
    ),
    highly_sensitive = list(
      cohort = glue("output/input_unvax_stage1.rds")
    )
  ),

  action(
    name = glue("describe_file-input_unvax_stage1"),
    run = glue("r:latest analysis/describe_file.R input_unvax_stage1 rds"),
    needs = list("stage1_data_cleaning_unvax"),
    moderately_sensitive = list(
      describe_model_input = glue("output/describe-input_unvax_stage1.txt")
    )
  ),

  comment("Stage 1 IDs"),
  action(
    name = "create_stage1_ids",
    run = glue("r:latest analysis/preprocess/Stage1_ids.R"),
    needs = list("stage1_data_cleaning_prevax", "stage1_data_cleaning_vax", "stage1_data_cleaning_unvax"),
    highly_sensitive = list(
      stage1_ids = glue("output/stage1_ids.csv")
    )
  ),

  comment("Table1 - prevax"),
  action(
    name = "table1_prevax",
    run = "r:latest analysis/descriptives/table1.R prevax",
    needs = list("stage1_data_cleaning_prevax"),
    moderately_sensitive = list(
      table1 = glue("output/table1_prevax.csv"),
      table1_rounded = glue("output/table1_prevax_rounded.csv")
      )
  ),

  comment("Table1 - vax"),
  action(
    name = "table1_vax",
    run = "r:latest analysis/descriptives/table1.R vax",
    needs = list("stage1_data_cleaning_vax"),
    moderately_sensitive = list(
      table1 = glue("output/table1_vax.csv"),
      table1_rounded = glue("output/table1_vax_rounded.csv")
      )
  ),

  comment("Table1 - unvax"),
  action(
    name = "table1_unvax",
    run = "r:latest analysis/descriptives/table1.R unvax",
    needs = list("stage1_data_cleaning_unvax"),
    moderately_sensitive = list(
      table1 = glue("output/table1_unvax.csv"),
      table1_rounded = glue("output/table1_unvax_rounded.csv")
      )
  ),

  unlist(
    lapply(
      repeat_events_steps$step,
      function (x)
      {
        
        studydef_needs <- list("vax_eligibility_inputs", "generate_index_dates", "create_stage1_ids")
        preflight_needs <- list(glue("generate_study_population_repeat_events_{x}"))
        preflight_high_sens <- list(
          out_date = glue("output/repeat_events/out_date_{x+1}.csv.gz")
        )
        if (x==1) {
          studydef_file <- "study_definition_repeat_events_1"
          params <- ""
          preflight_high_sens <- splice(
            preflight_high_sens,
            patient_ids = "output/repeat_events/patient_ids_*.rds"
          )
          preflight_mod_sens <- list(
            max_events = glue("output/repeat_events/max_events.json")
            )
        } else {
          studydef_file <- "study_definition_repeat_events_x"
          params <- " --param step={x}"
          studydef_needs <- splice(unique(c(
            studydef_needs,
            "preflight_repeat_events_2",
            glue("preflight_repeat_events_{x}")
          )))
          preflight_needs <- splice(
            preflight_needs,
            "preflight_repeat_events_2"
          )
          preflight_mod_sens <- NULL
        }
        
        actions <- 
          splice(
            comment(glue("Study_definition - repeat events step {x}")),
            action(
              name = glue("generate_study_population_repeat_events_{x}"),
              run = glue(
                "cohortextractor:latest generate_cohort",
                " --study-definition {studydef_file}",
                " --output-file output/repeat_events/input_repeat_events_{x}.csv.gz",
                params
                ),
              needs = studydef_needs,
              highly_sensitive = list(
                cohort = glue("output/repeat_events/input_repeat_events_{x}.csv.gz")
              )
            )
          )
        
        if (x < max(repeat_events_steps$step)) {
          actions <- splice(
            actions,
            comment(glue("Preflight - repeat events step {x+1}")),
            action(
              name = glue("preflight_repeat_events_{x+1}"),
              run = "r:latest analysis/repeat_events/preflight_repeat_events.R",
              arguments = x+1,
              needs = preflight_needs,
              highly_sensitive = preflight_high_sens,
              moderately_sensitive = preflight_mod_sens
            )
          )
        }
        
        return(actions)
      }
    ),
    recursive = FALSE
  ),

  comment("Join repeat events"),
  action(
    name = "join_repeat_events",
    run = glue("r:latest analysis/repeat_events/join_repeat_events.R"),
    needs = list("create_stage1_ids",
                 "generate_study_population_repeat_events_1",
                 "preflight_repeat_events_2",
                 "generate_study_population_repeat_events_2",
                 "generate_study_population_repeat_events_3"),
    highly_sensitive = list(
      repeat_events = glue("output/repeat_events/data_repeat_events_long.rds")
    )
  ),

  comment("Process repeat events - prevax"),
  action(
    name = "process_repeat_events_prevax",
    run = glue("r:latest analysis/repeat_events/process_repeat_events.R prevax"),
    needs = list("stage1_data_cleaning_prevax",
                 "join_repeat_events"),
    highly_sensitive = list(
      repeat_events = glue("output/repeat_events/repeat_events_prevax_*.csv")
    )
  ),

  comment("Process repeat events - vax"),
  action(
    name = "process_repeat_events_vax",
    run = glue("r:latest analysis/repeat_events/process_repeat_events.R vax"),
    needs = list("stage1_data_cleaning_vax",
                 "join_repeat_events"),
    highly_sensitive = list(
      repeat_events = glue("output/repeat_events/repeat_events_vax_*.csv")
    )
  ),

  comment("Process repeat events - unvax"),
  action(
    name = "process_repeat_events_unvax",
    run = glue("r:latest analysis/repeat_events/process_repeat_events.R unvax"),
    needs = list("stage1_data_cleaning_unvax",
                 "join_repeat_events"),
    highly_sensitive = list(
      repeat_events = glue("output/repeat_events/repeat_events_unvax_*.csv")
    )
  )





  # comment("Stage 5 - Run models"),
  # splice(
  #   # over outcomes
  #   unlist(lapply(1:nrow(active_analyses), 
  #                 function(x) apply_model_function(name = active_analyses$name[x],
  #                                                  cohort = active_analyses$cohort[x],
  #                                                  analysis = active_analyses$analysis[x],
  #                                                  ipw = active_analyses$ipw[x],
  #                                                  strata = active_analyses$strata[x],
  #                                                  covariate_sex = active_analyses$covariate_sex[x],
  #                                                  covariate_age = active_analyses$covariate_age[x],
  #                                                  covariate_other = active_analyses$covariate_other[x],
  #                                                  cox_start = active_analyses$cox_start[x],
  #                                                  cox_stop = active_analyses$cox_stop[x],
  #                                                  study_start = active_analyses$study_start[x],
  #                                                  study_stop = active_analyses$study_stop[x],
  #                                                  cut_points = active_analyses$cut_points[x],
  #                                                  controls_per_case = active_analyses$controls_per_case[x],
  #                                                  total_event_threshold = active_analyses$total_event_threshold[x],
  #                                                  episode_event_threshold = active_analyses$episode_event_threshold[x],
  #                                                  covariate_threshold = active_analyses$covariate_threshold[x],
  #                                                  age_spline = active_analyses$age_spline[x])), recursive = FALSE
  #   )
  # ),

  # comment("Make Table 2"),
  # splice(
  #   unlist(lapply(unique(active_analyses$cohort), 
  #                 function(x) table2(cohort = x)), 
  #          recursive = FALSE
  #   )
  # ),

  # comment("Stage 6 - make model output"),
  # action(
  #   name = "make_model_output",
  #   run = "r:latest analysis/model/make_model_output.R",
  #   needs = as.list(paste0("cox_ipw-",success$name)),
  #   moderately_sensitive = list(
  #     model_output = glue("output/model_output.csv")
  #   )
  # )


)

## combine everything ----
project_list <- splice(
  defaults_list,
  list(actions = actions_list)
)
  
#####################################################################################
## convert list to yaml, reformat comments and white space, and output a .yaml file #
#####################################################################################
as.yaml(project_list, indent=2) %>%
  # convert comment actions to comments
  convert_comment_actions() %>%
  # add one blank line before level 1 and level 2 keys
  str_replace_all("\\\n(\\w)", "\n\n\\1") %>%
  str_replace_all("\\\n\\s\\s(\\w)", "\n\n  \\1") %>%
  writeLines("project.yaml")
