# Load libraries ---------------------------------------------------------------
print('Load libraries')

library(magrittr)
library(tidyverse)
library(purrr)
library(data.table)
library(tidyverse)
library(svglite)
library(VennDiagram)
library(grid)
library(gridExtra)
# Load data --------------------------------------------------------------------
print("Load data")

df <- read_csv("output/post_release/plot_model_output.csv")

# Filter data ------------------------------------------------------------------
print("Filter data")

print("Filter data")

df_filtered <- df %>%
  filter(
    grepl("days", term), # keep terms containing "days"
    term == "days_pre", # then keep only days_pre
    is.na(model) | model == "mdl_age_sex" # drop this model
  ) %>%
  select(
    analysis,
    cohort,
    outcome,
    term,
    person_time_total,
    N_events_midpoint6
  )

print("Add incidence rate")

df_filtered <- df_filtered %>%
  mutate(
    event_personyears = paste0(
      N_events_midpoint6,
      "/",
      round((person_time_total / 365.25))
    ),
    incidencerate = round(
      N_events_midpoint6 / ((person_time_total / 365.25) / 100000)
    )
  ) %>%
  filter(grepl("^sub_smoking", analysis)) #filter to the subgroup of interest
