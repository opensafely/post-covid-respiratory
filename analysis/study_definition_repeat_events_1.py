###################################################################################
# 
# This study definition extracts the first repeat_events_steps$upper[1] events
# for each individual (out_date_{name}_{n}), and counts the total number of events
# for each outcome type in the full follow-up period (out_n_{name})
# 
###################################################################################

## Set seed
import numpy as np
np.random.seed(123456)
#patient ID, vaccination dates, vaccination eligibility

# Cohort extractor
from tracemalloc import start
from cohortextractor import (
  StudyDefinition,
  patients,
  date_expressions,
  codelist_from_csv,
  codelist,
  filter_codes_by_category,
  combine_codelists,
  params
)
## Codelists from codelist.py (which pulls them from the codelist folder)
from codelists import *

## Variables for deriving JCVI groups
from grouping_variables import study_dates

from common_variables import clinical_event_date_X

study = StudyDefinition(

    # Configure the expectations framework
    default_expectations={
        "date": {"earliest": study_dates["earliest_expec"], "latest": "today"},
        "rate": "uniform",
        "incidence": 0.5,
    },

    # Define the study population 
     
    # NB: all inclusions and exclusions are performed in stage 1
    population = patients.which_exist_in_file("output/stage1_ids.csv"), 

    ## Number of recordings of the outcome in during the study period
    out_n_breathless=patients.with_these_clinical_events(
        breathlessness_snomed,
        returning="number_of_matches_in_period",
        between=[study_dates["pandemic_start"],study_dates["omicron_date"]],
        return_expectations={"int" : {"distribution": "poisson", "mean": 15}, "incidence" : 0.6}, 
        ),

    out_n_cough=patients.with_these_clinical_events(
        cough_snomed,
        returning="number_of_matches_in_period",
        between=[study_dates["pandemic_start"],study_dates["omicron_date"]],
        return_expectations={"int" : {"distribution": "poisson", "mean": 15}, "incidence" : 0.6},
        ),

    out_n_urti=patients.with_these_clinical_events(
        urti_snomed,
        returning="number_of_matches_in_period",
        between=[study_dates["pandemic_start"],study_dates["omicron_date"]],
        return_expectations={"int" : {"distribution": "poisson", "mean": 15}, "incidence" : 0.6},
        ),

    out_n_asthma_exac=patients.with_these_clinical_events(
        asthma_exacerbation_snomed,
        returning="number_of_matches_in_period",
        between=[study_dates["pandemic_start"],study_dates["omicron_date"]],
        return_expectations={"int" : {"distribution": "poisson", "mean": 15}, "incidence" : 0.6},
        ),

    out_n_copd_exac=patients.with_these_clinical_events(
        copd_exacerbation_snomed,
        returning="number_of_matches_in_period",
        between=[study_dates["pandemic_start"],study_dates["omicron_date"]],
        return_expectations={"int" : {"distribution": "poisson", "mean": 15}, "incidence" : 0.6},
        ),

    ## First 5 outcomes in the study period
    **clinical_event_date_X(
      name="breathless", 
      index_date=study_dates["pandemic_start"], 
      n=5,
    ),

    **clinical_event_date_X(
      name="cough", 
      index_date=study_dates["pandemic_start"], 
      n=5,
    ),

    **clinical_event_date_X(
      name="urti", 
      index_date=study_dates["pandemic_start"], 
      n=5,
    ),        

    **clinical_event_date_X(
      name="asthma_exac", 
      index_date=study_dates["pandemic_start"], 
      n=5,
    ),

    **clinical_event_date_X(
      name="copd_exac", 
      index_date=study_dates["pandemic_start"], 
      n=5,
    ),


)