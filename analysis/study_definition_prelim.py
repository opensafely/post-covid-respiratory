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
)
## Codelists from codelist.py (which pulls them from the codelist folder)
from codelists import *

## Datetime functions
from datetime import date

## Variables for deriving JCVI groups
from grouping_variables import (
    jcvi_variables, 
    study_dates,
    start_date,
    end_date,
    pandemic_start
)
import json
study = StudyDefinition(

    # Specify index date for study
    index_date = pandemic_start,

    # Configure the expectations framework
    default_expectations={
        "date": {"earliest": study_dates["earliest_expec"], "latest": "today"},
        "rate": "uniform",
        "incidence": 0.5,
    },

    # Define the study population 
     
    # NB: all inclusions and exclusions are performed in stage 1
    population = patients.all(
    ), 

# Define death date

        ## Primary care
        primary_care_death_date=patients.with_death_recorded_in_primary_care(
            on_or_after="index_date",
            returning="date_of_death",
            date_format="YYYY-MM-DD",
            return_expectations={
                "date": {"earliest": "index_date", "latest" : "today"},
                "rate": "exponential_increase",
            },
        ),
        ## ONS
        ons_died_from_any_cause_date=patients.died_from_any_cause(
            on_or_after="index_date",
            returning="date_of_death",
            date_format="YYYY-MM-DD",
            return_expectations={
                "date": {"earliest": "index_date", "latest" : "today"},
                "rate": "exponential_increase",
            },
        ),
        ## Combined
        death_date=patients.minimum_of(
            "primary_care_death_date", "ons_died_from_any_cause_date"
        ),
    
    # COVID-19 Vaccinations

        ## Any covid vaccination, identified by target disease
        vax_date_covid_1=patients.with_tpp_vaccination_record(
            target_disease_matches="SARS-2 CORONAVIRUS",
            on_or_after=study_dates["vax1_earliest"],
            find_first_match_in_period=True,
            returning="date",
            date_format="YYYY-MM-DD",
            return_expectations={
                "date": {"earliest": study_dates["pandemic_start"], "latest": "today"},
                "incidence": 0.7
            },
        ),
        vax_date_covid_2=patients.with_tpp_vaccination_record(
            target_disease_matches="SARS-2 CORONAVIRUS",
            on_or_after="vax_date_covid_1 + 1 day",
            find_first_match_in_period=True,
            returning="date",
            date_format="YYYY-MM-DD",
            return_expectations={
                "date": {"earliest": study_dates["pandemic_start"], "latest" : "today"}, # dates can only be 'index_date','today', or specified date
                "incidence": 0.6
            },
        ),

        vax_date_covid_3=patients.with_tpp_vaccination_record(
            target_disease_matches="SARS-2 CORONAVIRUS",
            on_or_after="vax_date_covid_2 + 1 day",
            find_first_match_in_period=True,
            returning="date",
            date_format="YYYY-MM-DD",
            return_expectations={
                "date": {"earliest": study_dates["pandemic_start"], "latest" : "today"}, # dates can only be 'index_date','today', or specified date
                "incidence": 0.5
            },
        ),

# Define sex 
    # NB: this is required for JCVI variables hence is defined here
        cov_cat_sex = patients.sex(
            return_expectations = {
            "rate": "universal",
            "category": {"ratios": {"M": 0.49, "F": 0.51}},
            }
        ),
    # Define vaccine eligibility variables

        **jcvi_variables,
        
    
)