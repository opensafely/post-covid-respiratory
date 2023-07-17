# Import statements

## Set seed
import numpy as np
np.random.seed(123456)

## Cohort extractor
from cohortextractor import (
  StudyDefinition,
  patients,
  codelist_from_csv,
  codelist,
  filter_codes_by_category,
  combine_codelists,
  params,
)

## Codelists from codelist.py (which pulls them from the codelist folder)
from codelists import *

# import json module
import json

# import clinical_event_date_X function
from common_variables import clinical_event_date_X

# max_n for each cohort and outcome
with open("output/preprocess/max_events.json") as f:
   max_events = json.load(f)

#study_dates
with open("output/study_dates.json") as f:
  study_dates = json.load(f)

# define params
cohort=params["cohort"]

# extract repeat events
def out_date_n(name, cohort=cohort, max_events=max_events):
    # function for creating the out_date_5 variable
    def out_date_5(name, cohort):
        return {
            f"out_date_{name}_5": patients.with_value_from_file(
                f_path=f"output/preprocess/out_date_5_{cohort}.csv.gz",
                returning=f"out_date_{name}_5", 
                returning_type="date", 
                date_format='YYYY-MM-DD',
            )
        }
    variables=out_date_5(name, cohort)
    variables.update(
       clinical_event_date_X(
            name=name,
            index_date=f"out_date_{name}_5",
            n=int(max_events[cohort][name]),
            index_from=6,
       )
    )
    return variables

# define study definition
study = StudyDefinition(

    # Configure the expectations framework
    default_expectations={
        "date": {"earliest": study_dates["earliest_expec"], "latest": "today"},
        "rate": "uniform",
        "incidence": 0.5,
    },

    population=patients.which_exist_in_file(
        f_path=f"output/preprocess/out_date_5_{cohort}.csv.gz"
        ), 

    **out_date_n(name="breathless"),
    **out_date_n(name="asthma_exac"),
    **out_date_n(name="copd_exac"),
    **out_date_n(name="cough"),
    **out_date_n(name="urti"), 

)