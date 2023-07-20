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
  params
)

## Codelists from codelist.py (which pulls them from the codelist folder)
from codelists import *

# import json module
import json

# import clinical_event_date_X function
from common_variables import clinical_event_date_X

# max_n for each outcome
with open("output/repeat_events/max_events.json") as f:
   max_events = json.load(f)

#study_dates
with open("output/study_dates.json") as f:
  study_dates = json.load(f)

# params
n_min=int(params["n_min"])
n_max=int(params["n_max"])
index_event = n_min - 1

# extract repeat events
def out_date_n(
      name, 
      index_event=index_event,
      n_min=n_min, 
      n_max=n_max, 
      max_events=max_events
      ):
    # redefine n_max to be the maximum of n_max and max_events["name"]
    n_max = min(n_max, int(max_events[name]))
    # function for creating the out_date_5 variable
    def out_date_index(name, index_event):
        return {
            f"out_date_{name}_{index_event}": patients.with_value_from_file(
                f_path=f"output/repeat_events/out_date_{index_event}.csv.gz",
                returning=f"out_date_{name}_{index_event}", 
                returning_type="date", 
                date_format='YYYY-MM-DD',
            )
        }
    variables=out_date_index(name, index_event)
    variables.update(
       clinical_event_date_X(
            name=name,
            index_date=f"out_date_{name}_{index_event}",
            n=n_max,
            index_from=n_min,
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
        f_path=f"output/repeat_events/out_date_{index_event}.csv.gz"
        ), 

    **out_date_n(name="breathless"),
    **out_date_n(name="asthma_exac"),
    **out_date_n(name="copd_exac"),
    **out_date_n(name="cough"),
    **out_date_n(name="urti"), 

)