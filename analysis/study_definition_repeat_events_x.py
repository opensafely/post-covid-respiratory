###################################################################################
# 
# This study definition:
# - Takes the paramenter `step`
# - Selects patients with >= repeat_events_steps$lower[step] of any event type
# - Reads the vairables out_date_{name}_{repeat_events_steps$upper[step-1]}
# - Extracts out_date_{name}_{repeat_events_steps$lower[step]} to
#   out_date_{name}_{repeat_events_steps$upper[step]}
# 
###################################################################################

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

# repeat_events_steps
import pandas as pd
repeat_events_steps = pd.read_csv(
    filepath_or_buffer='./lib/repeat_events_steps.csv',
    dtype=int
)

# max events for each outcome
with open("output/repeat_events/max_events.json") as f:
   max_events = json.load(f)

#study_dates
with open("output/study_dates.json") as f:
  study_dates = json.load(f)

# params
step=int(params["step"])
# remember indexing starts at 0 in python, so take an extra -1 from step compared to R
index_event=repeat_events_steps["upper"][step-2] 
n_lower=repeat_events_steps["lower"][step-1]
n_upper=repeat_events_steps["upper"][step-1]

# extract repeat events
def out_date_n(
      name, 
      index_event=index_event,
      n_lower=n_lower, 
      n_upper=n_upper, 
      max_events=max_events
      ):
    # redefine n_upper to be the minimum of n_upper and max_events["name"]
    n_upper=min(n_upper, int(max_events[name]))
    # function for creating the out_date_5 variable
    def out_date_index(name, index_event):
        return {
            f"out_date_{name}_{index_event}": patients.with_value_from_file(
                f_path=f"output/repeat_events/out_date_{step}.csv.gz",
                returning=f"out_date_{name}_{index_event}", 
                returning_type="date", 
                date_format='YYYY-MM-DD',
            )
        }
    variables=out_date_index(name, index_event)
    variables.update(
       clinical_event_date_X(
            name=name,
            start_date=f"out_date_{name}_{index_event}",
            end_date=study_dates["omicron_date"],
            n=n_upper,
            index_from=n_lower,
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
        f_path=f"output/repeat_events/out_date_{step}.csv.gz"
        ), 

    **out_date_n(name="breathless"),
    **out_date_n(name="asthma_exac"),
    **out_date_n(name="copd_exac"),
    **out_date_n(name="cough"),
    **out_date_n(name="urti"), 

)