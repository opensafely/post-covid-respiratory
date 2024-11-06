# post-covid-respiratory

## Repository navigation

-   If you are interested in how we defined our code lists, look in the [`codelists`](./codelists) folder.

-   Analyses scripts are in the [`analysis`](./analysis) directory:

    -   If you are interested in how we defined our variables, we use the variable script [variable_helper_fuctions](analysis/variable_helper_functions.py) to define functions that generate variables. We then apply these functions in [variables_cohorts](analysis/variables_cohorts.py) to create a dictionary of variables for cohort definitions, and in [variables_dates](analysis/variables_dates.py) to create a dictionary of variables for calculating study start dates and end dates.
    -   If you are interested in how we defined study dates (e.g., index and end dates), these vary by cohort and are described in the protocol. We use the script [dataset_definition_dates](analysis/dataset_definition_dates.py) to generate a dataset with all required dates for each cohort. This script imported all variables generated from [variables_dates](analysis/variables_dates.py).
    -   If you are interested in how we defined cohorts, we use dataset definition script [dataset_definition_cohorts](analysis/dataset_definition_cohorts.py) to define the function that generate cohort. This script imported all variables generated from [variables_cohorts](analysis/variables_cohorts.py). We extracted the respective index dates and end dates for each cohort and applied this function to generate three specific cohorts—pre-vaccination, vaccinated, and unvaccinated—found in [dataset_definition_prevax](analysis/dataset_definition_prevax.py), [dataset_definition_vax](analysis/dataset_definition_vax.py), and [dataset_definition_unvax](analysis/dataset_definition_unvax.py). Extracted data is then combined to create our final cohorts, in the [preprocess data script](analysis/preprocess_data.R).
    -   This directory also contains all the R scripts that process, describe, and analyse the extracted data.

-   The [`lib/`](./lib) directory contains a list of active analyses.

-   The [`project.yaml`](./project.yaml) defines run-order and dependencies for all the analysis scripts. This file should not be edited directly. To make changes to the yaml, edit and run the [`create_project.R`](analysis/create_project.R) script which generates all the actions.

-   Descriptive and Model outputs, including figures and tables are in the [`released_outputs`](./release_outputs) directory.
