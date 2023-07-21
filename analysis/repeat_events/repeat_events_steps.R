################################################################################
# It's a good idea to select "Source on Save" for this file!
# 
# `repeat_events_steps` specifies the lower and upper limits for the number
# of events extracted in each study definition. E.g.:
# 
# - For study_definition_repeat_events_1.py, events `repeat_events_steps$lower[1]` 
#   to `repeat_events_steps$upper[1]` are extracted.
# 
# - For study_definition_repeat_events_x.py with argument step, events 
#   `repeat_events_steps$lower[step]` to `repeat_events_steps$upper[step]` 
#   are extracted.
# 
# The upper limit of the final step (1000 here) must be larger than the maximum 
# events that we are expecting across all outcomes. Inspect the log file for 
# preflight_repeat_events_2 to make sure the values of `repeat_events_steps$upper`
# are appropriate. Redefine if not and rerun preflight_repeat_events_2.
#
################################################################################

library(tidyverse) 

repeat_events_steps <- data.frame(upper = c(5, 20, 1000)) %>%
  mutate(
    step = row_number(), 
    lower = lag(upper, default = 0) + 1,
    .before = 1
  )

# Run checks

local({
  
  check <- readr::read_csv(here::here("lib", "repeat_events_steps.csv"))
  
  stopifnot(
    "`repeat_events_steps` does not match \"lib/repeat_events_steps.csv\".
  Take the following steps:
  1. Run \"analysis/create_project_actions.R\"
  2. Commit changes and push to remote
  3. Rerun the repeat events actions from preflight_repeat_events_2"
    = all(repeat_events_steps == check)
  )
  
})

# Save file to lib
readr::write_csv(
  repeat_events_steps,
  here::here("lib", "repeat_events_steps.csv")
  )
