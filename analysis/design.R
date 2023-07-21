library(tidyverse) 

# `repeat_events_increments` specifies the lower and upper limits for the number
# of events extracted in each study definition. E.g.:
# 
# - For study_definition_repeat_events_1.py, events `repeat_events_increments$lower[1]` 
#   to `repeat_events_increments$upper[1]` are extracted.
# 
# - For study_definition_repeat_events_x.py with argument step, events 
#   `repeat_events_increments$lower[step]` to `repeat_events_increments$upper[step]` 
#   are extracted.
# 
# The upper limit of the final step (1000 here) must be larger than the maximum 
# events that we are expecting across all outcomes. Inspect the log file for 
# preflight_repeat_events_2 to make sure the values of `repeat_events_increments$upper`
# are appropriate. Redefine if not and rerun preflight_repeat_events_2.
# 
repeat_events_increments <- data.frame(upper = c(5, 20, 1000)) %>%
  mutate(
    step = row_number(), 
    lower = lag(upper, default = 0)+1,
    .before = 1
  )
readr::write_csv(
  repeat_events_increments,
  here::here("lib", "repeat_events_increments.csv")
  )
