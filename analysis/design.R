library(tidyverse) 

# repeat_events_increments can be adapted based on the results of the action `prelight_repeat_events_2`
# The upper limit of the last increment (1000 here) just has to be some number
# that must be larger than the maximum events that we are expecting across all 
# outcomes.
repeat_events_increments <- data.frame(max = c(5, 20, 1000)) %>%
  mutate(
    step = row_number(), 
    min = lag(max, default = 0)+1,
    .before = 1
  )
readr::write_csv(
  repeat_events_increments,
  here::here("lib", "repeat_events_increments.csv")
  )
