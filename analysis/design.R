 
# the upper limit of the last increment (1000 here) just has to be some number
# that must be larger than the maximum events that we are expecting across all 
# outcomes.
repeat_events_increments <- data.frame(min = c(1, 6, 21), max = c(5, 20, 1000))
readr::write_csv(
  repeat_events_increments,
  here::here("lib", "repeat_events_increments.csv")
  )
