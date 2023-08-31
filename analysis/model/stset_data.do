*Import data
import delimited "C:\Users\hk19914\OneDrive - University of Bristol\Documents\GitHub\post-covid-respiratory\output\repeat_events\repeat_events_prevax_asthma_exac_no_preexisting.csv"

*Format variables
la define date_label 1 "index_date" 2 "episode_start" 3 "episode_end" 4 "end_date_outcome" 5 "exposure_date"
encode date_label, g(date_code) la(date_label)

g date2 = date(date, "YMD")
g exposure_date2 = date(exposure_date, "YMD")
format date2 exposure_date2 %td
drop date_label date exposure_date
rename date2 date
rename exposure_date2 exposure_date

*Add 1 to end_date_outcome
replace date = date + 1 if date_code == 4

*Stset data
xtset patient_id date

stset date, id(patient_id) origin(time mdy(01,01,2020)) enter(date_code == 1 3) failure(date_code == 2) exit(date_code == 4)
stsplit exposure, after(exposure_date) at(0 713)

replace exposure = 1 if exposure == 0
replace exposure = 0 if exposure == -1 | exposure == 713

*Model
xtstreg exposure, distribution(weibull)
