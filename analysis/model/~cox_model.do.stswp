* Specify parameters

local name "`1'"

// For local testing:
*local name "cohort_prevax-main_preex_FALSE-asthma"

* Set Ado file path

adopath + "analysis/extra_ados"

* Read and describe data

use "./output/model/ready-`name'.dta", clear
describe

* Filter data

keep patient_id exposure outcome fup_start fup_stop cox_weight cov_cat* strat* cov_num* cov_bin* 
drop cov_num_age_sq
duplicates drop

* Rename variables

rename cov_num_age age
rename strat_cat_region region

* Generate pre vaccination cohort dummy variable

local prevax_cohort = regexm("`name'", "_prevax")
display "`prevax_cohort'"

* Generate sex indicator

local sub_sex = regexm("`name'", "sub_sex")
display "`sub_sex'"

* Replace NA with missing value that Stata recognises

ds , has(type string)
foreach var of varlist `r(varlist)' {
	replace `var' = "" if `var' == "NA"
}

* Reformat date variables (already numeric)
foreach var of varlist exposure outcome fup_start fup_stop {
    format `var' %td
}

* Encode only string variables
foreach var of varlist region cov_bin* cov_cat* {
    di "Checking `var'..."
    capture confirm string variable `var'
    if !_rc {  // _rc = 0 means it's a string
        di "Encoding `var'"
        local var_short = substr("`var'", 1, length("`var'") - 1)
        encode `var', generate(`var_short'1)
        drop `var'
        rename `var_short'1 `var'
    }
    else {
        di "`var' is already numeric, skipping encode"
    }
}

* Summarize missingness

misstable summarize

* Update follow-up end

replace fup_stop = fup_stop + 1
format fup_stop %td

* Make age spline

centile age, centile(10 50 90)
mkspline age_spline = age, cubic knots(`r(c_1)' `r(c_2)' `r(c_3)')

* Make outcome status variable

egen outcome_status = rownonmiss(outcome)

* Apply stset including IPW here as unsampled datasets will be provided with cox_weights set to 1

if `prevax_cohort'==1 {
    stset fup_stop [pweight=cox_weight], failure(outcome_status) ///
        id(patient_id) enter(fup_start) origin(time mdy(01,01,2020))
    stsplit time, after(exposure) at(0 1 28 183 365 730 1065 1582)
    replace time = 1582 if time==-1
}
else {
    stset fup_stop [pweight=cox_weight], failure(outcome_status) ///
        id(patient_id) enter(fup_start) origin(time mdy(06,01,2021))
* check whether the date is 1 June 2021, as the codes was mdy(01,06,2021)
    stsplit time, after(exposure) at(0 1 28 183 365 730 1065)
    replace time = 1065 if time==-1
}

* Calculate study follow up

gen fup = _t - _t0
egen fup_total = total(fup)  

* Make days variables

* 0–1 days
gen days0_1 = 0
replace days0_1 = 1 if time==0
tab days0_1

* 1-28 days
gen days1_28 = 0
replace days1_28 = 1 if time==1
tab days1_28

* 28–183 days
gen days28_183 = 0
replace days28_183 = 1 if time == 28
tab days28_183

* 183–365 days
gen days183_365 = 0
replace days183_365 = 1 if time == 183
tab days183_365

* 365–730 days
gen days365_730 = 0
replace days365_730 = 1 if time == 365
tab days365_730

* 730–1065 days
gen days730_1065 = 0
replace days730_1065 = 1 if time == 730
tab days730_1065

* Additional category for prevax cohort only
if `prevax_cohort' == 1 {
    gen days1065_1582 = 0
    replace days1065_1582 = 1 if time == 1065
    tab days1065_1582
}

* Run models and save output [Note: cannot use efron method with weights]

tab time outcome_status 

di "Total follow-up in days: " fup_total
bysort time: summarize(fup), detail

if `sub_sex'==1 {
	stcox days* age_spline1 age_spline2, strata(region) vce(r)
	est store min, title(Age_Sex)
	stcox days* age_spline1 age_spline2 i.cov_cat_* cov_num_* cov_bin_*, strata(region) vce(r)
	est store max, title(Maximal)
}
else {
	stcox days* i.cov_cat_sex age_spline1 age_spline2, strata(region) vce(r)
	est store min, title(Age_Sex)
	stcox days* age_spline1 age_spline2 i.cov_cat_* cov_num_* cov_bin_*, strata(region) vce(r)
	est store max, title(Maximal)	
}


estout * using "output/model/stata_model_output-`name'.txt", cells("b se t ci_l ci_u p") stats(risk N_fail N_sub N N_clust) replace 

* Calculate median follow-up among individuals with the outcome

keep if outcome_status==1
keep patient_id days* fup
rename fup tte
gen term = ""

if `prevax_cohort'==1 {
    replace term = "days_pre"      if days0_1==0 & days1_28==0 & days28_183==0 & days183_365==0 & days365_730==0 & days730_1065==0 & days1065_1582==0
    replace term = "days0_1"       if days0_1==1 & days1_28==0 & days28_183==0 & days183_365==0 & days365_730==0 & days730_1065==0 & days1065_1582==0
    replace term = "days1_28"      if days0_1==0 & days1_28==1 & days28_183==0 & days183_365==0 & days365_730==0 & days730_1065==0 & days1065_1582==0
    replace term = "days28_183"    if days0_1==0 & days1_28==0 & days28_183==1 & days183_365==0 & days365_730==0 & days730_1065==0 & days1065_1582==0
    replace term = "days183_365"   if days0_1==0 & days1_28==0 & days28_183==0 & days183_365==1 & days365_730==0 & days730_1065==0 & days1065_1582==0
    replace term = "days365_730"   if days0_1==0 & days1_28==0 & days28_183==0 & days183_365==0 & days365_730==1 & days730_1065==0 & days1065_1582==0
    replace term = "days730_1065"  if days0_1==0 & days1_28==0 & days28_183==0 & days183_365==0 & days365_730==0 & days730_1065==1 & days1065_1582==0
    replace term = "days1065_1582" if days0_1==0 & days1_28==0 & days28_183==0 & days183_365==0 & days365_730==0 & days730_1065==0 & days1065_1582==1
}
else {
    replace term = "days_pre"      if days0_1==0 & days1_28==0 & days28_183==0 & days183_365==0 & days365_730==0 & days730_1065==0
    replace term = "days0_1"       if days0_1==1 & days1_28==0 & days28_183==0 & days183_365==0 & days365_730==0 & days730_1065==0
    replace term = "days1_28"      if days0_1==0 & days1_28==1 & days28_183==0 & days183_365==0 & days365_730==0 & days730_1065==0
    replace term = "days28_183"    if days0_1==0 & days1_28==0 & days28_183==1 & days183_365==0 & days365_730==0 & days730_1065==0
    replace term = "days183_365"   if days0_1==0 & days1_28==0 & days28_183==0 & days183_365==1 & days365_730==0 & days730_1065==0
    replace term = "days365_730"   if days0_1==0 & days1_28==0 & days28_183==0 & days183_365==0 & days365_730==1 & days730_1065==0
    replace term = "days730_1065"  if days0_1==0 & days1_28==0 & days28_183==0 & days183_365==0 & days365_730==0 & days730_1065==1
}

replace tte = tte + 28   if term == "days28_183"
replace tte = tte + 183  if term == "days183_365"
replace tte = tte + 365  if term == "days365_730"
replace tte = tte + 730  if term == "days730_1065"
replace tte = tte + 1065 if term == "days1065_1582"

bysort term: egen median_tte = median(tte)
egen events = count(patient_id), by(term)

keep term median_tte events
duplicates drop

export delimited using "output/model/stata_fup-`name'.csv", replace