* ==============================================================================
*  Setup and input
* ==============================================================================

* Specify parameters

local name "`1'"

// For local testing:

*local name "cohort_prevax-main_preex_FALSE-asthma"

* Set Ado file path

adopath + "analysis/extra_ados"

* Read and describe data

use "./output/model/ready-`name'.dta", clear
describe

* ==============================================================================
*  Data preparation (matches R preprocessing)
* ==============================================================================

* Filter data

keep patient_id exposure outcome fup_start fup_stop cox_weight cov_cat* strat* cov_num* cov_bin* 
drop cov_num_age_sq
duplicates drop

* Rename variables

rename cov_num_age age
rename strat_cat_region region

* Generate pre vaccination cohort indicators

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

* ==============================================================================
*  Summary checks
* ==============================================================================

misstable summarize

* ==============================================================================
*  Time-to-event setup (stset)
* ==============================================================================

* Update follow-up end

replace fup_stop = fup_stop + 1
format fup_stop %td

* Make age spline

centile age, centile(10 50 90)
mkspline age_spline = age, cubic knots(`r(c_1)' `r(c_2)' `r(c_3)')

* Make outcome status variable

egen outcome_status = rownonmiss(outcome)

* ==============================================================================
*  Total number of patients and exposed patients
* ==============================================================================

egen N_total = count(patient_id)
egen N_exposed = total(!missing(exposure))

* Apply stset including IPW here as unsampled datasets will be provided with cox_weights set to 1

if `prevax_cohort'==1 {
    stset fup_stop [pweight=cox_weight], failure(outcome_status) ///
        id(patient_id) enter(fup_start) origin(time mdy(01,01,2020))
    stsplit time, after(exposure) at(0 1 28 183 365 730 1095 1460 1825 1979)
    replace time = 1979 if time==-1
}
else {
    stset fup_stop [pweight=cox_weight], failure(outcome_status) ///
        id(patient_id) enter(fup_start) origin(time mdy(06,01,2021))
* check whether the date is 1 June 2021, as the codes was mdy(01,06,2021)
    stsplit time, after(exposure) at(0 1 28 183 365 730 1095 1460)
    replace time = 1460 if time==-1
}

* ==============================================================================
*  Person-time and events (matches R collapse step)
* ==============================================================================

gen fup = _t - _t0
egen fup_total = total(fup)  

* Person-time and events aggregated by time interval

egen person_time_total = total(fup), by(time)
egen N_events = total(outcome_status), by(time)

* ==============================================================================
* Median follow-up among those with the event
* ==============================================================================

gen tte = fup if outcome_status==1
egen outcome_time_median = median(tte), by(time)

*  Define indicator variables for each time period

* 0–1 days
gen days0_1 = (time == 0)
tab days0_1

* 1–28 days
gen days1_28 = (time == 1)
tab days1_28

* 28–183 days
gen days28_183 = (time == 28)
tab days28_183

* 183–365 days
gen days183_365 = (time == 183)
tab days183_365

* 365–730 days
gen days365_730 = (time == 365)
tab days365_730

* 730–1095 days
gen days730_1095 = (time == 730)
tab days730_1095

* 1095–1460 days
gen days1095_1460 = (time == 1095)
tab days1095_1460

* Additional category for prevax cohort only
if `prevax_cohort' == 1 {
* 1460–1825 days
    gen days1460_1825 = (time == 1460)
    tab days1460_1825

    gen days1825_1979 = (time == 1825)
    tab days1825_1979
}

* ==============================================================================
*  Adjust median times to cumulative scale
* ==============================================================================
preserve

gen term = ""

if `prevax_cohort'==1 {
    replace term = "days_pre"       if days0_1==0 & days1_28==0 & days28_183==0 & days183_365==0 & days365_730==0 & days730_1095==0 & days1095_1460==0 & days1460_1825==0 & days1825_1979==0
    replace term = "days0_1"        if days0_1==1 & days1_28==0 & days28_183==0 & days183_365==0 & days365_730==0 & days730_1095==0 & days1095_1460==0 & days1460_1825==0 & days1825_1979==0
    replace term = "days1_28"       if days0_1==0 & days1_28==1 & days28_183==0 & days183_365==0 & days365_730==0 & days730_1095==0 & days1095_1460==0 & days1460_1825==0 & days1825_1979==0
    replace term = "days28_183"     if days0_1==0 & days1_28==0 & days28_183==1 & days183_365==0 & days365_730==0 & days730_1095==0 & days1095_1460==0 & days1460_1825==0 & days1825_1979==0
    replace term = "days183_365"    if days0_1==0 & days1_28==0 & days28_183==0 & days183_365==1 & days365_730==0 & days730_1095==0 & days1095_1460==0 & days1460_1825==0 & days1825_1979==0
    replace term = "days365_730"    if days0_1==0 & days1_28==0 & days28_183==0 & days183_365==0 & days365_730==1 & days730_1095==0 & days1095_1460==0 & days1460_1825==0 & days1825_1979==0
    replace term = "days730_1095"   if days0_1==0 & days1_28==0 & days28_183==0 & days183_365==0 & days365_730==0 & days730_1095==1 & days1095_1460==0 & days1460_1825==0 & days1825_1979==0
    replace term = "days1095_1460"  if days0_1==0 & days1_28==0 & days28_183==0 & days183_365==0 & days365_730==0 & days730_1095==0 & days1095_1460==1 & days1460_1825==0 & days1825_1979==0
    replace term = "days1460_1825"  if days0_1==0 & days1_28==0 & days28_183==0 & days183_365==0 & days365_730==0 & days730_1095==0 & days1095_1460==0 & days1460_1825==1 & days1825_1979==0
    replace term = "days1825_1979"  if days0_1==0 & days1_28==0 & days28_183==0 & days183_365==0 & days365_730==0 & days730_1095==0 & days1095_1460==0 & days1460_1825==0 & days1825_1979==1
}
else {
    replace term = "days_pre"       if days0_1==0 & days1_28==0 & days28_183==0 & days183_365==0 & days365_730==0 & days730_1095==0 & days1095_1460==0
    replace term = "days0_1"        if days0_1==1 & days1_28==0 & days28_183==0 & days183_365==0 & days365_730==0 & days730_1095==0 & days1095_1460==0
    replace term = "days1_28"       if days0_1==0 & days1_28==1 & days28_183==0 & days183_365==0 & days365_730==0 & days730_1095==0 & days1095_1460==0
    replace term = "days28_183"     if days0_1==0 & days1_28==0 & days28_183==1 & days183_365==0 & days365_730==0 & days730_1095==0 & days1095_1460==0
    replace term = "days183_365"    if days0_1==0 & days1_28==0 & days28_183==0 & days183_365==1 & days365_730==0 & days730_1095==0 & days1095_1460==0
    replace term = "days365_730"    if days0_1==0 & days1_28==0 & days28_183==0 & days183_365==0 & days365_730==1 & days730_1095==0 & days1095_1460==0
    replace term = "days730_1095"   if days0_1==0 & days1_28==0 & days28_183==0 & days183_365==0 & days365_730==0 & days730_1095==1 & days1095_1460==0
    replace term = "days1095_1460"  if days0_1==0 & days1_28==0 & days28_183==0 & days183_365==0 & days365_730==0 & days730_1095==0 & days1095_1460==1
}

replace outcome_time_median = outcome_time_median + 28   if term == "days28_183"
replace outcome_time_median = outcome_time_median + 183  if term == "days183_365"
replace outcome_time_median = outcome_time_median + 365  if term == "days365_730"
replace outcome_time_median = outcome_time_median + 730  if term == "days730_1095"
replace outcome_time_median = outcome_time_median + 1095 if term == "days1095_1460"
replace outcome_time_median = outcome_time_median + 1460 if term == "days1460_1825"
replace outcome_time_median = outcome_time_median + 1825 if term == "days1825_1979"

order term N_total N_exposed N_events person_time_total outcome_time_median   
keep term N_total N_exposed N_events person_time_total outcome_time_median 
duplicates drop

* ==============================================================================
*  Save descriptive output (aligns with R's descriptive summary)
* ==============================================================================

save "./output/model/stata_tmp-`name'.dta", replace

* ==============================================================================
* Run models and save output [Note: cannot use efron method with weights]
* ==============================================================================
restore

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

* Save coefficients with CIs
estout * using "./output/model/stata_tmp_output-`name'.txt", cells("b se t ci_l ci_u p") stats(risk N_fail N_sub N N_clust) replace 

* ==============================================================================
* Format stata model output
* ==============================================================================

* Step 1. Import text file as raw

import delimited using "./output/model/stata_tmp_output-`name'.txt", clear stringcols(_all)

* Step 2. Rename columns properly

rename v1 term
rename v2 b_min
rename v3 se_min
rename v4 t_min
rename v5 lci_min
rename v6 uci_min
rename v7 p_min
rename v8 b_max
rename v9 se_max
rename v10 t_max
rename v11 lci_max
rename v12 uci_max
rename v13 p_max
drop in 1/2
* Drop last 5 rows (risk, N_fail, etc.)
count
drop in `=`r(N)'-4'/`r(N)'
drop p_* t_*

* Step 3. Convert to numeric where possible
destring b_min se_min lci_min uci_min ///
         b_max se_max lci_max uci_max, replace force

* Step 4. Calculate HRs from log-HR (b_min, b_max)
gen hr_min = exp(b_min)
gen conf_low_min = exp(lci_min)
gen conf_high_min = exp(uci_min)

gen hr_max = exp(b_max)
gen conf_low_max = exp(lci_max)
gen conf_high_max = exp(uci_max)

* Step 5. Reshape to long form for model variable

reshape long b_ se_ lci_ uci_ hr_ conf_low_ conf_high_, i(term) j(model) string

* Label models
replace model = "mdl_age_sex" if model=="min"
replace model = "mdl_max_adj" if model=="max"

* Rename for clarity
rename b_ lnhr
rename se_ se_lnhr
rename hr_ hr
rename conf_low_ conf_low
rename conf_high_ conf_high

*Relabel for readability
replace term = "cov_cat_sex=Female" if term=="1.cov_cat_sex"
replace term = "cov_cat_sex=Male" if term=="2.cov_cat_sex"
replace term = "cov_cat_imd=1 (most deprived)" if term=="1.cov_cat_imd"
replace term = "cov_cat_imd=2" if term=="2.cov_cat_imd"
replace term = "cov_cat_imd=3" if term=="3.cov_cat_imd"
replace term = "cov_cat_imd=4" if term=="4.cov_cat_imd"
replace term = "cov_cat_imd=5 (least deprived)" if term=="5.cov_cat_imd"

*tidy dataset by removing unnessary rows
drop if model=="mdl_age_sex" & !(strpos(term,"age_spline") | strpos(term,"cov_cat_sex=Male") | strpos(term,"days"))
drop if strpos(term,"cov_cat_sex=Female") | strpos(term,"cov_cat_imd=1 (most deprived)")

*Step 6. Merge with survival information 

merge m:1 term using "./output/model/stata_tmp-`name'.dta", nogen
sort model term

*Step 7. Add metadata fields

gen strata_warning = ""
gen surv_formula = ""
replace surv_formula = "Surv(_t0, _t, _d) ~ days* + age_spline1 + age_spline2 + i.cov_cat_sex strata(region)" if model=="mdl_age_sex"
replace surv_formula = "Surv(_t0, _t, _d) ~ days* + age_spline1 + age_spline2 + i.cov_cat_* + cov_num_* + cov_bin_* strata(region)" if model=="mdl_max_adj"

*Step 8. Save final CSV

export delimited using "./output/model/stata_model_output-`name'.csv", replace
