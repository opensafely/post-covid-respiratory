from ehrql import (
    codelist_from_csv,
    create_dataset,
    days,
    case,
    when,
    minimum_of,
    maximum_of,
)
# Bring table definitions from the TPP backend 
from ehrql.tables.tpp import ( 
    patients, 
    practice_registrations, 
    addresses, 
    appointments, 
    occupation_on_covid_vaccine_record,
    vaccinations,
    sgss_covid_all_tests,
    apcs, 
    ec, 
    opa, 
    opa_diag, 
    clinical_events, 
    medications, 
    ons_deaths,
)

# Codelists from codelists.py (which pulls all variables from the codelist folder)
from codelists import *


# Call functions from variable_helper_functions
from variable_helper_functions import (
    first_matching_event_clinical_ctv3_between,
    first_matching_event_clinical_snomed_between,
    first_matching_med_dmd_between,
    first_matching_event_apc_between,
    first_matching_event_opa_between,
    first_matching_event_ec_snomed_between,
    matching_death_between,
    last_matching_event_clinical_ctv3_before,
    last_matching_event_clinical_snomed_before,
    last_matching_med_dmd_before,
    last_matching_event_apc_before,
    last_matching_event_opa_before,
    last_matching_event_ec_snomed_before,
    matching_death_before,
)


def generate_variables(index_date, end_date_exp, end_date_out):  

    ## Define individual temporary variables (for exposures) first

        ### Covid
    tmp_exp_date_covid19_confirmed_sgss = (
        sgss_covid_all_tests.where(
            sgss_covid_all_tests.specimen_taken_date.is_on_or_between(index_date, end_date_exp)
        )
        .where(sgss_covid_all_tests.is_positive)
        .sort_by(sgss_covid_all_tests.specimen_taken_date)
        .first_for_patient()
        .specimen_taken_date
    )

    tmp_exp_date_covid19_confirmed_snomed = (
        clinical_events.where(
            (clinical_events.ctv3_code.is_in(
                covid_primary_care_code + 
                covid_primary_care_positive_test +
                covid_primary_care_sequalae)) &
            clinical_events.date.is_on_or_between(index_date, end_date_exp)
        )
        .sort_by(clinical_events.date)
        .first_for_patient()
        .date
    )

    tmp_exp_date_covid19_confirmed_apc = (
        apcs.where(
            ((apcs.primary_diagnosis.is_in(covid_codes)) | 
             (apcs.secondary_diagnosis.is_in(covid_codes))) & 
            (apcs.admission_date.is_on_or_between(index_date, end_date_exp))
        )
        .sort_by(apcs.admission_date)
        .first_for_patient()
        .admission_date
    )

    tmp_exp_date_covid19_confirmed_opa = (
        opa_diag.where(
            ((opa_diag.primary_diagnosis_code.is_in(covid_codes)) | 
             (opa_diag.secondary_diagnosis_code_1.is_in(covid_codes))) & 
            (opa_diag.appointment_date.is_on_or_between(index_date, end_date_exp))
        )
        .sort_by(opa_diag.appointment_date)
        .first_for_patient()
        .appointment_date
    )

    tmp_exp_covid19_confirmed_death = matching_death_between(covid_codes, index_date, end_date_exp)

    tmp_exp_date_death = ons_deaths.date

    tmp_exp_date_covid19_confirmed_death = case(
        when(tmp_exp_covid19_confirmed_death).then(tmp_exp_date_death)
    )

    ## Define individual temporary variables (subgroup variables) first before using them in the dictrionary

    ## 1. Covid-19 prior to study start date (2020-01-01)

    ### SGSS

    tmp_sub_bin_priorcovid19_confirmed_sgss = (
        sgss_covid_all_tests.where(
            sgss_covid_all_tests.specimen_taken_date.is_before(index_date)
        )
        .where(sgss_covid_all_tests.is_positive)
        .exists_for_patient()
    )

    ### Primary care

    tmp_sub_bin_priorcovid19_confirmed_snomed = (
        clinical_events.where(
            (clinical_events.ctv3_code.is_in(
                covid_primary_care_code + 
                covid_primary_care_positive_test + 
                covid_primary_care_sequalae)) &
            clinical_events.date.is_before(index_date)
        )
        .exists_for_patient()
    )

    ### SUS

    tmp_sub_bin_priorcovid19_confirmed_apc = (
        apcs.where(
            ((apcs.primary_diagnosis.is_in(covid_codes)) | (apcs.secondary_diagnosis.is_in(covid_codes))) & 
            (apcs.admission_date.is_before(index_date))
        )
        .exists_for_patient()
    )

    tmp_sub_bin_priorcovid19_confirmed_opa = (
        opa_diag.where(
            ((opa_diag.primary_diagnosis_code.is_in(covid_codes)) | (opa_diag.secondary_diagnosis_code_1.is_in(covid_codes))) & 
            (opa_diag.appointment_date.is_before(index_date))
        )
        .exists_for_patient()
    )

    ## 2. Covid-19 severity

    ### SUS

    tmp_sub_date_severecovid19_apc = (
        apcs.where(
            ((apcs.primary_diagnosis.is_in(covid_codes)) | (apcs.secondary_diagnosis.is_in(covid_codes))) & 
            (apcs.admission_date.is_on_or_after(index_date))
        )
        .sort_by(apcs.admission_date)
        .first_for_patient()
        .admission_date
    )

    tmp_sub_date_severecovid19_opa = (
        opa_diag.where(
            ((opa_diag.primary_diagnosis_code.is_in(covid_codes)) | (opa_diag.secondary_diagnosis_code_1.is_in(covid_codes))) & 
            (opa_diag.appointment_date.is_on_or_after(index_date))
        )
        .sort_by(opa_diag.appointment_date)
        .first_for_patient()
        .appointment_date
    )



    ## Combine the variables into the final dictionary
    dynamic_variables = dict(

# Exposures---------------------------------------------------------------------------------------------------

        ### Exposures----Covid-19

        tmp_exp_date_covid19_confirmed_sgss=tmp_exp_date_covid19_confirmed_sgss,
        tmp_exp_date_covid19_confirmed_snomed=tmp_exp_date_covid19_confirmed_snomed,
        tmp_exp_date_covid19_confirmed_apc=tmp_exp_date_covid19_confirmed_apc,
        tmp_exp_date_covid19_confirmed_opa=tmp_exp_date_covid19_confirmed_opa,
        tmp_exp_date_death=tmp_exp_date_death,
        tmp_exp_covid19_confirmed_death=tmp_exp_covid19_confirmed_death,
        tmp_exp_date_covid19_confirmed_death=tmp_exp_date_covid19_confirmed_death,
        
        exp_date_covid19_confirmed=minimum_of(
            tmp_exp_date_covid19_confirmed_sgss, 
            tmp_exp_date_covid19_confirmed_snomed,
            tmp_exp_date_covid19_confirmed_apc,
            tmp_exp_date_covid19_confirmed_opa,
            tmp_exp_date_covid19_confirmed_death
        ),



# Outcomes---------------------------------------------------------------------------------------------------

    ## ---First recording of the outcome in during the study period

      ### Pneumonia
        out_date_pneumonia= (
            first_matching_event_clinical_snomed_between(
            pneumonia_snomed, index_date, end_date_out
            )
        .date
        ),

      ### Asthma
        out_date_asthma= (
            first_matching_event_clinical_snomed_between(
            asthma_snomed, index_date, end_date_out
            )
        .date
        ),

      ### COPD
        out_date_asthma= (
            first_matching_event_clinical_ctv3_between(
            copd_ctv3, index_date, end_date_out
            )
        .date
        ),

      ### Pulmonary Fibrosis
        out_date_pulmonary_fibrosis= (
            first_matching_event_clinical_snomed_between(
            pulmonary_fibrosis_snomed, index_date, end_date_out
            )
        .date
        ),

# DEFINE EXISTING RESPIRATORY CONDITION COHORT --------------------------------------------------------------
        sub_bin_asthma_recent_snomed= (
            first_matching_event_clinical_ctv3_between(
            copd_ctv3, index_date-days(730), index_date-days(1)
            )
        .exists_for_patient()        
        ),
# COPD diagnosed ever----------------------------------------------------------------------------------------       
        sub_bin_copd_ctv3=(
            last_matching_event_clinical_ctv3_before(
            copd_ctv3, index_date
            )    
        .exists_for_patient()       
        )

# Covariates-------------------------------------------------------------------------------------------------  

        ## Age
        cov_date_of_birth=patients.date_of_birth,
        cov_num_age=patients.age_on(index_date),

        ## Sex
        cov_cat_sex=patients.sex,

        ## Ethnicity
        cov_cat_ethnicity=(
            clinical_events.where(
                clinical_events.ctv3_code.is_in(opensafely_ethnicity_codes_6)
            )
            .sort_by(clinical_events.date)
            .last_for_patient()
            .ctv3_code.to_category(opensafely_ethnicity_codes_6)
        ),

        ## Deprivation (IMD, 5 categories)
        cov_cat_imd=case(
            when((addresses.for_patient_on(index_date).imd_rounded >= 0) & 
                 (addresses.for_patient_on(index_date).imd_rounded < int(32844 * 1 / 5))).then("1 (most deprived)"),
            when(addresses.for_patient_on(index_date).imd_rounded < int(32844 * 2 / 5)).then("2"),
            when(addresses.for_patient_on(index_date).imd_rounded < int(32844 * 3 / 5)).then("3"),
            when(addresses.for_patient_on(index_date).imd_rounded < int(32844 * 4 / 5)).then("4"),
            when(addresses.for_patient_on(index_date).imd_rounded < int(32844 * 5 / 5)).then("5 (least deprived)"),
            otherwise="unknown",
        ),

        ## Region
        cov_cat_region=practice_registrations.for_patient_on(index_date).practice_nuts1_region_name,
        ## Consultation rate (these codes can run locally but fail in GitHub action test, details see https://docs.opensafely.org/ehrql/reference/schemas/tpp/#appointments)
#cov_num_consultation_rate = appointments.where(
#    appointments.status.is_in([
#        "Arrived",
#        "In Progress",
#        "Finished",
#        "Visit",
#        "Waiting",
#        "Patient Walked Out",
#    ]) & appointments.start_date.is_on_or_between(study_start_date - days(365), study_start_date)
#).count_for_patient()

        ## Smoking status (check this)
        cov_cat_smoking_status=last_matching_event_clinical_ctv3_before(
            smoking_clear, index_date
        ).ctv3_code.to_category(smoking_clear),


        ## Obesity 
        cov_bin_obesity=(
            (last_matching_event_clinical_snomed_before(
                bmi_obesity_snomed_clinical, index_date
            ).exists_for_patient()) |
            (last_matching_event_apc_before(
                bmi_obesity_icd10, index_date
            ).exists_for_patient()) |
            (last_matching_event_opa_before(
                bmi_obesity_icd10, index_date
            ).exists_for_patient())
        ),

        ## Carer
        cov_bin_carer=clinical_events.where(
            (clinical_events.snomedct_code.is_in(carer_primis)) &
            (clinical_events.date.is_before(index_date))
        ).exists_for_patient(),

        ## Healthcare worker
        cov_bin_healthcare_worker=occupation_on_covid_vaccine_record.where(
            (occupation_on_covid_vaccine_record.is_healthcare_worker == True)
        ).exists_for_patient(),

        ## Care home status
        cov_bin_carehome=(
            addresses.for_patient_on(index_date).care_home_is_potential_match |
            addresses.for_patient_on(index_date).care_home_requires_nursing |
            addresses.for_patient_on(index_date).care_home_does_not_require_nursing
        ),

        ## Dementia
        cov_bin_dementia_combined=(
            (last_matching_event_clinical_snomed_before(
                dementia_snomed_clinical + dementia_vascular_snomed_clinical, index_date
            ).exists_for_patient()) |
            (last_matching_event_clinical_ctv3_before(
                dementia_ctv3, index_date
            ).exists_for_patient()) |
            (last_matching_event_apc_before(
                dementia_icd10 + dementia_vascular_icd10, index_date
            ).exists_for_patient()) |
            (last_matching_event_opa_before(
                dementia_icd10 + dementia_vascular_icd10, index_date
            ).exists_for_patient())
        ),

        ## Liver disease
        cov_bin_liver_disease=(
            (last_matching_event_clinical_snomed_before(
                liver_disease_snomed_clinical, index_date
            ).exists_for_patient()) |
            (last_matching_event_apc_before(
                liver_disease_icd10, index_date
            ).exists_for_patient()) |
            (last_matching_event_opa_before(
                liver_disease_icd10, index_date
            ).exists_for_patient())
        ),

        ## Chronic liver disease
        cov_bin_chronic_liver_disease=last_matching_event_clinical_ctv3_before(
            cld_ctv3, index_date
        ).exists_for_patient(),

        ## Chronic kidney disease
        cov_bin_chronic_kidney_disease=(
            (last_matching_event_clinical_snomed_before(
                ckd_snomed_clinical, index_date
            ).exists_for_patient()) |
            (last_matching_event_apc_before(
                ckd_icd10, index_date
            ).exists_for_patient()) |
            (last_matching_event_opa_before(
                ckd_icd10, index_date
            ).exists_for_patient())
        ),

        ## Cancer
        cov_bin_cancer=(
            (last_matching_event_clinical_snomed_before(
                cancer_snomed_clinical, index_date
            ).exists_for_patient()) |
            (last_matching_event_apc_before(
                cancer_icd10, index_date
            ).exists_for_patient()) |
            (last_matching_event_opa_before(
                cancer_icd10, index_date
            ).exists_for_patient())
        ),

        ## Hypertension
        cov_bin_hypertension=(
            (last_matching_event_clinical_snomed_before(
                hypertension_snomed_clinical, index_date
            ).exists_for_patient()) |
            (last_matching_event_clinical_ctv3_before(
                hypertension_ctv3, index_date
            ).exists_for_patient()) |
            (last_matching_med_dmd_before(
                hypertension_drugs_dmd, index_date
            ).exists_for_patient()) |
            (last_matching_event_apc_before(
                hypertension_icd10, index_date
            ).exists_for_patient()) |
            (last_matching_event_opa_before(
                hypertension_icd10, index_date
            ).exists_for_patient())
        ),

        ## Diabetes # to add hba1c?
        cov_bin_diabetes=(
            (last_matching_event_clinical_snomed_before(
                hypertension_snomed_clinical, index_date
            ).exists_for_patient()) |
            (last_matching_event_clinical_ctv3_before(
                diabetes_type1_ctv3 + diabetes_type2_ctv3 + diabetes_diagnostic_ctv3 + diabetes_other_ctv3 + diabetes_gestational_ctv3, index_date
            ).exists_for_patient()) |
            (last_matching_med_dmd_before(
                insulin_dmd + antidiabetic_drugs_dmd + non_metformin_dmd, index_date
            ).exists_for_patient()) |
            (last_matching_event_apc_before(
                diabetes_type1_icd10 + diabetes_type2_icd10, index_date
            ).exists_for_patient()) |
            (last_matching_event_opa_before(
                diabetes_type1_icd10 + diabetes_type2_icd10, index_date
            ).exists_for_patient())
        ),

        ## COPD
        cov_bin_copd=(
            (last_matching_event_clinical_snomed_before(
                copd_snomed_clinical, index_date
            ).exists_for_patient()) |
            (last_matching_event_apc_before(
                copd_icd10, index_date
            ).exists_for_patient()) |
            (last_matching_event_opa_before(
                copd_icd10, index_date
            ).exists_for_patient())
        ),


        ## Ischaemic stroke
        cov_bin_stroke_isch=(
            (last_matching_event_clinical_snomed_before(
                stroke_isch_snomed_clinical, index_date
            ).exists_for_patient()) |
            (last_matching_event_apc_before(
                stroke_isch_icd10, index_date
            ).exists_for_patient()) |
            (last_matching_event_opa_before(
                stroke_isch_icd10, index_date
            ).exists_for_patient())
        ),

        ## All stroke
        cov_bin_stroke=(
            (last_matching_event_clinical_ctv3_before(
                stroke_codes, index_date
            ).exists_for_patient()) |
            (last_matching_event_apc_before(
                stroke_icd10, index_date
            ).exists_for_patient()) |
            (last_matching_event_opa_before(
                stroke_icd10, index_date
            ).exists_for_patient())
        ),
# Others
    ## History of Covid-19 Combined

        tmp_sub_bin_priorcovid19_confirmed_sgss=tmp_sub_bin_priorcovid19_confirmed_sgss,
        tmp_sub_bin_priorcovid19_confirmed_snomed=tmp_sub_bin_priorcovid19_confirmed_snomed,
        tmp_sub_bin_priorcovid19_confirmed_apc=tmp_sub_bin_priorcovid19_confirmed_apc,
        tmp_sub_bin_priorcovid19_confirmed_opa=tmp_sub_bin_priorcovid19_confirmed_opa,
        sub_bin_covid19_confirmed_history=(
            tmp_sub_bin_priorcovid19_confirmed_sgss |
            tmp_sub_bin_priorcovid19_confirmed_snomed |
            tmp_sub_bin_priorcovid19_confirmed_apc |
            tmp_sub_bin_priorcovid19_confirmed_opa 
        ),

    ## Covid_19 severity

        sub_date_covid19_hospital = minimum_of(tmp_sub_date_severecovid19_apc, tmp_sub_date_severecovid19_opa),

    # Inclusion/exclusion variables ----------------------------------------------------------------------------------------------------


    ## Registered for a minimum of 6 months prior to the study start date # line 98: https://github.com/opensafely/comparative-booster-spring2023/blob/main/analysis/dataset_definition.py 

        has_follow_up_previous_6months = (practice_registrations.for_patient_on(
            index_date - days(180)
            )).exists_for_patient(),

    ## Alive on the study start date

        was_alive = (((patients.date_of_death.is_null()) | (patients.date_of_death.is_after(index_date))) & 
        ((ons_deaths.date.is_null()) | (ons_deaths.date.is_after(index_date)))),

        has_died = (patients.date_of_death.is_before(index_date) | ons_deaths.date.is_before(index_date))


    # Quality assurance variables---------------------------------------------------------------------------------------------------------- 

    ## Prostate cancer

        qa_bin_prostate_cancer=(
            (last_matching_event_clinical_snomed_before(
                prostate_cancer_snomed_clinical, index_date
            ).exists_for_patient()) |
            (last_matching_event_apc_before(
                prostate_cancer_icd10, index_date
            ).exists_for_patient()) |
            (last_matching_event_opa_before(
                prostate_cancer_icd10, index_date
            ).exists_for_patient())
        ),

        ## Pregnancy

        qa_bin_pregnancy=last_matching_event_clinical_snomed_before(
            pregnancy_snomed_clinical, index_date
        ).exists_for_patient(),

        ## Year of birth
        qa_num_birth_year=patients.date_of_birth.year,

        ## COCP or heart medication
        qa_bin_hrtcocp=last_matching_med_dmd_before(
            cocp_dmd + hrt_dmd, index_date
        ).exists_for_patient(),
    )
    
    return dynamic_variables