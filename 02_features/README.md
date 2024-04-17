## 02_features

These scripts create features for the analysis

* `01a_make_new_enrollee_mortality_outcomes.R`: Makes mortality outcomes for main sample by month
* `01b_make_initial_mortality_outcomes.R`: Makes morality outcomes for the main sample by days from enrollment (for table 2)
* `02a_calc_new_enrollee_claims_cost_month.R`: Calculates spending and fills/days measures by month in main sample
* `03a_create_new_enrollee_initial_part_d_features.R`: Creates initial features (spending, fills, ATC) in main sample
* `03b_create_dual_initial_part_d_features.R`:  Creates initial features (spending, fills, ATC) in dual prediction sample
* `04a_determine_most_common_atc_classes.R`: Determines most common ATC classes in initial period in main sample
* `04b_create_new_enrollee_end_year1_class_variables.R`: Creates outcomes for number of days filled by ATC class in December year 1 for main sample
* `05_estimate_plan_cost_sharing.R`: Calculates average coinsurance rates by plan/year/cost-sharing arm

### get_drug_info
* `01_id_unique_ndc9.R`: Identifies all 9-digit ndc (labeler/product) codes in the main sample
* `02_create_ndc_atc_xwalk.R`: Creates xwalk between 9-digit ndc codes and atc4 codes
* `03_get_atc_indicators.R`: Makes indicators for atc classes for each ndc9 code

