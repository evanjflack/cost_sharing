## 01_sample

These scripts create the main analytic sample along with the dual prediction, falsification, and coinsurance rate caculation samples

* `01a_id_new_enrollees.sas`: Identifies OASI new enrollees at age 65 or 66  
* `01b_id_dual_benes.sas`: identifies beneficaries for dual prediction sample
* `01c_make_dual_pred_sample.R`: Makes dual predicition sample
* `02a_subset_new_enrollee_claims.sas`: Subsets bsf/ip/pde files to new enrollees
* `02b_subset_dual_claims.sas`: Subsets bsf/ip/pde files to dual prediction sample
* `03_transform_bsf_to_long.R`: Transforms new enrollee bsf file to long and defines sample keep criteria
* `04a_build_new_enrollee_sample.R`: Creates (almost) main analystic sample (final sample is created in 03_model/02_iv/01_prep_data)
* `04b_build_pre_dec_samples.R`: Makes samples of those that ft all criteria but die/lose coverage before december
* `04c_build_enrollment_month_sample.R`: Makes sample of all those that fit sample criteria at the time of enrollment
* `04d_build_dual_65_sample.R`: Makes dual age 65 falsification sample
* `05a_id_sample_plans.R`: Identfies all plans that sample beneficiaries are in
* `05b_id_plan_benes_in_bsf.sas`: Subsets bsf to those in a sample plan
* `05c_subset_plan_pde.sas`: Subsets pde to those in sample plans
* `06a_subset_older_benes_bsf.sas`: Subsets to old/disabled/older-dual enrollees (start of falsification sample)
* `06b_transform_old_disabled_bsf_to_long.R`: Mirrors 03_transform... but for the falsification sample
* `06c_make_old_dis_false_samples.R`: Builds falsification samples
* `06d_calc_old_dis_initial_spending.sas`: Calculates initial (Jan-March) spending for the falsification samples
