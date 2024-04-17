### 02_iv

These scripts product the tables/figures in the main text and appendix. 

### 01_prep_data
* `01_make_final_sample.R`: Subset to final analytic sample (those joining in IEP between Feb-Sep)
* `02_prep_spending_data.R`: Subsets monthly spending data for main analytic sample
* `03_prep_co_rate_data.R`: Finds en-of-year average prices and coinsuance rates for each beneficiary (to be used to compare to literture estimates)

### 02_tables:
* `01_sum_stats.R`: Makes table 1 (sample summary statistics)
* `02a_dem_util_balance.R`: Makes table 2 panel A
* `02b_initial_mort_balance.R`: Makes table 2 panel B
* `03a_mort_rf.R`: Makes table 3 panels A and B
* `03b_mort_false.R`: Makes table 3 panel C
* `04_estimate_util_by_class_risk.R`: Makes table 4 and table D.2

### 03_figures
* `01_survey.R`: Makes figure 1
* `02a_part_d_design.R`: Makes figure 2 panel A
* `02b_cov_arm_by_month.R`: Makes figure 2 panel B
* `03_arm_util_mort_rf_figure.R`: Makes figure 3
* `04a_estimate_year_1_2_gap_util_mort_rf.R`: Estimates for figure 4
* `04b_plot_year_1_2_gap_util_mort_rf.R`: Makes figure 4
* `05a_estimate_oos_false.R`: Estimate from old/dual/dis samples for figure 5
* `05b_estimate_ws_false.R`: Estimates from main sample for figure 5
* `05c_estimate_dual_sub_sample_false.R`: Estimates from dual sub-samples for figure 5, and figures C.1/C.2
* `05d_plot_false_dist.R`: Makes figure 5

### 04_appendix
* `01_make_enrollment_timing_figure.R`: Makes figure A.1
* `02_make_ramp_up_figure.R`: Makes figure A.2
* `03_fine_binned_price_fs.R`: Makes figure A.3
* `04_forward_looking.R`: Makes Table A.2
* `05_alt_cuts.R`: Makes table C.1
* `06_birth_month.R`: Makes table C.2
* `07_power_calc.R`: Makes figure C.3
* `08_risk_calib.R`: Makes figure D.1
* `09_class_specific_response.R`: Makes table D.1
* `10_any_fill.R`: Makes tabe D.3
* `11a_estimate_lit_demand.R`: Estimates for table E.1
* `11b_format_lit_demand.R` Makes table E.1