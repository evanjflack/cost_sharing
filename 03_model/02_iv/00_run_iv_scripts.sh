#!/bin/bash

# Proj: Cost-Sharing
# Author: Evan Flack (flack@stanford.edu)
# Desc: Runs all scripts to generate figures in main text/appendix

# Prep Data

pct=20pct

cd 01_prep_data

echo 01_make_final_sample.R
Rscript 01_make_final_sample.R -p $pct

wait;

echo 02_prep_spending_data.R
Rscript 02_prep_spending_data.R -p $pct &

echo 03_prep_co_rate_data.R

Rscript 03_prep_co_rate_data.R -p $pct

wait;

# Tables

cd ../02_tables

echo 01_sum_stats.R
Rscript 01_sum_stats.R -p $pct &

echo 02a_dem_balance.R
Rscript 02a_dem_util_balance.R -p $pct &

echo 02b_initial_mort_balance.R
Rscript 02b_initial_mort_balance.R -p $pct &

echo 03a_mort_rf.R
Rscript 03a_mort_rf.R -p $pct &

echo 03b_mort_false.R
Rscript 03b_mort_false.R -p $pct &

echo  04_estimate_util_by_class_risk.R
Rscript 04_estimate_util_by_class_risk.R -p $pct

wait;


# # Figures
cd ../03_figures
echo 01_survey.R
Rscript 01_survey.R &

echo 02a_part_d_design.R
Rscript 02a_part_d_design.R &

echo 02b_cov_arm_by_month.R
Rscript 02b_cov_arm_by_month.R &

echo 03_arm_util_mort_rf_figure.R
Rscript 03_arm_util_mort_rf_figure.R &

echo 04a_estimate_year_1_2_gap_util_mort_rf.R
Rscript 04a_estimate_year_1_2_gap_util_mort_rf.R

wait;

echo 04b_plot_year_1_2_gap_util_mort_rf.R
Rscript 04b_plot_year_1_2_gap_util_mort_rf.R &

wait;

echo 05a_estimate_oos_false.R (dual)
Rscript 05a_estimate_oos_false.R -t dual

echo 05a_estimate_oos_false.R (dis)
Rscript 05a_estimate_oos_false.R -t dis

echo 05a_estimate_oos_false.R (non dual)
Rscript 05a_estimate_oos_false.R -t non_dual

echo 05b_estimate_ws_false.R
Rscript 05b_estimate_ws_false.R

echo 05c_estimate_dual_sub_sample_false.R
Rscript 05c_estimate_dual_sub_sample_false.R

echo 05d_plot false_dist.R
Rscript 05d_plot_false_dist.R

wait;

# Appendix

cd ../04_appendix

echo 01_make_enrollment_timing_figure.R
Rscript 01_make_enrollment_timing_figure.R -p $pct &

echo 02_make_ramp_up_figure.R
Rscript 02_make_ramp_up_figure.R -p $pct &

echo 03_fine_binned_price_fs.R
Rscript 03_fine_binned_price_fs.R -p $pct &

echo 04_forward_looking.R
Rscript 04_forward_looking.R -p $pct  &

echo 05_alt_cuts.R
Rscript 05_alt_cuts.R  -p $pct &

echo 06_birth_month.R
Rscript 06_birth_month.R -p $pct &

wait;

echo 07_power_calc.R
Rscript 07_power_calc.R -p $pct &

echo 08_risk_calib.R
Rscript 08_risk_calib.R -p $pct &

echo 09_class_specific_response.R
Rscript 09_class_specific_response.R -p $pct &

echo 10_any_fill.R
Rscript 10_any_fill.R -p $pct &

echo 11a_estimate_lit_demand.R
Rscript 11a_estimate_lit_demand.R -p $pct &

wait;

echo 11b_format_lit_demand.R
11b_format_lit_demand.R -p $pct &

wait;

echo Done
