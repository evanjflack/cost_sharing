#!/bin/bash

# Proj: Cost-sharing
# Author: Evan Flack (flack@stanford.edu)
# Desc: Runs all scripts in 02_features directory

pct=20pct

echo Mortality outcomes
Rscript 01a_make_new_enrollee_mortality_outcomes.R -p $pct &

Rscript 01b_make_initial_mortality_outcomes.R -p $pct

wait;

echo Claims and cost per month
Rscript 02a_calc_new_enrollee_claims_cost_month.R -p $pct

wait;

echo Initial features
Rscript 03a_create_new_enrollee_initial_part_d_features.R -p $pct &

Rscript 03b_create_dual_initial_part_d_features.R -p $pct

wait;

echo End of year ATC vars
Rscript 04a_determine_most_common_atc_classes.R -p $pct

wait;

Rscript 04b_create_new_enrollee_end_year1_class_variables.R -p $pct &

echo Estimate plan cost sharing
Rscript 05_estimate_plan_cost_sharing.R -p $pct

echo Done
