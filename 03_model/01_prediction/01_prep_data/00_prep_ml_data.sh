#!/bin/bash

# Proj: Cost-Sharing
# Author: Evan Flack (flack@stanford.edu)
# Desc: Makes features for risk prediction model

pct=20pct

echo Dem features
Rscript 01a_prep_demographic_features.R -s new_enrollee -p $pct &

Rscript 01a_prep_demographic_features.R -s dual_pred -p $pct &

echo Initial spending
Rscript 01b_prep_initial_spending.R -s new_enrollee -p $pct &

Rscript 01b_prep_initial_spending.R -s dual_pred -p $pct &

echo ID treated
Rscript 01d_id_treated.R -p $pct &

echo acute outcomes
Rscript 01e_id_dual_acute_outcomes.R -p $pct &

echo mort outcomes
Rscript 01f_make_dual_mort_outcomes.R -p $pct

wait;

echo Prep Ids
Rscript 02a_prep_ids.R -p $pct

wait

echo Prep final data
Rscript 02b_prep_new_enrollee_model_data.R -p $pct &

Rscript 02c_prep_dual_model_data.R -p $pct

wait;

echo Done.
