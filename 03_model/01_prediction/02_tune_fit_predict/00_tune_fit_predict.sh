#!/bin/bash

# Proj: Cost-sharing
# Author: Evan Flack (flack@stanford.edu)
# Desc: Script to tune ensemble model components and fit/predict OOS

pct=20pct

# Mortality
resp_var=mort
population=all

echo 01a_tune_lasso
Rscript 01a_tune_lasso.R -r $resp_var -u $population  -p $pct &

echo 02a_tune_gbm
Rscript 02a_tune_gbm.R  -r $resp_var -u $population  -p $pct &

wait;

echo 01b_predict_lasso_oos
Rscript 01b_predict_lasso_oos.R -r $resp_var -u $population  -p $pct &

echo 02b_predict_gbm_oos
Rscript 02b_predict_gbm_oos.R -r $resp_var -u $population  -p $pct &

wait;

echo 03_predict_ensemble_oos
Rscript 03_predict_ensemble_oos.R -r $resp_var -u $population  -p $pct

wait;

# Caridovasular
resp_var=comp_card
population=untreated_any_card

echo 01a_tune_lasso
Rscript 01a_tune_lasso.R -r $resp_var -u $population  -p $pct &

echo 02a_tune_gbm
Rscript 02a_tune_gbm.R  -r $resp_var -u $population  -p $pct &

wait;

echo 01b_predict_lasso_oos.
Rscript 01b_predict_lasso_oos.R -r $resp_var -u $population  -p $pct &

echo 02b_predict_gbm_oos
Rscript 02b_predict_gbm_oos.R -r $resp_var -u $population  -p $pct &

wait;

echo 03_predict_ensemble_oos
Rscript 03_predict_ensemble_oos.R -r $resp_var -u $population  -p $pct

wait;

# Diabetes
resp_var=comp_diab
population=untreated_any_diab

echo 01a_tune_lasso
Rscript 01a_tune_lasso.R -r $resp_var -u $population  -p $pct &

echo 02a_tune_gbm
Rscript 02a_tune_gbm.R  -r $resp_var -u $population  -p $pct &

wait;

echo 01b_predict_lasso_oos.
Rscript 01b_predict_lasso_oos.R -r $resp_var -u $population  -p $pct &

echo 02b_predict_gbm_oos
Rscript 02b_predict_gbm_oos.R -r $resp_var -u $population  -p $pct &

wait;

echo 03_predict_ensemble_oos
Rscript 03_predict_ensemble_oos.R -r $resp_var -u $population  -p $pct

wait;

# Respiratory
resp_var=comp_resp
population=untreated_resp

echo 01a_tune_lasso
Rscript 01a_tune_lasso.R -r $resp_var -u $population  -p $pct &

echo 02a_tune_gbm
Rscript 02a_tune_gbm.R  -r $resp_var -u $population  -p $pct &

wait;

echo 01b_predict_lasso_oos.
Rscript 01b_predict_lasso_oos.R -r $resp_var -u $population  -p $pct &

echo 02b_predict_gbm_oos
Rscript 02b_predict_gbm_oos.R -r $resp_var -u $population  -p $pct &

wait;

echo 03_predict_ensemble_oos
Rscript 03_predict_ensemble_oos.R -r $resp_var -u $population  -p $pct


