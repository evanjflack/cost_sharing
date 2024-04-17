## Prediction

This directory contains scripts to predict cumulative spending/acute events 
using an ensemble of LASSO and gradient boosted trees (GBM). All models are fit
in a training sample of dual enrollees, who do not face cost sharing. The 
"betas" from this training are then applied to the analytic sample of new 
enrollees.

### 01_prep_data
Preps the outcomes (for dual training sample) and features matricies (for both
dual/new enrollee samples).

* `01a_prep_demographic_features.R`: makes sex, age, and state variables
* `01b_prep_initial_spending.R`: makes initial spending predictors
* `01d_id_treated.R`: identifies prediction sample individuals that were treated in the initial period (having a claim in a specific class)
* `02a_prep_ids.R`: Preps ids for the dual prediction sample (outcomes + sample selection criteria)
* `02b_prep_new_enrollee_model_data.R`: combines features in main analytic sample
* `02c_prep_dual_model_data.R`: combines features in prediction sample

### 02_tune_fit_predict
* `01a_tune_lasso.R`: tunes lasso lambda parameters in training set
* `01b_predict_lasso_oos.R`: fits lasso in training set then predicts in ensemble set and main analytic sample
* `02a_tune_gbm.R`: tunes gbm hyperparameters in training set
* `02b_predict_gbm_oos.R`: fits gbm in training set then predicts in ensemble set and main analytic sample
* `03_predict_ensemble_oos`: fits ensemble weights in ensemble set, then predicts in main analytic sample