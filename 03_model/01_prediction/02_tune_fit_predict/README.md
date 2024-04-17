## Ensemble Model Tuning

These scripts tune the hyperparameters of an ensemble model comprised of LASSO and GBM models in the tuning set. They also find the optimal ensemble weights in an ensemble set and evaluate model performance in a test set. Right now, this directory is used to predict both spending and cardiac event risk in the `enroll_month` identification strategy.

* `00_tune_ensemble.sh`: runs all scripts in the directory
* `01_fit_ols.R`: fits ols in the training set
* `02_fit_lasso.R`: tunes lasso for lambda in training set
* `03_tune_gbm.R`: tunes gbm over multiple hyperparameters in training set
* `04_fit_gbm.R`: fits gbm based on optimal parameters in training set
* `05_predict_ensemble_components.R`: predicts ols/lasso/gbm in all sets
* `06_train_ensemble.R`: uses ensemble set to find optimal weights
* `07_predict_ensemble.R`: predicts ensemble model in all sets
* `08_evaluate_prediction.R`: evaluates fit of ols/lasso/gbm/ensemble in both training and test sets
