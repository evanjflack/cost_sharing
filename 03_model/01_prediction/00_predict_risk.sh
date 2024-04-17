#!/bin/bash

# Proj: Cost-Sharing
# Author: Evan Flack (flack@stanford.edu)
# Desc: Runs scripts to make risk predictions

cd 01_prep_data
echo Start prepping data.
00_prep_ml_data.sh
echo Done prepping data.

cd ../02_tune_fit_predict
echo Start tune, fit, predict.
00_tune_fit_predict.sh
echo Done tune, fit, predict.
