#!/bin/bash

# Proj: Cost-Sharing
# Author: Evan Flack (flack@stanford.edu)
# Desc: Runs R scripts in 01_sample directory

pct=20pct

echo Transforming bsf to long

Rscript 03_transform_bsf_to_long.R -p $pct

wait;

echo Making samples

Rscript 04a_build_new_enrollee_sample.R -p $pct &

Rscript 04b_build_pre_dec_samples.R -p $pct &

Rscript 04c_build_enrollment_month_sample.R -p $pct &

Rscript 04d_build_dual_65_sample.R -p $pct

wait;

echo Id sample plans

Rscript 05a_id_sample_plans.R -p $pct &

echo Making false samples

Rscript 06b_transform_old_disabled_bsf_to_long.R -p $pct

wait;

Rscript 06c_make_old_dis_false_samples.R -p $pct

wait;

echo Done.
