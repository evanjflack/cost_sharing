#!/bin/bash

# Proj: Cost-Sharing
# Author: Evan Flack (flack@stanford.edu)
# Desc: Runs .sas script in 01_sample direcotry (part 2)

pct=20pct

echo ID benes in sample plans

sas 05b_id_plan_benes_in_bsf.sas -p $20pct

echo Subset plan PDE

sas 05c_subset_plan_pde.sas -p $20pct

echo Calculate old and dis initial spending

sas 06d_calc_old_dis_initial_spending.sas -p $20pct

echo Done.
