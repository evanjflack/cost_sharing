#!/bin/bash

# Proj: Cost-Sharing
# Author: Evan Flack (flack@stanford.edu)
# Desc: Runs .sas script in 01_sample direcotry (part 1)

echo ID main/prediction enrollees
sas 01a_id_new_enrollees.sas &

sas 01b_id_dual_benes.sas

wait;

echo Subset Claims
sas 02a_subset_new_enrollee_claims.sas &

sas 02b_subset_dual_claims.sas

wait;

echo ID false enrollees
sas 06a_subset_older_benes_bsf.sas

echo Done SAS part one.
