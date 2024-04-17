#!/bin/bash

# Proj: Cost-sharing
# Author: Evan Flack (flack@stanford.edu)
# Desc: Runs all drug inforation scripts

# These have been commented out and their output has been saved in the data
# directory. Uncomment if you would like to re-quety Rxnorm for drug information

# Rscript 01_id_unique_ndc9.R
#
# Rscript 02_create_ndc_atc_xwalk.R
pct=20pct

echo ATC indicators
Rscript 03_get_atc_indicators.R -p $pct
