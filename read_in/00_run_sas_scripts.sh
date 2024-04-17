#!/bin/bash

# Proj: Cost-Sharing
# Author: Evan Flack (flack@stanford.edu)
# Desc: Runs .sas script in read_in directory

echo read_in scripts

sas read_in_bsf.sas

sas read_in_pde.sas

sas read_in_ip.sas

sas read_in_pln.sas

wait;

echo Done.
