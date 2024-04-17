# Proj: cost-sharing
# Author: Evan Flack (flack@stanford.edu)
# Desc: Defines data libraries, loads packages, and defines user functions

# Set base working directories
lib_base <- "~/baicker-DUA50589/eflack-dua50589/cost_sharing/"
lib_base_data <- "~/baicker-DUA50589/eflack-dua50589/behavioral_hazard/final/data/"

# Load packages that are always needed
suppressMessages(library(rlang, quietly = TRUE))
suppressMessages(library(data.table, quietly = TRUE))
suppressMessages(library(magrittr, quietly = TRUE))
suppressMessages(library(tictoc, quietly = TRUE))
suppressMessages(library(optparse, quietly = TRUE))
suppressMessages(library(ggplot2, quietly = TRUE))
suppressMessages(library(stringr, quietly = TRUE))

# Define functions
source(paste0(lib_base, "/00_pre_process/def_fxns.R"))
