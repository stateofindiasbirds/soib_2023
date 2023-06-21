# necessary packages, functions/scripts, data
library(tidyverse)
library(glue)
library(tictoc)

source("00_scripts/00_functions.R")


my_assignment <- 1:100 # CHANGE FOR YOUR SUBSET


# STEP 1: Create subsampled data files using subsampled GROUP.IDs
# Requires:
# - tidyverse, tictoc
# - data files (ALL in 00_data/):
#   - "dataforanalyses.RData"
#   - "randomgroupids.RData"
# Outputs:
# - "01_analyses_full/dataforsim/dataX.csv" files

load("00_data/analyses_metadata.RData")

cur_mask <- "none" # analysis for full country (not masks)
tictoc::tic(glue("Generated subsampled data for full country (# {min(my_assignment)}:{max(my_assignment)})"))
source("00_scripts/create_random_datafiles.R")
tictoc::toc() 



# STEP 2: Run trends models for all selected species
# Requires:
# - tidyverse, tictoc, lme4, VGAM, parallel, foreach, doParallel
# - data files (ALL in 00_data/):
#   - "dataforanalyses.RData"
#   - "specieslists.RData"
# Outputs:
# - "01_analyses_full/trends/trendsX.csv" files

load("00_data/analyses_metadata.RData")

cur_mask <- "none" # analysis for full country (not masks)
tictoc::tic(glue("Species trends for full country (sims {min(my_assignment)}--{max(my_assignment)})"))
source("00_scripts/run_species_trends.R")
tictoc::toc() 

