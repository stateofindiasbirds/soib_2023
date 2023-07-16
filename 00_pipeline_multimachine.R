# necessary packages, functions/scripts, data
library(tidyverse)
library(glue)
library(tictoc)

source("00_scripts/00_functions.R")
load("00_data/analyses_metadata.RData")


# # full country runs -------------------------------------------------------
# 
# my_assignment <- 1:100 # CHANGE FOR YOUR SUBSET
# 
# 
# # STEP 1: Create subsampled data files using subsampled GROUP.IDs
# # Requires:
# # - tidyverse, tictoc
# # - data files:
# #   - "dataforanalyses.RData"
# #   - "randomgroupids.RData"
# # Outputs:
# # - "dataforsim/dataX.csv" files
# 
# cur_mask <- "none" # analysis for full country (not masks)
# tic(glue("Generated subsampled data for full country (# {min(my_assignment)}:{max(my_assignment)})"))
# source("00_scripts/create_random_datafiles.R")
# toc() 
# 
# 
# 
# # STEP 2: Run trends models for all selected species
# # Requires:
# # - tidyverse, tictoc, lme4, VGAM, parallel, foreach, doParallel
# # - data files:
# #   - "dataforsim/dataX.csv"
# #   - "specieslists.RData"
# # Outputs:
# # - "trends/trendsX.csv" files
# 
# cur_mask <- "none" # analysis for full country (not masks)
# tic(glue("Species trends for full country (sims {min(my_assignment)}--{max(my_assignment)})"))
# source("00_scripts/run_species_trends.R")
# toc() 
# 
# 
# mask runs -------------------------------------------------------------------

cur_mask <- "woodland" # CHANGE FOR YOUR MASK {woodland, cropland, ONEland}


# STEP 1: Create subsampled data files using subsampled GROUP.IDs
tic(glue("Generated subsampled data for {cur_mask}"))
source("00_scripts/create_random_datafiles.R")
toc() 


# STEP 2: Run trends models for all selected species
tic(glue("Species trends for {cur_mask}"))
source("00_scripts/run_species_trends.R")
toc() 
