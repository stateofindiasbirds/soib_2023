library(tidyverse)
library(glue)
library(tictoc)
# for parallel iterations
library(furrr)
library(parallel)

source("00_scripts/00_functions.R")

interannual_update = TRUE

# STEP 3: Process and filter data for analyses

load("00_data/analyses_metadata.RData")
tic("Processing and filtering data for analyses")
source("00_scripts/filter_data_for_species.R")
toc()