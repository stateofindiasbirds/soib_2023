library(tidyverse)
library(glue)
library(tictoc)
# for parallel iterations
library(furrr)
library(parallel)

source("00_scripts/00_functions.R")

# Is the current run for a new major SoIB version (every 3-4 years), 
# or for an interannual update (every year between major versions)?
# testing git
interannual_update = TRUE

# PART 1 (prepare) ------------------------------------------------------------------

# STEP 3: Process and filter data for analyses
# Run:
# - every time "data.RData" is updated
# Requires:
# - tidyverse, lubridate
# - data files (ALL in 00_data/):
#   - "data.RData"
#   - "indiaspecieslist.csv" (common and scientific names of all species)
#   - "SoIB_mapping_2023.csv"
# Outputs:
# - "dataforanalyses_extra.RData"
# - "fullspecieslist.csv" for whole country and individual mask versions
# - "sub_samp_locs.csv" for whole country and individual mask versions
# - "dataforanalyses.RData" for whole country and individual mask versions, which contains:
#   - info on amount of data in each temporal bin
#   - full species list (with all attribute columns)
#   - selected species list
#   - data
# - "specieslists.RData" for whole country and individual mask versions
load("00_data/analyses_metadata.RData")
tic("Processing and filtering data for analyses")
source("00_scripts/filter_data_for_species.R")
toc() 
