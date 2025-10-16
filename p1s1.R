library(tidyverse)
library(glue)
library(tictoc)

source("00_scripts/00_functions.R")

# Is the current run for a new major SoIB version (every 3-4 years), 
# or for an interannual update (every year between major versions)?
# testing git
interannual_update = TRUE

# PART 0 (paths) ----------------------------------------------------------
source("00_scripts/01_create_metadata.R")

# PART 1 (prepare) ------------------------------------------------------------------
# Run each step in order. May start from in between but progress sequentially down.
# STEP 1: Sequence of steps to clean data starting from .txt file:
# - clean the eBird EBD
# - remove unnecessary columns
# - add necessary columns
# Run:
# - after EVERY new EBD download
# Requires:
# - tidyverse, lubridate
# - EBD .txt file MUST BE in the working directory
# Outputs:
# - "indiaspecieslist.csv" (common and scientific names of all species)
# - "eBird_location_data.csv"
# - "rawdata.RData"
tic("Reading and cleaning raw data")
readcleanrawdata(rawpath = "00_data/ebd_IN_unv_smp_relAug-2025.txt",
                 sensitivepath = "00_data/ebd_sensitive_relAug-2025_IN.txt")
toc()
