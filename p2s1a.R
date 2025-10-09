library(tidyverse)
library(glue)
library(tictoc)
# for parallel iterations
library(furrr)
library(parallel)
library(dplyr)
library(magrittr)

source("00_scripts/00_functions.R")

# Is the current run for a new major SoIB version (every 3-4 years),
# or for an interannual update (every year between major versions)?
# testing git
interannual_update = TRUE

# PART 2 (subsample) ------------------------------------------------------------------

# Preparing data for trends analysis

# STEP 1: Subsample data for locations (create set of randomly selected GROUP.IDs)
# a file with random GROUP.IDs is first created so that the more time consuming step (creating the data files)
# can be repeated without sampling a different set of GROUP.IDs each time
# Run:
# - every time "sub_samp_locs.csv" is updated
# Requires:
# - tidyverse, parallel, foreach, doParallel, tictoc
# - data files:
#   - "sub_samp_locs.csv" for whole country and individual mask versions
# Outputs:
# - "rgids-[1-1000].RData" for whole country and individual mask versions,
#                          one each per assignment

load("00_data/analyses_metadata.RData")

# not functionising because parallelisation doesn't work inside functions
cur_mask <- "none"
tic("generated random group IDs for full country")
source("00_scripts/create_random_groupids_memrun_opt.R")
toc()

cur_mask <- "woodland"
tic("generated random group IDs for woodland")
source("00_scripts/create_random_groupids_memrun_opt.R")
toc()

cur_mask <- "cropland"
tic("generated random group IDs for cropland")
source("00_scripts/create_random_groupids_memrun_opt.R")
toc()

cur_mask <- "ONEland"
tic("generated random group IDs for ONEland")
source("00_scripts/create_random_groupids_memrun_opt.R")
toc()

cur_mask <- "PA"
tic("generated random group IDs for PA")
source("00_scripts/create_random_groupids_memrun_opt.R")
toc()

# states
tic("generated random group IDs for all states") # 91 min

analyses_metadata %>%
  filter(MASK.TYPE == "state") %>%
  distinct(MASK) %>%
  pull(MASK) %>%
  # walking over each state
  walk(~ {

    assign("cur_mask", .x, envir = .GlobalEnv)
    source("00_scripts/create_random_groupids_memrun_opt.R")

  })

toc()
