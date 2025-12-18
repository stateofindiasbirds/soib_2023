library(tidyverse)
library(glue)
library(tictoc)
# for parallel iterations
library(furrr)
library(parallel)
library(dplyr)
library(magrittr)

source("00_scripts/00_functions.R")

interannual_update = TRUE

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