library(tidyverse)
library(glue)
library(tictoc)
# for parallel iterations
library(furrr)
library(parallel)

source("00_scripts/00_functions.R")

interannual_update = TRUE

load("00_data/analyses_metadata.RData")

cur_mask <- "none"
tic(glue("Generated optimized data for full country"))
source("00_scripts/optimize_assignment_datafiles.R")
toc()

cur_mask <- "woodland"
tic(glue("Generated optimized data for {cur_mask}"))
source("00_scripts/optimize_assignment_datafiles.R")
toc() 

cur_mask <- "cropland"
tic(glue("Generated optimized data for {cur_mask}"))
source("00_scripts/optimize_assignment_datafiles.R")
toc()

cur_mask <- "ONEland"
tic(glue("Generated optimized data for {cur_mask}"))
source("00_scripts/optimize_assignment_datafiles.R")
toc()

cur_mask <- "PA"
tic(glue("Generated optimized data for {cur_mask}"))
source("00_scripts/optimize_assignment_datafiles.R")
toc() 

# states
not_my_states <- c(
)

tic("Generated optimized data for all states") # 4 hours for 21 states

analyses_metadata %>% 
  filter(MASK.TYPE == "state") %>% 
  distinct(MASK) %>% 
  filter(!MASK %in% not_my_states) %>% 
  pull(MASK) %>% 
  # walking over each state
  walk(~ {
    
    tic(glue("Generated optimized data for {.x} state"))
    assign("cur_mask", .x, envir = .GlobalEnv)
    source("00_scripts/optimize_assignment_datafiles.R")
    toc()
    
  })

toc()
rm(not_my_states)