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
# - "rawdata_obs.RData"
# - "rawdata_chk.RData"

tic("Reading and cleaning raw data")
readcleanrawdata(ebdpath = "00_data/ebd_IN_unv_smp_relJun-2026.txt", 
                 samppath = "00_data/ebd_IN_unv_smp_relJun-2026_sampling.txt", 
                 sensitivepath = "00_data/ebd_sensitive_relJun-2026.txt",
                 centroidspath = "00_data/centroids_sanitized_relJun-2026.rds")
toc() # 994s


# for the following steps, there are two data files required, which need to be generated
# from respective scripts if not already existing:
# - "00_data/habmasks_sf.RData": sf map of SoIB2 habitat masks 
#   (also rerun if  habitat masks .json file updated)
# - "00_data/map_DEM.RData": dataframe of DEM (country & states), which is the basemap in range maps


# STEP 2: Add map and grid variables to dataset (dataframe)
# - admin & PA boundaries
# - SoIB2 habitat masks
# - square grids at 5 resolutions (5, 25, 50, 100, 200 km*km), unclipped and clipped to India
# Run:
# - every time habitat masks or other maps are updated
# - every time "rawdata.RData" is updated
# Requires:
# - tidyverse, sf
# - Data (ALL in 00_data/):
#   - "rawdata.RData"
#   - "grids_sf_full.RData", "grids_g0_sf.RData", "maps_sf.RData", "maps_pa_sf.RData"* 
#       See the India Maps repo:
#       https://github.com/birdcountindia/india-maps/blob/main/scripts/create_maps_sf.R
#   - "habmasks_sf.RData" (to be generated separately)
# Outputs:
# - "data.RData"

tic("Adding map and grid variables to dataset")
addmapvars()
toc() # 161s


# STEP 3: Process and filter data for analyses
# Run:
# - every time "data.RData" is updated
# Requires:
# - tidyverse, lubridate
# - data files (ALL in 00_data/):
#   - "data.RData"
#   - "indiaspecieslist.csv" (common and scientific names of all species)
#   - "SoIB_mapping_2024.csv"
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
# 51 min (SoIB 2023)
# 21 min (2024 annual update)
# 12 min (2025 annual update)
# 13 min (2026 annual update)

# PART 3 (run) ------------------------------------------------------------------

# STEP 1: Run trends models for all selected species
# Run:
# - PART 2 is now discontinued
# Requires:
# - tidyverse, tictoc, lme4, VGAM, parallel, foreach, doParallel
# - data files:
#   - "dataforanalyses.RData" for whole country and individual mask versions
#   - "specieslists.RData" for whole country and individual mask versions
# Outputs:
# - several species-specific outputs for whole country and individual mask versions

load("00_data/analyses_metadata.RData")
par_cores = 12
sims_main = 20 # for the bootMer here, no subsampling!
sims_boot = 20
#speciesfortrends = c("Cotton Pygmy-Goose",
#                     "Indian Courser")

cur_mask <- "none"
tic(glue("Species trends for full country"))
source("00_scripts/produce_trends.R")
toc()

cur_mask <- "woodland"
tic(glue("Species trends for {cur_mask}"))
source("00_scripts/produce_trends.R")
toc() 

cur_mask <- "cropland"
tic(glue("Species trends for {cur_mask}"))
source("00_scripts/produce_trends.R")
toc() 

cur_mask <- "ONEland"
tic(glue("Species trends for {cur_mask}"))
source("00_scripts/produce_trends.R")
toc()

cur_mask <- "PA"
tic(glue("Species trends for {cur_mask}"))
source("00_scripts/produce_trends.R")
toc()

not_my_states <- c(
  c("Karnataka")
#  c("Gujarat", "Uttarakhand", "West Bengal", "Maharashtra", "Karnataka", "Kerala", "Tamil Nadu", 
#    "Meghalaya", "Ladakh")
)
tic.clearlog()
tic("Ran species trends for all states")
# Karnataka takes 4.5 min per 1 sim

analyses_metadata %>% 
  filter(MASK.TYPE == "state") %>% 
  distinct(MASK) %>% 
  filter(!MASK %in% not_my_states) %>% 
  #filter(MASK %in% c("Tripura", "Punjab", "Jharkhand", "Nagaland")) %>% 
  pull(MASK) %>% 
  # walking over each state
  walk(~ {
    
    tic(glue("Ran species trends for {.x} state"))
    assign("cur_mask", .x, envir = .GlobalEnv)
    source("00_scripts/produce_trends.R")
    toc(log = TRUE) 
    
  })

toc(log = TRUE, quiet = TRUE) 
tic.log()
#rm(not_my_states)


# STEP 2: Run occupancy analyses (presence-based and model)
# Run:
# - after above step (P3, S1)
# - both analyses run only for full country; for states, only presence-based
# - this is not run for habmasks at all
# Requires:
# - tidyverse, tictoc, glue, parallel, foreach, doParallel
# - data files:
#   - "dataforanalyses.RData" for whole country and individual states
#   - "specieslists.RData" for whole country and individual states
#   - "00_data/SoIB_mapping_2024.csv"
#   - "00_data/grids_sf_nb.RData"
# Outputs: 
# - csv files in occupancy-presence/ 
# - "occupancy-model/chunk_X.csv" for whole country and individual states
load("00_data/analyses_metadata.RData")


# full country
cur_mask <- "none"
source("00_scripts/run_species_occupancy-setup.R")
tic("Ran presence-based occupancy")
source("00_scripts/run_species_occupancy-presence.R")
toc()  
tic("Ran modelled occupancy")
source("00_scripts/run_species_occupancy-model.R")
toc()


# occupancy not run for hab masks at all. both presence and modelled data pulled from full-country.


# states
tic.clearlog()
tic("Ran species occupancy for all states")

analyses_metadata %>% 
  filter(MASK.TYPE == "state") %>%
  distinct(MASK) %>% 
  # slice(1) %>% 
  pull(MASK) %>% 
  # walking over each state
  walk(~ {
    
    assign("cur_mask", .x, envir = .GlobalEnv)

    source("00_scripts/run_species_occupancy-setup.R")
    
    tic(glue("Ran presence-based occupancy for {.x} state"))
    source("00_scripts/run_species_occupancy-presence.R")
    toc(log = TRUE)
    # occupancy models only run for national level data. for hab masks and states, data
    # pulled from full-country.

  })

toc(log = TRUE, quiet = TRUE) 
tic.log() # 483s



# PART 4 (resolve) ------------------------------------------------------------------

# STEP 1: Resolve trends & occupancy for all selected species
# Run:
# - after above steps (P3, S1-2)
# Requires:
# - tidyverse, tictoc, sf, VGAM, writexl
# - data files:
#   - fullspecieslist.csv
#   - trends/trendsX.csv for whole country and individual mask versions
# Outputs: several

load("00_data/analyses_metadata.RData")


tic.clearlog()
tic("Resolved trends & occupancy for all 42 masks")

print(glue("Activated future-walking using advanced Kenbunshoku Haki!"))

# start multiworker parallel session
plan(multisession, workers = parallel::detectCores()/2)

analyses_metadata %>%
  pull(MASK) %>% 
  # future-walking over each mask
  future_walk(.progress = TRUE, .options = furrr_options(seed = TRUE), ~ {
    
    # new environment for each parallel iteration
    cur_env <- new.env()
    assign("cur_mask", .x, envir = cur_env)
    assign("interannual_update", interannual_update, envir = cur_env)
    
    tic(glue("Resolved trends & occupancy for {.x}"))
    source("00_scripts/combine_trends_and_soib_mapping.R", local = cur_env)
    source("00_scripts/combine_occupancy_and_soib_mapping.R", local = cur_env)
    toc()
    
  })

# end multiworker parallel session
plan(sequential)

toc(log = TRUE, quiet = TRUE) 
tic.log()

# STEP 2: Classify using trends and range status, and generate necessary outputs
# Run:
# - after above steps (P4, S1)
# Requires:
# - tidyverse, tictoc, writexl
# - data files:
#   - specieslists.RData
#   - trends/trendsX.csv for whole country and individual mask versions
#   - X/SoIB_main_wocats.csv
# Outputs: several

load("00_data/analyses_metadata.RData")
load("00_data/dataforanalyses_extra_chk.RData")

tic.clearlog()
tic("Finished classifying and summarising for all masks") # 2 min

analyses_metadata %>% 
  pull(MASK) %>% 
  # walking over each mask
  walk(., ~ {
    
    assign("cur_mask", .x, envir = .GlobalEnv)
    
    tic(glue("Finished classifying and summarising for {.x}"))
    source("00_scripts/classify_and_summarise.R")
    toc(log = TRUE)
    
  })

toc(log = TRUE, quiet = TRUE) 
tic.log()