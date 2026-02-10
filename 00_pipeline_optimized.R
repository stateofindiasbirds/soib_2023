# Is the current run for a new major SoIB version (every 3-4 years), 
# or for an interannual update (every year between major versions)?

# interannual_update needs to set in each file!

# According to theold pipeline and the pipeline in 
# https://esajournals.onlinelibrary.wiley.com/doi/10.1002/ecs2.70290

# Run each step in order. May start from in between but progress sequentially down.

## PART 1 (prepare) ------------------------------------------------------------------

## STEP 1

# After git clone, you need to copy two data files to 00_data:
  
# - ebd_IN_unv_smp_relAug-2025.txt
# - 00_data/ebd_sensitive_relAug-2025_IN.txt

# The exact names may differ, and are in p1s1.R

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

# - "rawdata.RData"

# For the next step to *not* fail, you need to up your stack limit:
  
# $ ulimit -s unlimited

# Running this also overwrites these files which are under source control:
  
# - 00_data/analyses_metadata.RData
# - 00_data/current_soib_migyears.RData
# - 00_data/eBird_location_data.csv
# - 00_data/indiaspecieslist.csv

source("p1s1.R")

## STEP 2

# Copy protected area map - maps_pa_sf.RData

# Note this step does not change any tracked files

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

source("p1s2.R")

## STEP 3: Process and filter data for analyses

# Running p1s3.R changes these files, which are all tracked under git

# fullspecieslist.csv in all masks

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

source("p1s3.R")

# PART 2 (subsample) ------------------------------------------------------------------

# Preparing data for trends analysis

# This is a variant of the original Part 2, Step 1. Everything is made ready to run the next step

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

source("p2s1a.R")

# This is a variant of the original Part 2, Step 2.

# Step 2: Everything is ready to run the next step. Unlike the original
# code, this does not generate remapped data. It merely optimizes
# the data for each mask. Runtime uses the optimized data and
# randomgroupids which are stored separately to get to the actual
# data

# Create subsampled data files using subsampled GROUP.IDs

# Run:

# - after above step (P2, S1)

# Requires:

# - tidyverse, tictoc
# - data files:
#   - "dataforanalyses.RData" for whole country and individual mask versions

# Outputs:

# For whole country and individual mask versions
# - dataforanalyses.RData-data_opt
# - dataforanalyses.RData-metadata
# - species_names.RData
# - timegroups.RData

source("p2s2a.R")

# PART 3 (run) ------------------------------------------------------------------

# STEP 1: Run trends models for all selected species

# Requires:

# - tidyverse, tictoc, lme4, VGAM, parallel, foreach, doParallel
# - data files:
#   - "dataforanalyses.RData-data_opt"
#   - "dataforanalyses.RData-metadata"
#   - "specieslists.RData"

# Outputs:

# - "trends/trendsX.csv" files

# This computes the species trends. It is a compute intensive step.

# The configuration for this step is in
# config/localhost/config.R
# This is where species, masks, assignments, threads, etc. can be set

# By default, 1 assignment of the entire country is run

source("p3s1a.R")
