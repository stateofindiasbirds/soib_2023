library(tidyverse)
library(glue)
library(tictoc)

source("00_scripts/00_functions.R")

# Is the current run for a new major SoIB version (every 3-4 years), 
# or for an interannual update (every year between major versions)?
# testing git
interannual_update = TRUE

# PART 1 (prepare) ------------------------------------------------------------------

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
toc()
