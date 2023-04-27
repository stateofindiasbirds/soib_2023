library(tidyverse)
library(sf)

load("dataforanalyses.RData")
source("https://github.com/birdcountindia/bci-functions/raw/main/mapping.R") # for join mapvars

load("maps_sf.RData")
load("grids_g0_sf.RData") # just g0 and clipped g0 as separate RData due to size

sf_use_s2(FALSE) # turn off spherical geometries, because that results in errors


# joining mapvars (sf) ###
# this step needs to be done much before, just replace old map-join step
# needs LATITUDE, LONGITUDE, COUNTY, STATE, SAMPLING.EVENT.IDENTIFIER
# new columns: GRID.G1, GRID.G2, GRID.G3, GRID.G4, DISTRICT.NAME, STATE.NAME
# can rename to: gridg1, gridg2, gridg3, gridg4, DISTRICT, ST_NM
data <- data %>% join_map_sf()

# for now, will assume old grid IDs map correctly to new IDs

# dataframe of 720 trended species ----------------------------------------

# getting list of 720 trended species
trends <- read.csv("trends.csv")

spec_rangecov <- trends %>% distinct(COMMON.NAME, timegroups, timegroupsf)

# finding covered range of species
temp <- data %>% 
  # only need the trended species
  filter(COMMON.NAME %in% spec_rangecov$COMMON.NAME) %>% 
  # overall range
  group_by(COMMON.NAME) %>% 
  mutate(TOT.G1 = n_distinct(gridg1),
         TOT.G3 = n_distinct(gridg3)) %>% 
  group_by(COMMON.NAME, timegroups, TOT.G1, TOT.G3) %>% 
  dplyr::summarise(TIME.G1 = n_distinct(gridg1),
                   TIME.G3 = n_distinct(gridg3)) %>% 
  ungroup() %>% 
  mutate(RANGE.PERC.G1 = 100*TIME.G1/TOT.G1,
         RANGE.PERC.G3 = 100*TIME.G3/TOT.G3) %>% 
  rename(timegroupsf = timegroups) %>% 
  distinct(COMMON.NAME, timegroupsf, RANGE.PERC.G1, RANGE.PERC.G3)

spec_rangecov <- spec_rangecov %>% left_join(temp)
  

# subgrid coverage --------------------------------------------------------


# linking g1 and g0
grid_link <- g1_in_sf %>% 
  st_join(g0_in_sf, join = st_covers) %>% 
  st_drop_geometry() %>% 
  distinct(GRID.G1, GRID.G0) %>% 
  # getting total subgrids in each 25x25 cell
  group_by(GRID.G1) %>% 
  mutate(TOT.G0 = n_distinct(GRID.G0)) 


# ideally, use lat-long of data to join G0 cells
data %>% 
  distinct(SAMPLING.EVENT.IDENTIFIER, COUNTY, STATE, LONGITUDE, LATITUDE) %>% 
  # sensitive species haven't got district/admin updates so showing up with different values
  group_by(SAMPLING.EVENT.IDENTIFIER) %>% 
  slice(1) %>% # joining map vars to EBD
  ungroup() %>% 
  st_as_sf(coords = c("LONGITUDE", "LATITUDE"), remove = F) %>% 
  st_set_crs(st_crs(g0_in_sf)) %>% 
  st_join(g0_in_sf %>% dplyr::select(GRID.G0, GEOM.G0)) %>% 
  # dropping GEOM (sf polygon), only need IDs
  st_drop_geometry()


spec_subgridcov <- data %>% 
  distinct(COMMON.NAME, timegroups, gridg1, GRID.G0) %>% 
  rename(timegroupsf = timegroups)
