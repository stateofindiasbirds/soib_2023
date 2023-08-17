library(tidyverse)
library(raster)
library(tictoc)
library(glue)
library(sf)


load("00_data/analyses_metadata.RData")
load("00_data/maps_sf.RData")
rm(g1_in_sf, g2_in_sf, g3_in_sf, g4_in_sf, india_buff_sf, dists_sf, grid_sizes_deg, grid_sizes_km)

# to join mask codes
source("00_scripts/20_functions.R")

# import DEM TIF (our basemap)
map_dem_base <- brick("00_data/IndiaDEM-Colour.tif")

# making sure same CRS
st_crs(map_dem_base) == st_crs(india_sf)


# India (<20 sec) --------------------------------------------------------------------

tic("Processed TIF basemap for India")

# further processing
map_dem_in <- map_dem_base %>% 
  raster::mask(india_sf) %>%
  as.data.frame(xy = TRUE) %>% 
  mutate(across(starts_with("IndiaDEM.Colour"), ~ . / 255)) %>% 
  magrittr::set_colnames(c("x", "y", "r", "g", "b")) %>% 
  replace_na(replace = list(r = 0, g = 0, b = 0)) %>% 
  # getting hexcodes based on RGB values, and changing blacks (zeroes) to NA
  mutate(codes = rgb(r, g, b) %>% replace(., . == "#000000", NA))

toc()


# states (<2 min) --------------------------------------------------------------------

states_metadata <- analyses_metadata %>% 
  filter(MASK.TYPE == "state")%>% 
  join_mask_codes() %>% 
  distinct(MASK.CODE, MASK)

# walking over each state
walk2(states_metadata$MASK.CODE, states_metadata$MASK, ~ {
  
  # name to be used for current state's dem object
  cur_name <- glue("map_dem_{.x}")
  
  # state to be masked to
  cur_state_sf <- states_sf %>% filter(STATE.NAME == .y)

  tic(glue("Processed TIF basemap for {.y} state"))
  
  # further processing
  map_dem_state <- map_dem_base %>% 
    raster::mask(cur_state_sf) %>%
    raster::crop(cur_state_sf) %>% # "clip" to state's extent
    as.data.frame(xy = TRUE) %>% 
    mutate(across(starts_with("IndiaDEM.Colour"), ~ . / 255)) %>% 
    magrittr::set_colnames(c("x", "y", "r", "g", "b")) %>% 
    replace_na(replace = list(r = 0, g = 0, b = 0)) %>% 
    # getting hexcodes based on RGB values, and changing blacks (zeroes) to NA
    mutate(codes = rgb(r, g, b) %>% replace(., . == "#000000", NA))
  
  toc()
  
  # assign current state sf to environment
  assign(x = cur_name, value = map_dem_state, envir = .GlobalEnv)
  
})



# writing ---------------------------------------------------------------------------

rm(analyses_metadata, map_dem_base, states_sf, india_sf)

# also states_metadata
save.image(file = "00_data/map_DEM.RData")

