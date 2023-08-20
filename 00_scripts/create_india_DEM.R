library(tidyverse)
library(raster)
library(tictoc)
library(sf)

load("00_data/maps_sf.RData")


# import DEM TIF (our basemap)
map_dem_base <- brick("00_data/IndiaDEM-Colour.tif")

# making sure same CRS
st_crs(map_dem_base) == st_crs(india_sf)


# India (<20 sec) 

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


# writing 
save(map_dem_in, file = "00_data/map_DEM.RData")
