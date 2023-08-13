# map of India with grids and points

library(tidyverse)
library(sf)
library(spdep)
library(glue)
# library(raster)

load("00_data/dataforanalyses_extra.RData")
load("00_data/maps_sf.RData")

### settings ###

write_path <- "02_graphs/00_methods/"
if (!dir.exists(write_path)) (dir.create(write_path, recursive = TRUE))

points_col <- "#347895" 


# filter data for points
# convert to sf
points_sf = data0 %>% 
  distinct(LOCALITY.ID, LONGITUDE, LATITUDE) %>% 
  st_as_sf(coords = c("LONGITUDE", "LATITUDE")) %>% 
  st_set_crs(st_crs(india_sf))


### create and write plot ###


methods_map <- ggplot() +
  geom_sf(data = points_sf, colour = points_col, size = 0.5) +
  geom_sf(data = india_sf, colour = "black", fill = NA) +
  geom_sf(data = g1_in_sf, colour = "grey", fill = NA) +
  geom_sf(data = g3_in_sf, colour = "black", fill = NA, linewidth = 0.5) +
  theme_void() +
  coord_sf(clip = "off")


ggsave(plot = methods_map, filename = glue("{write_path}india_grids_points.png"), 
       units = "in", width = 8, height = 11, dpi = 500, bg = "transparent")
