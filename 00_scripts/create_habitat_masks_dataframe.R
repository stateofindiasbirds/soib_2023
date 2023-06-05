library(tidyverse)
library(geojsonsf)
library(sf)

habmasks_sf <- geojson_sf("00_data/soibV2GridsIN_habMasksAdded.geojson") %>% 
  # removing unnecessary columns
  mutate(woodland = NULL, cropland = NULL, one = NULL) %>% 
  st_drop_geometry()

save(habmasks_sf, file = "00_data/habmasks_sf.RData")
