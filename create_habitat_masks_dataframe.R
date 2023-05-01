library(tidyverse)
library(geojsonsf)
library(sf)

habmasks_sf <- geojson_sf("soibV2GridsIN_habMasksAdded.geojson") %>% 
  # removing unnecessary columns
  mutate(woodland = NULL, cropland = NULL, one = NULL) %>% 
  st_drop_geometry()

save(habmasks_sf, file = "habmasks_sf.RData")
