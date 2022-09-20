
# Ecoregions2017 https://ecoregions.appspot.com/
# downloaded the shapefiles, now importing as sf object

sf_use_s2(FALSE)
temp <- st_read(dsn = "hist_spread/Ecoregions2017", layer = "Ecoregions2017")

indiamap <- indiamap %>% st_as_sf() %>% dplyr::select(geometry)
st_crs(indiamap) <- st_crs(temp)

ecoregions <- temp %>% 
  dplyr::select("ECO_NAME") %>% 
  st_intersection(indiamap)
