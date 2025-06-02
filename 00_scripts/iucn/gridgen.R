library(sf)
library(dplyr)
library(purrr)

source("utils.R")
source("config.R")

generate_grids <- function(grid_size_km, geography) {
  # Define grid size in degrees
  grid_size_deg <- grid_size_km / 111  # Assume 1 degree ~ 111 km
  
  # Create a grid spanning a fixed range (Kerala)
  bbox <- st_bbox(c(xmin = 74.5, xmax = 77.5, ymin = 8, ymax = 13), crs = 4326)
  
  grid <- st_make_grid(
    st_as_sfc(st_bbox(bbox)), 
    cellsize = c(grid_size_deg, grid_size_deg),
    square = TRUE
  )
  
  # Convert grid to sf object
  grid_sf <- st_sf(geometry = grid)
  
  # Compute the centroid of each grid cell
  centroids <- st_centroid(grid_sf)
  
  # Extract lat/lon from centroids
  lat_lon <- st_coordinates(centroids)
  
  # Generate grid_id for each centroid
  grid_sf$GridID <- mapply(compute_grid_id, lat_lon[, 2], lat_lon[, 1], MoreArgs = list(grid_size_km = grid_size_km))
  
  grid_sf <- st_join(grid_sf, geography, join = st_intersects) %>% filter (STATE_CODE == geography$STATE_CODE)
  grid_sf %>% select (GridID) 
  
  return(grid_sf)
}

kerala_map <- st_read("..\\data\\states_sf_admin_mapped/states_sf_admin_mapped.shp") %>% filter (STATE_NAME == "Kerala")

# Generate the global grid
grids_sf <- setNames(
  lapply(grid_sizes_km, function(grid_size_km) generate_grids(grid_size_km, kerala_map)),
  grid_sizes_km
)

names(grids_sf) <- grid_sizes_km

saveRDS(grids_sf, "kl_grids.RDS")