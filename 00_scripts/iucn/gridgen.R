library(sf)
library(dplyr)
library(purrr)

source("utils.R")
source("config.R")

# -------------------------------
# Fixed origin (India-friendly)
# -------------------------------
origin_lon <- 68
origin_lat <- 6

# -------------------------------
# Snap bbox to grid
# -------------------------------
snap_bbox <- function(bbox, grid_size_deg) {
  
  # Convert to plain numeric (IMPORTANT FIX)
  xmin <- as.numeric(bbox["xmin"])
  ymin <- as.numeric(bbox["ymin"])
  xmax <- as.numeric(bbox["xmax"])
  ymax <- as.numeric(bbox["ymax"])
  
  # Snap to grid
  xmin <- floor((xmin - origin_lon) / grid_size_deg) * grid_size_deg + origin_lon
  ymin <- floor((ymin - origin_lat) / grid_size_deg) * grid_size_deg + origin_lat
  
  xmax <- ceiling((xmax - origin_lon) / grid_size_deg) * grid_size_deg + origin_lon
  ymax <- ceiling((ymax - origin_lat) / grid_size_deg) * grid_size_deg + origin_lat
  
  # Recreate bbox
  st_bbox(c(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), crs = 4326)
}
# -------------------------------
# Main grid generator (OPTIMIZED)
# -------------------------------
generate_grids <- function(grid_size_km, geography) {
  
  grid_size_deg <- grid_size_km / 111
  
  # Snap bbox
  bbox <- snap_bbox(st_bbox(geography), grid_size_deg)
  
  # Create grid
  grid <- st_make_grid(
    st_as_sfc(st_bbox(bbox)),
    cellsize = grid_size_deg,
    square = TRUE
  )
  
  grid_sf <- st_sf(geometry = grid)
  
  # Use centroids (safe for square grids)
  coords <- st_coordinates(st_centroid(grid_sf))
  
  # Vectorised GridID (FAST)
  col <- floor((coords[,1] - origin_lon) / grid_size_deg)
  row <- floor((coords[,2] - origin_lat) / grid_size_deg)
  
  grid_sf$GridID <- paste(grid_size_km, row, col, sep = "_")
  
  # Fast spatial filter (sparse)
  keep <- lengths(st_intersects(grid_sf, geography)) > 0
  grid_sf <- grid_sf[keep, ]
  
  # Keep only ID
  grid_sf <- grid_sf %>% select(GridID)
  
  return(grid_sf)
}

# -------------------------------
# Load India geometry
# -------------------------------
load("..\\..\\00_data\\maps_sf.RData")

india_sf <- st_transform(india_sf, 4326)

if (!st_is_valid(india_sf)) {
  india_sf <- st_make_valid(india_sf)
}

# -------------------------------
# Generate grids
# -------------------------------
grids_sf <- setNames(
  lapply(aoo_grid_sizes_km, function(aoo_grid_sizes_km) {
    generate_grids(aoo_grid_sizes_km, india_sf)
  }),
  aoo_grid_sizes_km
)

# -------------------------------
# Save
# -------------------------------
saveRDS(grids_sf, "in_grids.RDS")