compute_grid_id <- function(lat, lon, grid_size_km,
                            origin_lon = 68,
                            origin_lat = 6) {
  
  grid_size_deg <- grid_size_km / 111
  
  # Vectorised NA handling
  lat_na <- is.na(lat)
  lon_na <- is.na(lon)
  na_idx <- lat_na | lon_na
  
  # Compute indices
  lat_grid <- floor((lat - origin_lat) / grid_size_deg)
  lon_grid <- floor((lon - origin_lon) / grid_size_deg)
  
  # Combine into integer ID
  grid_id <- lat_grid * 100000 + lon_grid
  
  # Assign NA where needed
  grid_id[na_idx] <- NA_integer_
  
  return(grid_id)
}