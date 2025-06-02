compute_grid_id <- function(lat, lon, grid_size_km) {
  # Convert latitude and longitude to grid number
  lat_grid <- floor(lat / (grid_size_km / 111))
  lon_grid <- floor(lon / (grid_size_km / 111)) 
  
  # Combine lat_grid and lon_grid into a unique grid ID
  return(lat_grid * 100000 + lon_grid) # Combine as a unique integer
}



