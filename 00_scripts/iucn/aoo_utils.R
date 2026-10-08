compute_bbox_diagonal <- function(
    lon_min, lon_max,
    lat_min, lat_max
) {
  
  lon_span_km <- (lon_max - lon_min) * 111 * cos(pi * (lat_min + lat_max) / 360)
  lat_span_km <- (lat_max - lat_min) * 111
  
  sqrt(lon_span_km^2 + lat_span_km^2)
}

compute_grid_id <- function(lat, lon, grid_size_km,
                            origin_lon = 68,
                            origin_lat = 6) {
  
  grid_size_deg <- grid_size_km / 111
  
  lat_na <- is.na(lat)
  lon_na <- is.na(lon)
  na_idx <- lat_na | lon_na
  
  row <- floor((lat - origin_lat) / grid_size_deg)
  col <- floor((lon - origin_lon) / grid_size_deg)
  
  grid_id <- paste(grid_size_km, row, col, sep = "_")
  
  grid_id[na_idx] <- NA_character_
  
  return(grid_id)
}

compute_grid_number <- function(lat, lon, extent_km, protocol, grid_size_km) {
  # Check if the distance is greater than the grid size
  return (
    ifelse ( (protocol != 'Stationary' & protocol != 'Traveling') | (protocol == 'Traveling' & extent_km > grid_size_km), 
             NA_character_,
             {
               compute_grid_id (lat, lon, grid_size_km)
             }))
}

bbox_grid_ids <- function(
    lon_min, lon_max,
    lat_min, lat_max,
    grid_size_km,
    origin_lon = 68,
    origin_lat = 6
) {
  
  if (
    is.na(lon_min) ||
    is.na(lon_max) ||
    is.na(lat_min) ||
    is.na(lat_max)
  ) {
    return(character(0))
  }
  
  grid_size_deg <- grid_size_km / 111
  
  row_min <- floor((lat_min - origin_lat) / grid_size_deg)
  row_max <- floor((lat_max - origin_lat) / grid_size_deg)
  
  col_min <- floor((lon_min - origin_lon) / grid_size_deg)
  col_max <- floor((lon_max - origin_lon) / grid_size_deg)
  
  expand.grid(
    row = row_min:row_max,
    col = col_min:col_max
  ) %>%
    transmute(
      GridID = paste(grid_size_km, row, col, sep = "_")
    ) %>%
    pull(GridID)
}

# Forcing gridID type before joins
force_gridid <- function(df) {
  df %>%
    mutate(GridID = as.character(GridID))
}
