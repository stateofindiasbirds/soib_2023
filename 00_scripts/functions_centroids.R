# Helper functions

library(sf)
library(tidyverse)


# Determine if points fall within a specific region ----
get_location_status <- function(df, lon_col, lat_col, region_sf) {
  sf_obj <- tryCatch({
    st_as_sf(df, coords = c(lon_col, lat_col), crs = 4326)
  }, error = function(e) NULL)
  
  if (is.null(sf_obj)) return(rep(0, nrow(df)))
  return(lengths(st_intersects(sf_obj, region_sf)))
}

# Calculate perpendicular distance from checklist location to bounding box ----
compute_bbox_distance <- 
  function(df, lon_min, lon_max, lat_min, lat_max, actual_lon, actual_lat) {
  # Checklist locations as points
  pts <- st_as_sf(df, coords = c(actual_lon, actual_lat), crs = 4326)
  
  # Bounding boxes as polygons
  boxes <- st_sfc(purrr::pmap(
    list(df[[lon_min]], df[[lon_max]], df[[lat_min]], df[[lat_max]]),
    function(xmin, xmax, ymin, ymax) {
      st_polygon(list(matrix(c(xmin, ymin, xmax, ymin, xmax, ymax, xmin, ymax, xmin, ymin), 
                             ncol = 2, byrow = TRUE)))
    }
  ), crs = 4326)
  
  return(as.numeric(st_distance(pts, boxes, by_element = TRUE)))
}

# Checking centroid file for errors ----

validate_centroid_schema <- function(df) {
  errors <- character() # A vector to collect all error messages
  
  expected_cols <- c("checklist_id", "obs_dt", "longitude_min", "longitude_max", 
                     "latitude_min", "latitude_max", "centroid_longitude", "centroid_latitude")
  
  # Check if expected columns are present
  missing <- setdiff(expected_cols, names(df))
  if (length(missing) > 0) {
    errors <- c(errors, paste("Missing columns:", paste(missing, collapse = ", ")))
  }
  
  # Check data type
  if (!is.character(df$checklist_id)) errors <- c(errors, "checklist_id must be character")
  
  coord_cols <- c("longitude_min", "longitude_max", "latitude_min", "latitude_max", 
                  "centroid_longitude", "centroid_latitude")
  
  bad_types <- coord_cols[!sapply(df[coord_cols], is.numeric)]
  if (length(bad_types) > 0) {
    errors <- c(errors, paste("Non-numeric columns:", paste(bad_types, collapse = ", ")))
  }
  
  # Check for missing values
  na_cols <- names(which(colSums(is.na(df)) > 0))
  if (length(na_cols) > 0) {
    errors <- c(errors, paste("NAs found in:", paste(na_cols, collapse = ", ")))
  }
  

  if (length(errors) > 0) {
    # Combine all errors into one message and stop
    stop("\n--- SCHEMA VALIDATION FAILED ---\n", paste("-", errors, collapse = "\n"))
  }
  
  return(TRUE)
}

# --- Scoring a checklist ---

# Assign score based on distance in kilometers
calculate_location_score <- function(dist_km) {
  case_when(
    dist_km == 0 ~ 0,
    dist_km < 2  ~ 1,
    dist_km > 2 & dist_km < 5 ~ 2,
    TRUE ~ 3
  )
}