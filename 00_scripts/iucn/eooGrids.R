library(sf)
library(dplyr)
library(purrr)
library(tidyr)
source("config.R")

EOOGridMap <- data.frame(
  Species = character(),
  GridResolution = numeric(),
  EOOGridMap = I(list())  # Use I(list()) for list-columns like spatial objects
)

# Function to create DataFrame of Grid IDs that intersect with EOOMap
create_grid_dataframe <- function(eoo, grid_sizes_km, grids_sf) {
  # Helper function to process each species and grid size
  process_species_grid <- function(species_data, grid_size_km, grids_sf) {
    
    grid_sf <- grids_sf[[as.character(grid_size_km)]]
    result_df <- tibble(GridResolution = grid_size_km, GridID = integer(0))
    eoo_shape <- species_data$EOOMap[[1]]  # Access the shape for the species
    
    if(!is.null(eoo_shape))
    {
      # Clip the grid to the EOOShape
      clipped_grid <- st_intersection(grid_sf, eoo_shape)
      
      # Prepare the result dataframe
      if (nrow(clipped_grid) > 0) {
        # Add the new row to the dataframe. Note this is directly updating a global variable as a side-effect and hence <<-
        EOOGridMap <<- rbind(EOOGridMap, data.frame(Species = species_data$Species, GridResolution = grid_size_km, EOOGridMap = I(list(clipped_grid))))        
        result_df <- clipped_grid %>%
          st_set_geometry(NULL) %>%
          mutate( GridResolution = grid_size_km) %>%
          select(GridResolution, GridID)
      } 
    }
    return(result_df)
  }

  # Apply the function to all species and grid sizes
  grid_data <- eoo %>%
    rowwise() %>%
    mutate(grid_data = list(
      bind_rows(map_dfr(grid_sizes_km, ~ process_species_grid(cur_data(), .x, grids_sf)))
    )) %>%
    unnest(grid_data) %>% 
    select(Species, GridResolution, GridID)
  
  return(grid_data)
}



EOO <- readRDS("eoo.RDS")
kl_grids <- readRDS("kl_grids.RDS")
# Create the grid data frame
grid_data <- create_grid_dataframe(EOO, grid_sizes_km, kl_grids)

saveRDS(grid_data,"grids.RDS")
saveRDS(EOOGridMap, "grid_maps.RDS")
