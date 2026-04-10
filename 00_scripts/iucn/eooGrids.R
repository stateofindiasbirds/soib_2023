library(sf)
library(dplyr)
library(purrr)
library(tidyr)
library(furrr)

plan(multisession, workers = 2) # Parallel ready
plan(sequential)

source("config.R")

EOOGridMap <- data.frame(
  Species = character(),
  GridResolution = numeric(),
  EOOGridMap = I(list())  # Use I(list()) for list-columns like spatial objects
)

# Function to create DataFrame of Grid IDs that intersect with EOOMap
create_grid_dataframe <- function(eoo, grid_sizes_km, grids_sf) {
  
  process_species_grid <- function(species_data, grid_size_km, grids_sf) {
    species_name <- species_data$Species
    
    start_time <- Sys.time()
    message(paste0("Processing: ", species_name, " | Grid: ", grid_size_km, " km"))
    
    grid_sf <- grids_sf[[as.character(grid_size_km)]]
    result_df <- tibble(Species = character(), GridResolution = numeric(), GridID = integer())
    grid_map_entry <- NULL
    
    eoo_shape <- species_data$EOOMap[[1]]
    
    if(!is.null(eoo_shape)) {
      message(paste0("Grid cells: ", nrow(grid_sf)))
      message(paste0("EOO bbox area: ", st_area(st_as_sfc(st_bbox(eoo_shape)))))
      
      # Step 1: bbox filter
      bbox <- st_as_sfc(st_bbox(eoo_shape))
      grid_sf <- grid_sf[st_intersects(grid_sf, bbox, sparse = FALSE), ]
      
      # Step 2: precise filter
      matched <- grid_sf[st_intersects(grid_sf, eoo_shape, sparse = FALSE), ]
      
      message("Grid after filter: ", nrow(matched))
      
      if (nrow(matched) > 0) {
        # grid_data
        result_df <- matched %>%
          st_set_geometry(NULL) %>%
          mutate(
            Species = species_name,
            GridResolution = grid_size_km
          ) %>%
          select(Species, GridResolution, GridID)
        
        # grid_map
        grid_map_entry <- data.frame(
          Species = species_name,
          GridResolution = grid_size_km,
          EOOGridMap = I(list(matched))
        )
      }
    }
    
    end_time <- Sys.time()
    message(paste0("Done: ", species_name, " | Grid: ", grid_size_km,
                   " | Time: ", round(difftime(end_time, start_time, units = "secs"), 2), " sec"))
    
    return(list(
      grid_data = result_df,
      grid_map = grid_map_entry
    ))
  }
  
  # Sequential execution (parallel-ready structure)
  results <- future_map(
      1:nrow(eoo),
      function(i) {
        species_data <- eoo[i, ]
        
        map(grid_sizes_km, function(gs) {
          process_species_grid(species_data, gs, grids_sf)
        })
      },
      .options = furrr_options(
        seed = TRUE,
      globals = list(
        grids_sf = grids_sf,
        grid_sizes_km = grid_sizes_km
      ),
      packages = c("sf", "dplyr", "purrr", "tibble")
    )
  )  
  results_flat <- purrr::flatten(results)
  
  # Combine outputs
  grid_data <- bind_rows(purrr::map(results_flat, "grid_data"))
  EOOGridMap <- bind_rows(purrr::compact(purrr::map(results_flat, "grid_map")))
  
  return(list(
    grid_data = grid_data,
    EOOGridMap = EOOGridMap
  ))
}

species <- c (
  "Banasura Laughingthrush",
  "Nilgiri Laughingthrush",
  "Ashambu Laughingthrush",
  "Black-headed Greenfinch",
  "Andaman Masked-Owl",
  "Mangrove Pitta",
  "Naga Wren-Babbler",
  "Nilgiri Pipit",
  "Nilgiri Sholakili",
  "White-bellied Sholakili",
  "Nicobar Imperial-Pigeon",
  "Mishmi Wren-Babbler",
  "Andaman Woodpecker",
  "Marsh Babbler",
  "Swamp Grass Babbler",
  "Pale-capped Pigeon",
  "Kashmir Nuthatch",
  "Gray-crowned Prinia",
  "White-bellied Blue Flycatcher",
  "Upland Pipit",
  "Yellow-eyed Pigeon",
  "White-browed Tit-Warbler",
  "Brown-cheeked Rail"
)

EOO <- readRDS("eoo.RDS")

# Testing. uncomment
#EOO <- EOO %>% filter (Species %in% species)

in_grids <- readRDS("in_grids.RDS")
# Create the grid data frame
system.time({
  res <- create_grid_dataframe(EOO, aoo_grid_sizes_km, in_grids)
})

grid_data <- res$grid_data
EOOGridMap <- res$EOOGridMap

saveRDS(grid_data,"grids.RDS")
saveRDS(EOOGridMap, "grid_maps.RDS")
