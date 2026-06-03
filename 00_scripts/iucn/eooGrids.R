library(sf)
library(dplyr)
library(purrr)
library(tidyr)
library(furrr)
library(progressr)

scriptpath <- "00_scripts/iucn"
datapath   <- "00_scripts/iucn"

handlers(global = TRUE)
handlers("txtprogressbar")

config_settings <- list (
  use_parallel = TRUE,
  workers = 16
)

tmpdir <- file.path(datapath, "tmp")
dir.create(tmpdir, showWarnings = FALSE)
log_file <- file.path(datapath, "eoogrid_progress.log")

log_msg <- function(...) {
  cat(sprintf("[%s] %s\n",
              Sys.time(),
              paste(..., collapse = " ")),
      file = log_file, append = TRUE)
}

if (config_settings$use_parallel)
{
  plan(multisession, workers = config_settings$workers) # Parallel ready
#  plan(multicore, workers = config_settings$workers) # Parallel ready
}

source(file.path(scriptpath, "config.R"))

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
    log_msg(paste0("Processing: ", species_name, " | Grid: ", grid_size_km, " km"))
    
    grid_base <- grids_sf[[as.character(grid_size_km)]]

    result_df <- tibble( 
                    Species = character(), 
                    GridResolution = numeric(), 
                    GridID = integer())
    
    grid_map_entry <- NULL
    
    eoo_shape <- species_data$EOOMap[[1]]
    
    if(!is.null(eoo_shape)) {
      #message(paste0("Grid cells: ", nrow(grid_sf)))
      #message(paste0("EOO bbox area: ", st_area(st_as_sfc(st_bbox(eoo_shape)))))
      
      # Step 1: bbox filter
      bbox <- st_as_sfc(st_bbox(eoo_shape))
      hits1 <- st_intersects(grid_base, bbox) # No spare matrix 
      grid_subset <- grid_base[lengths(hits1) > 0, ]
      grid_base <- NULL 
#      grid_sf <- grid_sf[st_intersects(grid_sf, bbox, sparse = TRUE), ] # Made sparse as TRUE
      
      # Step 2: precise filter
      hits2 <- st_intersects(grid_subset, eoo_shape)
      matched <- grid_subset[lengths(hits2) > 0, ]
      grid_subset <- NULL
#      matched <- grid_sf[st_intersects(grid_sf, eoo_shape, sparse = TRUE), ] # Made sparse as TRUE
      
      #message("Grid after filter: ", nrow(matched))
      
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
    log_msg(paste0("Done: ", species_name, " | Grid: ", grid_size_km,
                   " | Time: ", round(difftime(end_time, start_time, units = "secs"), 2), " sec"))
    
    return(list(
      grid_data = result_df,
      grid_map = grid_map_entry
    ))
  }
  
  eoo <- eoo[order(eoo$Species), ]
  if (config_settings$use_parallel)
  {

    message("Starting Parallel...")
    progressr::with_progress({
      
      p <- progressr::progressor(along = 1:nrow(eoo))
      
      future_map(
        1:nrow(eoo),
        function(i) {
          
          species_data <- eoo[i, ]
          species_name <- species_data$Species
          start_time <- Sys.time()
          
          res <- map(grid_sizes_km, function(gs) {
            process_species_grid(species_data, gs, grids_sf)
          })
          
          res <- flatten(res)   # 🔥 add this
          
          safe_name <- gsub("[^A-Za-z0-9_]", "_", species_name)

          tmp_file <- file.path(tmpdir, paste0(safe_name, ".rds.tmp"))
          final_file <- file.path(tmpdir, paste0(safe_name, ".rds"))
          
          saveRDS(res, file = tmp_file)
          file.rename(tmp_file, final_file)          
          rm(res)
          log_msg(
            "ALL GRIDS DONE: ", species_name,
            " | Time: ", round(difftime(Sys.time(), start_time, units = "secs"), 2)
          )

          p(sprintf("Done: %s", species_name)) 
          #res
        },
        .options = furrr_options(
          seed = TRUE,
          globals = list(
            grids_sf = grids_sf,
            grid_sizes_km = grid_sizes_km,
            log_msg = log_msg,
            log_file = log_file,
            tmpdir = tmpdir
          ),
          packages = c("sf", "dplyr", "purrr", "tibble")
        )
      )
      
    })
  }  
  else
  {
    map(
      1:nrow(eoo),
      function(i) {
        species_data <- eoo[i, ]
        species_name <- species_data$Species
        
        res <- map(grid_sizes_km, function(gs) {
          process_species_grid(species_data, gs, grids_sf)
        })
        
        res <- flatten(res)   # 🔥 add this
        safe_name <- gsub("[^A-Za-z0-9_]", "_", species_name)
        saveRDS(res, file = file.path(tmpdir, paste0(safe_name, ".rds")))
        NULL
      }
    )
  }  
#  results_flat <- purrr::flatten(results)
  
  # Combine outputs
#  grid_data <- bind_rows(purrr::map(results_flat, "grid_data"))
#  EOOGridMap <- bind_rows(purrr::compact(purrr::map(results_flat, "grid_map")))
 
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

aoo_selected <- read.csv(file.path(datapath, "aoo_selected.csv"),
                      stringsAsFactors = FALSE)

EOO <- readRDS(file.path(datapath,"eoo.RDS"))
EOO <- EOO %>%
  filter(Species %in% aoo_selected$Species)

# Testing. uncomment
#EOO <- EOO %>% filter (Species %in% species)

in_grids <- readRDS(file.path(datapath,"in_grids.RDS"))
# Create the grid data frame

message("Starting EOO Gridding for ", nrow(EOO)," species")

system.time({
  create_grid_dataframe(EOO, aoo_grid_sizes_km, in_grids)
})

# As file is huge, we do not read and merge them anymore
#files <- list.files("tmp", full.names = TRUE)

#results <- map(files, purrr::possibly(readRDS, NULL))
#results <- compact(results)

#grid_data <- bind_rows(map(results, ~ bind_rows(map(.x, "grid_data"))))
#EOOGridMap <- bind_rows(map(results, ~ bind_rows(compact(map(.x, "grid_map")))))  

#unlink("tmp/*")

#saveRDS(grid_data,"grids.RDS")
#saveRDS(EOOGridMap, "grid_maps.RDS")

