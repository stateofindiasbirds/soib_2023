# error check: presence-based occupancy not be run for habitat masks
if (!cur_metadata$MASK.TYPE %in% c("country", "state")) {
  return("Presence-based occupancy only to be run for full-country and individual states.")
}

# creating new directory if it doesn't already exist
if (!dir.exists(cur_metadata$OCCU.PRES.PATHONLY)) {
  dir.create(cur_metadata$OCCU.PRES.PATHONLY, 
             recursive = T)
}


# calculation -------------------------------------------------------------

# get the set of 25 km grids with confirmed presence for each species during each season

speciesforocc %>% 
  {walk2(.$eBird.English.Name.2022, .$status, ~ {
    
    tic(glue("Presence-based occupancy for {.x}"))
    
    # File names for individual files
    write_path <- cur_metadata %>%
      summarise(OCCU.PRES.PATH = glue("{OCCU.PRES.PATHONLY}{.x}_{.y}.csv")) %>%
      pull(OCCU.PRES.PATH)
    
    data_grids_present <- data %>% 
      # filter (or not) the data based on migratory status
      filt_data_for_mig(.x, .y) %>%
      # get presence-cells of the species
      filter(COMMON.NAME == .x) %>% 
      distinct(gridg1) %>% 
      mutate(presence = 1,
             status = .y, 
             COMMON.NAME = .x)
    
    toc()
    
    if (length(data_grids_present$COMMON.NAME) > 0) {
      write.csv(data_grids_present, file = write_path, row.names = FALSE)
    }
    
    gc()
    
  })}
