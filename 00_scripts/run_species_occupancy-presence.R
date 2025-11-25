# error check: presence-based occupancy not be run for habitat masks
if (!cur_metadata$MASK.TYPE %in% c("country", "state")) {
  return("Presence-based occupancy only to be run for full-country and individual states.")
}

# creating new directory if it doesn't already exist
if (!dir.exists(cur_metadata$OCCU.PRES.PATHONLY)) {
  dir.create(cur_metadata$OCCU.PRES.PATHONLY, 
             recursive = TRUE)
} else {
  
  # in interannual updates, we need to delete all past-year output files
  # because species names change every year with taxonomy updates.
  # hence, although most species' files will simply get overwritten, for many
  # species we will end up with multiple files, one for each taxonomy update
  # (if not interannual update, everything will be in a new repo so no need for this.)
  if (interannual_update == TRUE) {
    
    files_to_del <- list.files(cur_metadata$OCCU.PRES.PATHONLY, full.names = TRUE)
    
    if (length(files_to_del) != 0) {
      file.remove(files_to_del)
    }
    
  }
  
}


# calculation -------------------------------------------------------------

# get the set of 25 km grids with confirmed presence for each species during each season

speciesforocc %>% 
  {walk2(.$eBird.English.Name.2024, .$status, ~ {
    
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
    
  })}
