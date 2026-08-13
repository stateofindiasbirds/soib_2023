# error check: presence-based occupancy not be run for habitat masks
if (cur_metadata$MASK.TYPE != "country") {
  return("Model-based occupancy only to be run for full-country!")
}

# creating new directory if it doesn't already exist
if (!dir.exists(cur_metadata$OCCU.MOD.PATHONLY)) {
  dir.create(cur_metadata$OCCU.MOD.PATHONLY, 
             recursive = T)
} else {
  
  # in interannual updates, we need to delete all past-year output files
  # because species names change every year with taxonomy updates.
  # hence, although most species' files will simply get overwritten, for many
  # species we will end up with multiple files, one for each taxonomy update
  # (if not interannual update, everything will be in a new repo so no need for this.)
  if (interannual_update == TRUE) {
    
    files_to_del <- list.files(cur_metadata$OCCU.MOD.PATHONLY, full.names = TRUE)
    
    if (length(files_to_del) != 0) {
      file.remove(files_to_del)
    }
    
  }
  
}

###

library(parallel)
require(foreach)
require(doParallel)

load("00_data/grids_sf_nb.RData")
our_neighbours <- g1_nb_q
rm(g1_nb_r, g2_nb_q, g2_nb_r, g3_nb_q, g3_nb_r, g4_nb_q, g4_nb_r)

data = data %>%
  mutate(OBSERVATION.COUNT = 1)

speciesforocc %>%
  {walk2(.$eBird.English.Name.2025, .$status, ~ {
    
    tic(glue("Model-based occupancy for {.x}"))
    
    # File names for individual files
    write_path <- cur_metadata %>%
      summarise(OCCU.MOD.PATH = glue("{OCCU.MOD.PATHONLY}{.x}_{.y}.csv")) %>%
      pull(OCCU.MOD.PATH)
    
    occu0 = occupancyrun(data = data, 
                         species = .x,
                         status = .y,
                         queen_neighbours = g1_nb_q)

    toc()

    
    if (length(occu0$COMMON.NAME) > 0) {
      write.csv(occu0, file = write_path, row.names = FALSE)
    }
    
  })}