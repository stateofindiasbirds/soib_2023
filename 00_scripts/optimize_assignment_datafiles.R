require(tidyverse)
library("tictoc")
library("dplyr")

convert_observation_count <- function(cstr) {
  count <- ifelse(cstr=="X",1L,as.integer(cstr));
  return(count)
}

convert_group_id <- function(x) {
  base <- ifelse(substr(x,1,1)=="S",0L,1000000000L) # G=>giga base
  return(base + as.integer(substr(x,2,12)))
}

convert_observer_id <- function(x) {
  return(as.integer(substr(x,5,15)))
}

as_mb <- function(val) {
  return(as.integer(val/(1024*1000)))
}

# preparing data for specific mask (this is the only part that changes, but automatically)
cur_metadata <- get_metadata(cur_mask)
read_path_data <- cur_metadata$DATA.PATH
read_path_groupids <- cur_metadata$RAND.GROUP.IDS.PATH
speclist_path <- cur_metadata$SPECLISTDATA.PATH

# don't run if no species selected
load(speclist_path)
to_run <- (1 %in% specieslist$ht) | (1 %in% specieslist$rt) |
  (1 %in% restrictedspecieslist$ht) | (1 %in% restrictedspecieslist$rt)

if (to_run == TRUE) {

  message(glue("Optimizing data files for {cur_mask}"))

  message("Loading data: ", read_path_data)
  tictoc::tic(read_path_data)
  load(read_path_data)
  tictoc::toc()

  metadata_file <- paste0(read_path_data, "-metadata")
  message("Saving metadata: ", metadata_file)
  save(databins,sampledcells,file=metadata_file)

  tgt_dataset <- paste0(read_path_data, "-data_opt")
  base_dir <- dirname(read_path_data)
  species_map <- paste0(base_dir, "/species_names.RData")
  timegroups_map <- paste0(base_dir, "/timegroups.RData")

  before = as.numeric(object.size(data))
  message(paste("Size of data BEFORE= ", as_mb(before), "MB"))

  # Map species names into indices inside a table
  species_names <- distinct(data,COMMON.NAME)
  species_index <- match(data$COMMON.NAME, species_names$COMMON.NAME)
  message(paste("Remap Status: ", all(data$COMMON.NAME == species_names[species_index,1])))
  data$COMMON.NAME <- species_index

  timegroups_names <- distinct(data,timegroups)
  timegroups_index <- match(data$timegroups, timegroups_names$timegroups)
  data$timegroups <- timegroups_index

  data$gridg0 <- as.integer(data$gridg0)
  data$gridg1 <- as.integer(data$gridg1)
  data$gridg2 <- as.integer(data$gridg2)
  data$gridg3 <- as.integer(data$gridg3)
  data$gridg4 <- as.integer(data$gridg4)

  data$ALL.SPECIES.REPORTED <- as.integer(data$ALL.SPECIES.REPORTED)
  data$month <- as.integer(data$month)
  data$year <- as.integer(data$year)

  # OBSERVATION.COUNT values of "X" convert to 1
  data$OBSERVATION.COUNT[data$OBSERVATION.COUNT == "X"] <- "1"
  storage.mode(data$OBSERVATION.COUNT) <- "integer"

  data$group.id <- convert_group_id(data$group.id)
  data$OBSERVER.ID <- convert_observer_id(data$OBSERVER.ID)

  # These aren't used in the code later - remove them
  data$gridg0 <- NULL
  data$gridg2 <- NULL
  data$gridg4 <- NULL

  after = as.numeric(object.size(data))
  percent <- ((before-after)/before)*100.0
  percent_str <- format(round(percent, 2), nsmall = 2)
  message(paste("Size of data AFTER= ", as_mb(after), "MB Savings=", percent_str, "%"))

  message(paste("Remapped dataset:",tgt_dataset))
  message(paste("Species mapping:",species_map))
  message(paste("Timegroups mapping:",timegroups_map))

  tic("Saving remapped dataset")

  # Use xz with max threads to compress this fast! Single
  # threaded takes forever. Saves 30%+ over default compression
  # quote filename as it has spaces
  con <- pipe(paste0("xz -T0 -e > \"", tgt_dataset, "\""), "wb")
  save(data, file=con)
  close(con)

  save(species_names, file=species_map)
  save(timegroups_names, file=timegroups_map)
  toc()
} else {
  message(glue("Skipping optimizing data files for {cur_mask}"))
}
