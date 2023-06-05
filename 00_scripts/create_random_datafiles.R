require(tidyverse)

# preparing data for specific mask (this is the only part that changes, but automatically)
cur_metadata <- analyses_metadata %>% filter(MASK == cur_mask)
read_path_data <- cur_metadata$DATA.PATH
read_path_groupids <- cur_metadata$RAND.GROUP.IDS.PATH

# for the full country analysis, runs are split among multiple systems, and use
# separate subsampled datasets. We need to ensure this information exists.
if (cur_mask == "none") {
  
  if (!exists("my_assignment")) {
    return("'my_assignment' is empty! Please specify IDs of data files assigned to you.")
  }

  cur_assignment <- my_assignment
  
} else {
  
  # woodland mask needs more sims
  if (cur_mask == "woodland") {
    no_sim <- 300
  } else {
    no_sim <- 200
  }
  
  cur_assignment <- 1:no_sim
  
}


###

# create individual data files subsampled using random GROUPIDs

tictoc::tic("Loading data")
load(read_path_data)
load(read_path_groupids)
tictoc::toc()

# creating new directory if it doesn't already exist
if (!dir.exists(cur_metadata$SIMDATA.PATHONLY)) {
  dir.create(cur_metadata$SIMDATA.PATHONLY, 
             recursive = T)
}


for (i in cur_assignment)
{
  # file names for individual files
  write_path <- cur_metadata %>% 
    dplyr::summarise(SIMDATA.PATH = glue("{SIMDATA.PATHONLY}data{i}.csv"))
  
  
  tictoc::tic(glue("({i}/{max(cur_assignment)}) Filtering data"))
  data_filt = data %>% filter(group.id %in% randomgroupids[,i]) 
  tictoc::toc()
  
  tictoc::tic(glue("({i}/{max(cur_assignment)}) Writing data"))
  write.csv(data_filt, file = write_path$SIMDATA.PATH, row.names = F)
  tictoc::toc()
  
  gc()
}

# cleaning up memory
rm(no_sim, cur_assignment, cur_metadata, read_path_data, read_path_groupids, 
   write_path, data_filt,
   data, sampledcells, databins, stats, randomgroupids)

gc()

