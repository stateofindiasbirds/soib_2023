library(parallel)

# preparing data for specific mask (this is the only part that changes, but automatically)
cur_metadata <- get_metadata(cur_mask)
read_path <- cur_metadata$LOCS.PATH
write_path <- cur_metadata$RAND.GROUP.IDS.PATH
speclist_path <- cur_metadata$SPECLISTDATA.PATH

# don't run if no species selected
load(speclist_path)
to_run <- (1 %in% specieslist$ht) | (1 %in% specieslist$rt) |
  (1 %in% restrictedspecieslist$ht) | (1 %in% restrictedspecieslist$rt)

###

convert_group_id <- function(x) {
  base <- ifelse(substr(x,1,1)=="S",0L,1000000000L) # G=>giga base
  return(base + as.integer(substr(x,2,12)))
}

idx_col <- function(row) {
  return(paste0(row["LOCALITY.ID"],as.character(row["month"]),row["timegroups"]))
}

group_values <- function(df) {
  result <- df %>%
    group_by(idx)
  return(result)
}

write_rgids <- function(write_dir, rgids) {
  # use default compression. good enough for small sets
  for(i in 1:1000) {
    randomgroupids <-as.integer(rgids[,i])
    save(randomgroupids, file = paste0(write_dir,"/rgids-",i,".RData"))
  }
}

write_rgids_xz <- function(write_path, rgids) {
  # deploy xz compression for large files
  cores <- parallel::detectCores()
  running <- 0
  next_idx <- 1
  done_count <- 0
  while((next_idx<=1000) || (done_count<1000)) {
    started <- 0
    while((running < cores) && (next_idx<=1000)) {
      randomgroupids <- rgids[,next_idx]
      mcparallel(save(randomgroupids, file = paste0(write_path,"-",next_idx), compress="xz"))
      next_idx <- next_idx + 1
      running <- running+1
      started <- started+1
    }
    #print(paste("Running:", running, " Started:", started, "next_idx:",next_idx))
    ret <- mccollect(timeout=1, wait=FALSE)
    running <- running-length(ret)
    done_count <- done_count + length(ret)
    #print(paste("Done count:", done_count))
  }
}

if (to_run == TRUE) {

  # create the set of random locations (doesn't work inside a function)
  require(tidyverse)

  source('00_scripts/00_functions.R')

  message(paste("Loading:", read_path))
  locs = read.csv(read_path)
  locs$keys = apply(locs, 1, idx_col)
  indexes <- distinct(locs,keys)
  key_index <- match(locs$keys, indexes$keys)
  locs$idx <- key_index
  locs$group.id <- convert_group_id(locs$group.id)

  # remove unneeded columns
  locs[c("keys", "month","timegroups","LOCALITY.ID")] <- list(NULL)
  gc()

  locs <- locs %>% group_by(idx)
  message("Generating random samples...")
  # Sampling 1000, one for each run, with replacement is much
  # faster than doing one at a time
  rtable <-locs %>%
	   slice_sample(n = 1000, replace=TRUE) %>%
	   ungroup()
  # rearrange the 1000 to represent each run
  rgids <- matrix(rtable$group.id, ncol=1000, byrow=TRUE)
  rm(rtable)
  rm(locs)
  # Write the 1000
  target_path <- write_path
  target_dir <- dirname(target_path)
  message(paste("Saving random ids at:",target_dir))
  if(!dir.exists(target_dir)) {
    dir.create(target_dir, recursive = TRUE)
  }

  # Use xz compression for large datasets (1000 sets >= 100 MB)
  # For small datasets (e.g. states) the benefits are minimal
#  if(nrow(rgids)>=50000) {
#    write_rgids_xz(target_path, rgids)
#  } else {
#    write_rgids(target_path, rgids)
#  }
  write_rgids(target_dir, rgids)
  #mcparallel(write_rgids(write_path, rgids))

  # Cleanup
  rm(rgids)
  gc()

} else {

  print(glue("Skipping creation of random group IDs for {cur_mask}"))

}
