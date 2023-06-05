# preparing data for specific mask (this is the only part that changes, but automatically)
cur_metadata <- analyses_metadata %>% filter(MASK == cur_mask)
read_path <- cur_metadata$LOCS.PATH
write_path <- cur_metadata$RAND.GROUP.IDS.PATH

###


# create the set of random locations (doesn't work inside a function)

require(tidyverse)
require(parallel)
require(foreach)
require(doParallel)

source('00_scripts/00_functions.R')



locs = read.csv(read_path)

# start parallel
n.cores = parallel::detectCores()/2
# create the cluster
my.cluster = parallel::makeCluster(
  n.cores, 
  type = "PSOCK"
)
# register it to be used by %dopar%
doParallel::registerDoParallel(cl = my.cluster)

# # check if it is registered (optional)
# foreach::getDoParRegistered()
# # how many workers are available? (optional)
# foreach::getDoParWorkers()


randomgroupids = foreach(i = 1:1000, .combine = 'cbind') %dopar%
  createrandomlocs(locs)

parallel::stopCluster(cl = my.cluster)

save(randomgroupids, file = write_path)


# cleaning up memory
rm(locs, n.cores, my.cluster, randomgroupids, cur_metadata, read_path, write_path)

detach("package:doParallel", unload = TRUE)
detach("package:foreach", unload = TRUE)
detach("package:parallel", unload = TRUE)

gc()
