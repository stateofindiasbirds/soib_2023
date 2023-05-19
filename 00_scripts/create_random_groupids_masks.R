## create the set of random locations that doesn't work inside a function
## start parallel

require(tidyverse)
require(parallel)
require(foreach)
require(doParallel)



## woodland

source('00_scripts/SoIBv2_functions.R')
locs = read.csv("masks_analyses/sub_samp_locs_mask_woodland.csv")

n.cores = parallel::detectCores()/2
#create the cluster
my.cluster = parallel::makeCluster(
  n.cores, 
  type = "PSOCK"
)
#register it to be used by %dopar%
doParallel::registerDoParallel(cl = my.cluster)
#check if it is registered (optional)
#foreach::getDoParRegistered()
#how many workers are available? (optional)
#foreach::getDoParWorkers()


randomgroupids = foreach(i=1:1000, .combine='cbind') %dopar%
  createrandomlocs(locs)

parallel::stopCluster(cl = my.cluster)

save(randomgroupids,file = "masks_analyses/randomgroupids_mask_woodland.RData")

rm(list = ls())
gc()



## cropland

source('00_scripts/SoIBv2_functions.R')
locs = read.csv("masks_analyses/sub_samp_locs_mask_cropland.csv")

n.cores = parallel::detectCores()/2
#create the cluster
my.cluster = parallel::makeCluster(
  n.cores, 
  type = "PSOCK"
)
#register it to be used by %dopar%
doParallel::registerDoParallel(cl = my.cluster)
#check if it is registered (optional)
#foreach::getDoParRegistered()
#how many workers are available? (optional)
#foreach::getDoParWorkers()


randomgroupids = foreach(i=1:1000, .combine='cbind') %dopar%
  createrandomlocs(locs)

parallel::stopCluster(cl = my.cluster)

save(randomgroupids,file = "masks_analyses/randomgroupids_mask_cropland.RData")

rm(list = ls())
gc()




## oneland

source('00_scripts/SoIBv2_functions.R')
locs = read.csv("masks_analyses/sub_samp_locs_mask_oneland.csv")

n.cores = parallel::detectCores()/2
#create the cluster
my.cluster = parallel::makeCluster(
  n.cores, 
  type = "PSOCK"
)
#register it to be used by %dopar%
doParallel::registerDoParallel(cl = my.cluster)
#check if it is registered (optional)
#foreach::getDoParRegistered()
#how many workers are available? (optional)
#foreach::getDoParWorkers()


randomgroupids = foreach(i=1:1000, .combine='cbind') %dopar%
  createrandomlocs(locs)

parallel::stopCluster(cl = my.cluster)

save(randomgroupids,file = "masks_analyses/randomgroupids_mask_oneland.RData")

rm(list = ls())
gc()





## pa

source('00_scripts/SoIBv2_functions.R')
locs = read.csv("masks_analyses/sub_samp_locs_mask_pa.csv")

n.cores = parallel::detectCores()/2
#create the cluster
my.cluster = parallel::makeCluster(
  n.cores, 
  type = "PSOCK"
)
#register it to be used by %dopar%
doParallel::registerDoParallel(cl = my.cluster)
#check if it is registered (optional)
#foreach::getDoParRegistered()
#how many workers are available? (optional)
#foreach::getDoParWorkers()


randomgroupids = foreach(i=1:1000, .combine='cbind') %dopar%
  createrandomlocs(locs)

parallel::stopCluster(cl = my.cluster)

save(randomgroupids,file = "masks_analyses/randomgroupids_mask_pa.RData")

rm(list = ls())
gc()
