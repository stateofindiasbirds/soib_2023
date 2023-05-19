## create the set of random locations that doesn't work inside a function
## start parallel

require(tidyverse)
require(parallel)
require(foreach)
require(doParallel)

source('00_scripts/SoIBv2_functions.R')
locs = read.csv("sub_samp_locs.csv")

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

save(randomgroupids,file = "randomgroupids.RData")

rm(list = ls())
