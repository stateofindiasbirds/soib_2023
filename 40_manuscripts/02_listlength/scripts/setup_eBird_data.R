load("00_data/dataforanalyses_extra.RData")
load("40_manuscripts/02_listlength/KL_Atlas_data.RData")

require(tidyverse)

source('00_scripts/00_functions.R')

imp = c("CATEGORY","COMMON.NAME","OBSERVATION.COUNT",
        "LOCALITY.ID","ST_NM","DISTRICT",
        "PROTOCOL.TYPE","DURATION.MINUTES","EFFORT.DISTANCE.KM",
        "ALL.SPECIES.REPORTED","group.id",
        "month","year","no.sp","gridg1","gridg2","gridg3","gridg4")

KL_data = data0 %>%
  # remove Atlas checklists
  filter(!group.id %in% KL_Atlas_data$group.id) %>%
  dplyr::select(all_of(imp)) %>%
  filter(ALL.SPECIES.REPORTED == 1, ST_NM == "Kerala",
         month %in% 1:3, year %in% 2014:2019)

save(KL_data, file = "40_manuscripts/02_listlength/KL_data.RData")




########################

load("40_manuscripts/02_listlength/KL_data.RData")

require(tidyverse)
require(parallel)
require(foreach)
require(doParallel)

createrandomlocs = function(locs)
{
  require(tidyverse)
  
  locs1 = locs %>% 
    group_by(LOCALITY.ID, month) %>% sample_n(1)
  
  return(locs1$group.id)
}
  
locs = KL_data %>%
  distinct(LOCALITY.ID, group.id, month)
locs_du = KL_data %>%
  filter(!is.na(DURATION.MINUTES)) %>%
  distinct(LOCALITY.ID, group.id, month, DURATION.MINUTES)
locs_di = KL_data %>%
  filter(!is.na(EFFORT.DISTANCE.KM)) %>%
  distinct(LOCALITY.ID, group.id, month, DURATION.MINUTES, EFFORT.DISTANCE.KM)


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
randomgroupids_du = foreach(i = 1:1000, .combine = 'cbind') %dopar%
  createrandomlocs(locs_du)
randomgroupids_di = foreach(i = 1:1000, .combine = 'cbind') %dopar%
  createrandomlocs(locs_di)

parallel::stopCluster(cl = my.cluster)

save(randomgroupids, file = "40_manuscripts/02_listlength/randomgroupids.RData")
save(randomgroupids_du, file = "40_manuscripts/02_listlength/randomgroupids_du.RData")
save(randomgroupids_di, file = "40_manuscripts/02_listlength/randomgroupids_di.RData")






#####################################

load("40_manuscripts/02_listlength/KL_data.RData")

load("40_manuscripts/02_listlength/randomgroupids.RData")
load("40_manuscripts/02_listlength/randomgroupids_du.RData")
load("40_manuscripts/02_listlength/randomgroupids_di.RData")


for (i in 1:1000)
{
  # file names for individual files
  write_path = paste("40_manuscripts/02_listlength/dataforsim/overall/data",i,".csv",sep="")
  data_filt = KL_data %>% filter(group.id %in% randomgroupids[,i])
  write.csv(data_filt, file = write_path, row.names = F)

  gc()
}

for (i in 1:1000)
{
  # file names for individual files
  write_path = paste("40_manuscripts/02_listlength/dataforsim/duration/data",i,".csv",sep="")
  data_filt = KL_data %>% filter(group.id %in% randomgroupids_du[,i])
  write.csv(data_filt, file = write_path, row.names = F)
  
  gc()
}

for (i in 1:1000)
{
  # file names for individual files
  write_path = paste("40_manuscripts/02_listlength/dataforsim/distance/data",i,".csv",sep="")
  data_filt = KL_data %>% filter(group.id %in% randomgroupids_di[,i])
  write.csv(data_filt, file = write_path, row.names = F)
  
  gc()
}
