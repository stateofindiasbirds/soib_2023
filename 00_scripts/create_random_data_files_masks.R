## create N data files using randomgroupids

require(tidyverse)
source('00_scripts/SoIBv2_functions.R')



## woodland

load("masks_analyses/dataforanalyses_mask_woodland.RData")
load("masks_analyses/randomgroupids_mask_woodland.RData")

dir.create("masks_analyses/dataforsim_mask_woodland")

for (i in 1:300)
{
  start = Sys.time()
  data0 = data %>% 
    filter(group.id %in% randomgroupids[,i]) 
  end = Sys.time()
  print(end-start)
  
  start = Sys.time()
  nm = paste("/data",i,".csv",sep="")
  filename = paste("masks_analyses/dataforsim_mask_woodland",nm,sep = '')
  write.csv(data0,filename,row.names=F)
  end = Sys.time()
  print(end-start)
  gc()
}

rm(list = ls())
gc()




## cropland

load("masks_analyses/dataforanalyses_mask_cropland.RData")
load("masks_analyses/randomgroupids_mask_cropland.RData")

dir.create("masks_analyses/dataforsim_mask_cropland")

for (i in 1:200)
{
  start = Sys.time()
  data0 = data %>% 
    filter(group.id %in% randomgroupids[,i]) 
  end = Sys.time()
  print(end-start)
  
  start = Sys.time()
  nm = paste("/data",i,".csv",sep="")
  filename = paste("masks_analyses/dataforsim_mask_cropland",nm,sep = '')
  write.csv(data0,filename,row.names=F)
  end = Sys.time()
  print(end-start)
  gc()
}

rm(list = ls())
gc()




## oneland

load("masks_analyses/dataforanalyses_mask_oneland.RData")
load("masks_analyses/randomgroupids_mask_oneland.RData")

dir.create("masks_analyses/dataforsim_mask_oneland")

for (i in 1:200)
{
  start = Sys.time()
  data0 = data %>% 
    filter(group.id %in% randomgroupids[,i]) 
  end = Sys.time()
  print(end-start)
  
  start = Sys.time()
  nm = paste("/data",i,".csv",sep="")
  filename = paste("masks_analyses/dataforsim_mask_oneland",nm,sep = '')
  write.csv(data0,filename,row.names=F)
  end = Sys.time()
  print(end-start)
  gc()
}

rm(list = ls())
gc()





## pa

load("masks_analyses/dataforanalyses_mask_pa.RData")
load("masks_analyses/randomgroupids_mask_pa.RData")

dir.create("masks_analyses/dataforsim_mask_pa")

for (i in 1:200)
{
  start = Sys.time()
  data0 = data %>% 
    filter(group.id %in% randomgroupids[,i]) 
  end = Sys.time()
  print(end-start)
  
  start = Sys.time()
  nm = paste("/data",i,".csv",sep="")
  filename = paste("masks_analyses/dataforsim_mask_pa",nm,sep = '')
  write.csv(data0,filename,row.names=F)
  end = Sys.time()
  print(end-start)
  gc()
}

rm(list = ls())
gc()

