## create N data files using randomgroupids
## if your assigned files are 201-400, then line 14
## should be for (i in 201:400)

require(tidyverse)

source('00_scripts/SoIBv2_functions.R')

load("dataforanalyses.RData")
load("randomgroupids.RData")

dir.create("dataforsim")

for (i in 201:400)
{
  start = Sys.time()
  data0 = data %>% 
    filter(group.id %in% randomgroupids[,i]) 
  end = Sys.time()
  print(end-start)
  
  start = Sys.time()
  nm = paste("/data",i,".csv",sep="")
  filename = paste("E:/Abhinandan/BCI/soib_v2/dataforsim",nm,sep = '')
  write.csv(data0,filename,row.names=F)
  end = Sys.time()
  print(end-start)
  gc()
}

rm(list = ls())
