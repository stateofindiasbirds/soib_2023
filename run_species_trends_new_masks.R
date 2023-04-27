require(tidyverse)
require(lme4)
require(VGAM)
require(parallel)
require(foreach)
require(doParallel)

source('SoIB_v2 functions.R')





## woodland

load("masks_analyses/specieslists_mask_woodland.RData")
lsa = specieslist %>% filter(!is.na(ht) | !is.na(rt))
listofspecies = c(lsa$COMMON.NAME,restrictedspecieslist$COMMON.NAME)
speclen = length(listofspecies)

dir.create("masks_analyses/trends_mask_woodland")

for (k in 1:200)
{
  start = Sys.time()
  
  # read required data files from folder - specify path accordingly
  
  nm = paste("/data",k,".csv",sep="")
  filename = paste("masks_analyses/dataforsim_mask_woodland",nm,sep = '')
  data = read.csv(filename)
  
  data$gridg1 = as.factor(data$gridg1)
  data$gridg2 = as.factor(data$gridg2)
  data$gridg3 = as.factor(data$gridg3)
  data$gridg4 = as.factor(data$gridg4)
  data$month = as.factor(data$month)
  data$timegroups = as.factor(data$timegroups)
  data$gridg = data$gridg3
  
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
  
  trends0 = foreach (i = listofspecies, .combine='cbind') %dopar%
    singlespeciesrun(data,i,specieslist,restrictedspecieslist)
  
  trends = data.frame(trends0)
  
  len = length(as.vector(trends[,1]))
  n = len/29
  spnames = as.vector(trends[1,])
  sq = -seq(1,len,29)
  trends = trends[sq,]
  names(trends) = spnames
  a = rep(c("freq","se"),each=14)
  a1 = rep(a,n)
  tg = c("before 2000","2000-2006","2007-2010",
         "2011-2012","2013","2014","2015","2016",
         "2017","2018","2019","2020","2021","2022")
  #b1 = rep(1:n,each=28)
  databins1 = rep(databins$year,n*2)
  tg1 = rep(tg,n*2)
  
  trends$timegroups = databins1
  trends$timegroupsf = tg1
  trends$type = a1
  trends$sl = k
  
  trends = pivot_longer(trends, -c(timegroups,timegroupsf,sl,type), 
                        values_to = "value", names_to = "COMMON.NAME")
  trends = pivot_wider(trends, names_from = type, values_from = value)
  trends$sp = rep(1:speclen,14*n)
  
  trends = trends %>%
    arrange(sl,sp) %>%
    select(sl,COMMON.NAME,timegroupsf,timegroups,freq,se,-sp)
  
  write.csv(trends, paste0('masks_analyses/trends_mask_woodland/trends_', k,'.csv'), row.names = F)
  
  end = Sys.time()
  print(end-start)
  
  print(k)
  
  gc()
}





## cropland

load("masks_analyses/specieslists_mask_cropland.RData")
lsa = specieslist %>% filter(!is.na(ht) | !is.na(rt))
listofspecies = c(lsa$COMMON.NAME,restrictedspecieslist$COMMON.NAME)
speclen = length(listofspecies)

dir.create("masks_analyses/trends_mask_cropland")

for (k in 1:200)
{
  start = Sys.time()
  
  # read required data files from folder - specify path accordingly
  
  nm = paste("/data",k,".csv",sep="")
  filename = paste("masks_analyses/dataforsim_mask_cropland",nm,sep = '')
  data = read.csv(filename)
  
  data$gridg1 = as.factor(data$gridg1)
  data$gridg2 = as.factor(data$gridg2)
  data$gridg3 = as.factor(data$gridg3)
  data$gridg4 = as.factor(data$gridg4)
  data$month = as.factor(data$month)
  data$timegroups = as.factor(data$timegroups)
  data$gridg = data$gridg3
  
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
  
  trends0 = foreach (i = listofspecies, .combine='cbind') %dopar%
    singlespeciesrun(data,i,specieslist,restrictedspecieslist)
  
  trends = data.frame(trends0)
  
  len = length(as.vector(trends[,1]))
  n = len/29
  spnames = as.vector(trends[1,])
  sq = -seq(1,len,29)
  trends = trends[sq,]
  names(trends) = spnames
  a = rep(c("freq","se"),each=14)
  a1 = rep(a,n)
  tg = c("before 2000","2000-2006","2007-2010",
         "2011-2012","2013","2014","2015","2016",
         "2017","2018","2019","2020","2021","2022")
  #b1 = rep(1:n,each=28)
  databins1 = rep(databins$year,n*2)
  tg1 = rep(tg,n*2)
  
  trends$timegroups = databins1
  trends$timegroupsf = tg1
  trends$type = a1
  trends$sl = k
  
  trends = pivot_longer(trends, -c(timegroups,timegroupsf,sl,type), 
                        values_to = "value", names_to = "COMMON.NAME")
  trends = pivot_wider(trends, names_from = type, values_from = value)
  trends$sp = rep(1:speclen,14*n)
  
  trends = trends %>%
    arrange(sl,sp) %>%
    select(sl,COMMON.NAME,timegroupsf,timegroups,freq,se,-sp)
  
  write.csv(trends, paste0('masks_analyses/trends_mask_cropland/trends_', k,'.csv'), row.names = F)
  
  end = Sys.time()
  print(end-start)
  
  print(k)
  
  gc()
}





## oneland

load("masks_analyses/specieslists_mask_oneland.RData")
lsa = specieslist %>% filter(!is.na(ht) | !is.na(rt))
listofspecies = c(lsa$COMMON.NAME,restrictedspecieslist$COMMON.NAME)
speclen = length(listofspecies)

dir.create("masks_analyses/trends_mask_oneland")

for (k in 1:200)
{
  start = Sys.time()
  
  # read required data files from folder - specify path accordingly
  
  nm = paste("/data",k,".csv",sep="")
  filename = paste("masks_analyses/dataforsim_mask_oneland",nm,sep = '')
  data = read.csv(filename)
  
  data$gridg1 = as.factor(data$gridg1)
  data$gridg2 = as.factor(data$gridg2)
  data$gridg3 = as.factor(data$gridg3)
  data$gridg4 = as.factor(data$gridg4)
  data$month = as.factor(data$month)
  data$timegroups = as.factor(data$timegroups)
  data$gridg = data$gridg3
  
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
  
  trends0 = foreach (i = listofspecies, .combine='cbind') %dopar%
    singlespeciesrun(data,i,specieslist,restrictedspecieslist)
  
  trends = data.frame(trends0)
  
  len = length(as.vector(trends[,1]))
  n = len/29
  spnames = as.vector(trends[1,])
  sq = -seq(1,len,29)
  trends = trends[sq,]
  names(trends) = spnames
  a = rep(c("freq","se"),each=14)
  a1 = rep(a,n)
  tg = c("before 2000","2000-2006","2007-2010",
         "2011-2012","2013","2014","2015","2016",
         "2017","2018","2019","2020","2021","2022")
  #b1 = rep(1:n,each=28)
  databins1 = rep(databins$year,n*2)
  tg1 = rep(tg,n*2)
  
  trends$timegroups = databins1
  trends$timegroupsf = tg1
  trends$type = a1
  trends$sl = k
  
  trends = pivot_longer(trends, -c(timegroups,timegroupsf,sl,type), 
                        values_to = "value", names_to = "COMMON.NAME")
  trends = pivot_wider(trends, names_from = type, values_from = value)
  trends$sp = rep(1:speclen,14*n)
  
  trends = trends %>%
    arrange(sl,sp) %>%
    select(sl,COMMON.NAME,timegroupsf,timegroups,freq,se,-sp)
  
  write.csv(trends, paste0('masks_analyses/trends_mask_oneland/trends_', k,'.csv'), row.names = F)
  
  end = Sys.time()
  print(end-start)
  
  print(k)
  
  gc()
}





## pa

load("masks_analyses/specieslists_mask_pa.RData")
lsa = specieslist %>% filter(!is.na(ht) | !is.na(rt))
listofspecies = c(lsa$COMMON.NAME,restrictedspecieslist$COMMON.NAME)
speclen = length(listofspecies)

dir.create("masks_analyses/trends_mask_pa")

for (k in 1:200)
{
  start = Sys.time()
  
  # read required data files from folder - specify path accordingly
  
  nm = paste("/data",k,".csv",sep="")
  filename = paste("masks_analyses/dataforsim_mask_pa",nm,sep = '')
  data = read.csv(filename)
  
  data$gridg1 = as.factor(data$gridg1)
  data$gridg2 = as.factor(data$gridg2)
  data$gridg3 = as.factor(data$gridg3)
  data$gridg4 = as.factor(data$gridg4)
  data$month = as.factor(data$month)
  data$timegroups = as.factor(data$timegroups)
  data$gridg = data$gridg3
  
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
  
  trends0 = foreach (i = listofspecies, .combine='cbind') %dopar%
    singlespeciesrun(data,i,specieslist,restrictedspecieslist)
  
  trends = data.frame(trends0)
  
  len = length(as.vector(trends[,1]))
  n = len/29
  spnames = as.vector(trends[1,])
  sq = -seq(1,len,29)
  trends = trends[sq,]
  names(trends) = spnames
  a = rep(c("freq","se"),each=14)
  a1 = rep(a,n)
  tg = c("before 2000","2000-2006","2007-2010",
         "2011-2012","2013","2014","2015","2016",
         "2017","2018","2019","2020","2021","2022")
  #b1 = rep(1:n,each=28)
  databins1 = rep(databins$year,n*2)
  tg1 = rep(tg,n*2)
  
  trends$timegroups = databins1
  trends$timegroupsf = tg1
  trends$type = a1
  trends$sl = k
  
  trends = pivot_longer(trends, -c(timegroups,timegroupsf,sl,type), 
                        values_to = "value", names_to = "COMMON.NAME")
  trends = pivot_wider(trends, names_from = type, values_from = value)
  trends$sp = rep(1:speclen,14*n)
  
  trends = trends %>%
    arrange(sl,sp) %>%
    select(sl,COMMON.NAME,timegroupsf,timegroups,freq,se,-sp)
  
  write.csv(trends, paste0('masks_analyses/trends_mask_pa/trends_', k,'.csv'), row.names = F)
  
  end = Sys.time()
  print(end-start)
  
  print(k)
  
  gc()
}









