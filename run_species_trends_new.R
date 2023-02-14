require(tidyverse)
require(lme4)
require(VGAM)
require(parallel)
require(foreach)
require(doParallel)

source('SoIB_v2 functions.R')

load("dataforanalyses.RData")
lsa = specieslist %>% filter(!is.na(ht) | !is.na(rt))
listofspecies = c(lsa$COMMON.NAME,restrictedspecieslist$COMMON.NAME)
count = 0
databins=c(1992,2003,2009,2012,2013,2014,2015,2016,2017,2018,2019,2020,2021)

for (k in 1:100)
{
  start = Sys.time()
  
  # read required data files from folder - specify path accordingly
  
  nm = paste("/data",k,".csv",sep="")
  filename = paste("E:/Abhinandan/BCI/soib_v2/dataforsim",nm,sep = '')
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
  
  parallel::stopCluster(cl = my.cluster)
  
  if (count == 0)
    trends = data.frame(trends0)
  if (count > 0)
    trends = rbind(trends,data.frame(trends0))
  
  end = Sys.time()
  print(end-start)
  
  count = count + 1
  print(k)
}

trends_temp = trends

len = length(as.vector(trends[,1]))
n = len/27
spnames = as.vector(trends[1,])
sq = -seq(1,len,27)
trends = trends[sq,]
names(trends) = spnames
a = rep(c("freq","se"),each=13)
a1 = rep(a,n)
tg = c("before 2000","2000-2006","2007-2010",
       "2011-2012","2013","2014","2015","2016",
       "2017","2018","2019","2020","2021")
b1 = rep(1:n,each=26)
databins1 = rep(databins,n*2)
tg1 = rep(tg,n*2)

trends$timegroups = databins1
trends$timegroupsf = tg1
trends$type = a1
trends$sl = b1

trends = pivot_longer(trends, -c(timegroups,timegroupsf,sl,type), 
                      values_to = "value", names_to = "COMMON.NAME")
trends = pivot_wider(trends, names_from = type, values_from = value)
trends$sp = rep(1:720,13*n)

trends = trends %>%
  arrange(sl,sp) %>%
  select(sl,COMMON.NAME,timegroupsf,timegroups,freq,se,-sp)

write.csv(trends, "trends_1.csv", row.names = F)










