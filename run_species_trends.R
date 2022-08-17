require(tidyverse)
require(lme4)
require(VGAM)
require(parallel)

source('SoIB_v2 functions.R')
load("dataforanalyses.RData")

databins=c(1992,2003,2009,2012,2013,2014,2015,2016,2017,2018,2019,2020,2021)
error=T
nsim = 1000

data$gridg1 = as.factor(data$gridg1)
data$gridg2 = as.factor(data$gridg2)
data$gridg3 = as.factor(data$gridg3)
data$gridg4 = as.factor(data$gridg4)

## considers only complete lists

data = data %>%
  filter(ALL.SPECIES.REPORTED == 1)

data$month = as.factor(data$month)

data$timegroups = as.factor(data$timegroups)
data$gridg = data$gridg3

data0 = data

## the loop across all species for a subset of specieslist

specieslist = specieslist %>% filter(COMMON.NAME %in% c("Red-necked Falcon","Ashy Prinia",
                                                        "House Sparrow"))
c = 0

start = Sys.time()
for (species in specieslist$COMMON.NAME)
{
  c = c + 1
  data = data0
  
  specieslist1 = specieslist %>%
    filter(COMMON.NAME == species)
  
  ## filters data based on whether the species has been selected for long-term trends (ht) 
  ## or short-term trends (rt) 
  
  if (is.na(specieslist1$ht) & !is.na(specieslist1$rt))
  {
    g1 = data.frame(timegroups = unique(data$timegroups))
    g1$se = g1$freq = NA
    g1$timegroups = factor(g1$timegroups, levels = c("before 2000","2000-2006","2007-2010",
                                                     "2011-2012","2013","2014","2015","2016",
                                                     "2017","2018","2019","2020","2021"))
    g1 = g1[order(g1$timegroups),]
    names(g1)[1] = "timegroupsf"
    mp = data.frame(timegroupsf = c("before 2000","2000-2006","2007-2010",
                                    "2011-2012","2013","2014","2015","2016",
                                    "2017","2018","2019","2020","2021"), 
                    timegroups = as.numeric(databins))
    g1 = left_join(g1,mp)
    g1$species = species
    g1 = g1 %>%
      filter(timegroups < 2017)
    
    data = data %>%
      filter(year >= 2017)
  }
  
  if (is.na(specieslist1$ht) & is.na(specieslist1$rt))
  {
    f1 = data.frame(timegroups = unique(data$timegroups))
    f1$se = f1$freq = NA
    f1$timegroups = factor(f1$timegroups, levels = c("before 2000","2000-2006","2007-2010",
                                                     "2011-2012","2013","2014","2015","2016",
                                                     "2017","2018","2019","2020","2021"))
    f1 = f1[order(f1$timegroups),]
    names(f1)[1] = "timegroupsf"
    mp = data.frame(timegroupsf = c("before 2000","2000-2006","2007-2010",
                                    "2011-2012","2013","2014","2015","2016",
                                    "2017","2018","2019","2020","2021"), 
                    timegroups = as.numeric(databins))
    f1 = left_join(f1,mp)
    f1$species = species
    f1 = f1 %>% filter(!is.na(f1$timegroups))
  }
  

  temp = data %>%
    filter(COMMON.NAME == species) %>%
    distinct(gridg3,month)
  data = temp %>% left_join(data)
  
  ## calculate a median list length to use to predict
  
  datay = data %>%
    group_by(gridg3,gridg1,group.id) %>% slice(1) %>% ungroup %>%
    group_by(gridg3,gridg1) %>% summarize(medianlla = median(no.sp)) %>%
    group_by(gridg3) %>% summarize(medianlla = mean(medianlla)) %>%
    summarize(medianlla = round(mean(medianlla)))
  
  medianlla = datay$medianlla
  
  ## expand dataframe to include absences as well
  
  ed = expandbyspecies(data,species)
  tm = unique(data$timegroups)
  #rm(data, pos = ".GlobalEnv")
  
  ## the model
  
  m1 = glmer(OBSERVATION.COUNT ~ month + month:log(no.sp) + timegroups + (1|gridg3/gridg1), data = ed, 
             family=binomial(link = 'cloglog'), nAGQ = 0, control = glmerControl(optimizer = "bobyqa"))
  
  ## prepare a new data file to predict
  
  f = data.frame(unique(tm))
  f = do.call("rbind", replicate(length(unique(ed$month)),f,simplify=F))
  names(f) = "timegroups"
  f$month = rep(unique(ed$month), each = length(f$timegroups)/length(unique(ed$month)))
  ltemp = data.frame(timegroups = f$timegroups,
                     no.sp = medianlla, month = f$month)
  
  f1 = data.frame(timegroups = tm)
  
  f2 = data.frame(freq = numeric(length(ltemp$no.sp)))
  f2$se = numeric(length(ltemp$no.sp))
  f2$timegroups = ltemp$timegroups
  
  ## bootstrap to get errors
  
  if (error)
  {
    predFun = function(m1) {
      predict(m1,ltemp, re.form = NA, allow.new.levels=TRUE)
    }
    
    cr = max(1, detectCores())
    cl = makeCluster(rep("localhost", cr), outfile = 'log.txt')
    #showConnections(all = T)
    #clusterEvalQ(cl, library(lme4))
    
    pred = bootMer(m1, nsim = nsim, FUN = predFun, parallel = "multicore",
                   use.u = FALSE, type = "parametric", ncpus = cr, cl = cl)
    
    stopCluster(cl)
    
    for (i in 1:length(ltemp$no.sp))
    {
      f2$freq[i] = median(na.omit(pred$t[,i]))
      f2$se[i] = sd(na.omit(pred$t[,i]))
    }
    
    f2$freqt = clogloglink(f2$freq,inverse = T)
    f2$cl = clogloglink((f2$freq-f2$se),inverse = T)
    f2$set = f2$freqt-f2$cl
    
    fx = f2 %>%
      filter(!is.na(freqt) & !is.na(set)) %>%
      group_by(timegroups) %>% summarize(freq = mean(freqt), se = sqrt(sum(set^2)/n())) 
    
    f1 = left_join(f1,fx)
  }
  
  if(!isTRUE(error))
  {
    f2$freq = predict(m1, newdata = ltemp,
                      type="response", re.form = NA)
    f1 = f2 %>%
      filter(!is.na(freq)) %>%
      group_by(timegroups) %>% summarize(freq = mean(freq))
    f1$se = NA
  }
  
  
  f1$timegroups = factor(f1$timegroups, levels = c("before 2000","2000-2006","2007-2010",
                                                   "2011-2012","2013","2014","2015","2016",
                                                   "2017","2018","2019","2020","2021"))
  f1 = f1[order(f1$timegroups),]
  names(f1)[1] = "timegroupsf"
  mp = data.frame(timegroupsf = c("before 2000","2000-2006","2007-2010",
                                  "2011-2012","2013","2014","2015","2016",
                                  "2017","2018","2019","2020","2021"), 
                  timegroups = as.numeric(databins))
  f1 = left_join(f1,mp)
  f1$species = species
  
  if (is.na(specieslist1$ht) & !is.na(specieslist1$rt))
  {
    f1 = rbind(g1,f1)
  }
  
  tre = f1
  
  
  ########### end of freqtrends function
  
  if (c == 1)
  {
    trends = tre
  }
  if (c > 1)
  {
    trends = rbind(trends,tre)
  }
}

end = Sys.time()
print(end-start)

