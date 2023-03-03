source('SoIB_v2 functions.R')
library(tidyverse)
library(VGAM)

load("dataforanalyses.RData")

file_names = dir("trend_base_files") #where you have your files
trends = do.call(rbind,lapply(paste("trend_base_files/",file_names,sep=""),read.csv))


trends = trends %>%
  group_by(COMMON.NAME,timegroupsf,timegroups) %>% 
  summarize(mean_trans = mean(freq), se_trans = sd(freq) + sqrt(sum(se^2))/n()) %>%
  mutate(lci = clogloglink(mean_trans - 1.96*se_trans,inverse = T),
         mean = clogloglink(mean_trans,inverse = T),
         rci = clogloglink(mean_trans + 1.96*se_trans,inverse = T)) %>%
  ungroup()

trends_framework = data.frame(timegroups = rep(c(unique(trends$timegroups),2022:2030),720),
                              COMMON.NAME = rep(unique(trends$COMMON.NAME),each=22))
trends_framework = left_join(trends_framework,trends[,1:3])
trends_framework$timegroupsf[is.na(trends_framework$timegroupsf)] = trends_framework$timegroups[is.na(trends_framework$timegroupsf)]
  
modtrends = na.omit(trends)
modtrends_2014 = trends %>% filter(timegroups >= 2014)

modtrends = modtrends %>%
  arrange(COMMON.NAME,timegroups)

modtrend_2014s = modtrends_2014 %>%
  arrange(COMMON.NAME,timegroups)

modtrends = modtrends %>%
  group_by(COMMON.NAME) %>% mutate(m1 = first(mean_trans)) %>% ungroup() %>%
  group_by(COMMON.NAME) %>% mutate(m2 = first(mean)) %>% ungroup() %>%
  group_by(COMMON.NAME) %>% mutate(s1 = first(se_trans)) %>% ungroup()

modtrends$lci_std = NA
modtrends$mean_std = 100*modtrends$mean/modtrends$m2
modtrends$rci_std = NA

for (i in unique(modtrends$COMMON.NAME))
{
  for (j in unique(modtrends$timegroups[modtrends$COMMON.NAME == i]))
  {
    mean_trans = modtrends$mean_trans[modtrends$COMMON.NAME == i & modtrends$timegroups == j]
    se_trans = modtrends$se_trans[modtrends$COMMON.NAME == i & modtrends$timegroups == j]
    m1 = modtrends$m1[modtrends$COMMON.NAME == i & modtrends$timegroups == j]
    s1 = modtrends$s1[modtrends$COMMON.NAME == i & modtrends$timegroups == j]
    
    modtrends$lci_std[modtrends$COMMON.NAME == i & modtrends$timegroups == j] = 
      100*as.numeric(simerrordiv(mean_trans,m1,se_trans,s1)[1])
    modtrends$rci_std[modtrends$COMMON.NAME == i & modtrends$timegroups == j] = 
      100*as.numeric(simerrordiv(mean_trans,m1,se_trans,s1)[2])
  }
}



modtrends_2014 = modtrends_2014 %>%
  group_by(COMMON.NAME) %>% mutate(m1 = first(mean_trans)) %>% ungroup() %>%
  group_by(COMMON.NAME) %>% mutate(m2 = first(mean)) %>% ungroup() %>%
  group_by(COMMON.NAME) %>% mutate(s1 = first(se_trans)) %>% ungroup()

modtrends_2014$lci_std_2014 = NA
modtrends_2014$mean_std_2014 = 100*modtrends_2014$mean/modtrends_2014$m2
modtrends_2014$rci_std_2014 = NA


for (i in unique(modtrends_2014$COMMON.NAME))
{
  for (j in unique(modtrends_2014$timegroups[modtrends_2014$COMMON.NAME == i]))
  {
    mean_trans = modtrends_2014$mean_trans[modtrends_2014$COMMON.NAME == i & modtrends_2014$timegroups == j]
    se_trans = modtrends_2014$se_trans[modtrends_2014$COMMON.NAME == i & modtrends_2014$timegroups == j]
    m1 = modtrends_2014$m1[modtrends_2014$COMMON.NAME == i & modtrends_2014$timegroups == j]
    s1 = modtrends_2014$s1[modtrends_2014$COMMON.NAME == i & modtrends_2014$timegroups == j]
    
    modtrends_2014$lci_std_2014[modtrends_2014$COMMON.NAME == i & modtrends_2014$timegroups == j] = 
      100*as.numeric(simerrordiv(mean_trans,m1,se_trans,s1)[1])
    modtrends_2014$rci_std_2014[modtrends_2014$COMMON.NAME == i & modtrends_2014$timegroups == j] = 
      100*as.numeric(simerrordiv(mean_trans,m1,se_trans,s1)[2])
  }
}

# set the number of years of interest
newdata = data.frame(timegroups = 2022:2030)
flag = 0
for (i in unique(modtrends_2014$COMMON.NAME))
{
  print(i)
  pred0 = newdata
  flag = flag + 1
  count = 0
  for (j in unique(modtrends_2014$timegroups[modtrends_2014$COMMON.NAME == i]))
  {
    count = count + 1
    mean_trans = modtrends_2014$mean_trans[modtrends_2014$COMMON.NAME == i & modtrends_2014$timegroups == j]
    se_trans = modtrends_2014$se_trans[modtrends_2014$COMMON.NAME == i & modtrends_2014$timegroups == j]

    tp0 = data.frame(val = clogloglink(rnorm(1000,mean_trans,se_trans), inverse = T),
                     COMMON.NAME = i,
                     timegroups = j,
                     sl = 1:1000)
    
    if (count == 1)
      tp = tp0
    if (count > 1)
      tp = rbind(tp,tp0)
  }
  
  ct = 0
  
  if (!i %in% c("Rufous-vented Laughingthrush","Pale-footed Bush Warbler"))
  {
    for (z in 1:1000)
    {
      ct = ct + 1
      temp = tp %>% filter(sl == z)
      
      fit = with(temp,lm(log(val)~timegroups))
      pd = predict(fit,newdata,se = T)
      
      pred0$mean = pd$fit
      pred0$se = pd$se.fit
      
      pred0$COMMON.NAME = i
      
      if (ct == 1)
        pred = pred0
      if (ct > 1)
        pred = rbind(pred,pred0)
    }
    
    pred = pred %>%
      group_by(COMMON.NAME,timegroups) %>% 
      summarize(ext_mean_trans = mean(mean),
                ext_se_trans = sd(mean) + sqrt(sum(se^2))/n()) %>%
      ungroup %>%
      mutate(lci_ext = exp(ext_mean_trans-1.96*ext_se_trans),
             mean_ext = exp(ext_mean_trans),
             rci_ext = exp(ext_mean_trans+1.96*ext_se_trans))
    
    if (flag == 1)
      ext_trends = pred
    if (flag > 1)
      ext_trends = rbind(ext_trends,pred)
  }
}

ext_trends = ext_trends %>% select(-c(ext_mean_trans,ext_se_trans))


modtrends = modtrends %>%
  group_by(COMMON.NAME) %>% 
  mutate(lci_std = case_when(timegroups == first(timegroups) ~ mean_std,
                                  TRUE ~ lci_std)) %>%
  mutate(rci_std = case_when(timegroups == first(timegroups) ~ mean_std,
                                  TRUE ~ rci_std)) %>%
  ungroup()

modtrends_2014 = modtrends_2014 %>%
  group_by(COMMON.NAME) %>% 
  mutate(lci_std_2014 = case_when(timegroups == first(timegroups) ~ mean_std_2014,
                             TRUE ~ lci_std_2014)) %>%
  mutate(rci_std_2014 = case_when(timegroups == first(timegroups) ~ mean_std_2014,
                             TRUE ~ rci_std_2014)) %>%
  ungroup()
         
modtrends = modtrends %>%
  dplyr::select(timegroupsf,timegroups,COMMON.NAME,lci_std,mean_std,rci_std)

modtrends_2014 = modtrends_2014 %>%
  dplyr::select(timegroupsf,timegroups,COMMON.NAME,lci_std_2014,mean_std_2014,rci_std_2014)

trends = left_join(trends,modtrends)
trends = left_join(trends,modtrends_2014)

trends = trends %>%
  select(COMMON.NAME,timegroupsf,timegroups,mean_trans,se_trans,lci,mean,rci,
         lci_std,mean_std,rci_std,lci_std_2014,mean_std_2014,rci_std_2014)

trends = left_join(trends_framework,trends)
trends = left_join(trends,ext_trends)

trends$COMMON.NAME = factor(trends$COMMON.NAME, levels = specieslist$COMMON.NAME)
trends = trends %>%
  arrange(COMMON.NAME,timegroups)

trends$rci_comb = trends$mean_comb = trends$lci_comb = NA
trends$lci_comb[is.na(trends$lci_ext)] = trends$lci[is.na(trends$lci_ext)]
trends$lci_comb[!is.na(trends$lci_ext)] = trends$lci_ext[!is.na(trends$lci_ext)]  
trends$mean_comb[is.na(trends$mean_ext)] = trends$mean[is.na(trends$mean_ext)]
trends$mean_comb[!is.na(trends$mean_ext)] = trends$mean_ext[!is.na(trends$mean_ext)] 
trends$rci_comb[is.na(trends$rci_ext)] = trends$rci[is.na(trends$rci_ext)]
trends$rci_comb[!is.na(trends$rci_ext)] = trends$rci_ext[!is.na(trends$rci_ext)] 
trends$se_comb = (trends$mean_comb-trends$lci_comb)/1.96

#### standardize extrapolated to 2014 assuming symmetric SEs

extra_2014 = trends %>% filter(timegroups >= 2014, !is.na(mean_comb)) 

extra_2014 = extra_2014 %>%
  group_by(COMMON.NAME) %>% mutate(m1 = first(mean_comb)) %>% ungroup() %>%
  group_by(COMMON.NAME) %>% mutate(s1 = first(se_comb)) %>% ungroup() %>%
  mutate(mean_comb_std = 100*mean_comb/m1) %>%
  mutate(se_comb_std = 100*as.numeric(errordiv(mean_comb,m1,se_comb,s1)[,2])) %>%
  mutate(lci_comb_std = mean_comb_std-1.96*se_comb_std) %>%
  mutate(rci_comb_std = mean_comb_std+1.96*se_comb_std) %>%
  select(COMMON.NAME,timegroups,lci_comb_std,mean_comb_std,rci_comb_std)

extra_2014 = extra_2014 %>%
  group_by(COMMON.NAME) %>% 
  mutate(lci_comb_std = case_when(timegroups == first(timegroups) ~ mean_comb_std,
                                  TRUE ~ lci_comb_std)) %>%
  mutate(rci_comb_std = case_when(timegroups == first(timegroups) ~ mean_comb_std,
                                  TRUE ~ rci_comb_std)) %>%
  ungroup()

trends = left_join(trends,extra_2014)
trends$lci_comb_std[trends$lci_comb_std<0] = 0

write.csv(trends,"trends.csv",row.names=F)
