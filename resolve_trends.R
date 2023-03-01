source('SoIB_v2 functions.R')
library(tidyverse)
library(VGAM)

load("dataforanalyses.RData")

trends1 = read.csv("trends_1.csv")
trends2 = read.csv("trends_2.csv")
trends = rbind(trends1,trends2)

trends = trends %>%
  group_by(COMMON.NAME,timegroupsf,timegroups) %>% 
  summarize(mean_trans = mean(freq), se_trans = sd(freq) + sqrt(sum(se^2))/n()) %>%
  mutate(lci = clogloglink(mean_trans - 1.96*se_trans,inverse = T),
         mean = clogloglink(mean_trans,inverse = T),
         rci = clogloglink(mean_trans + 1.96*se_trans,inverse = T)) %>%
  ungroup()
  
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


trends$COMMON.NAME = factor(trends$COMMON.NAME, levels = specieslist$COMMON.NAME)
trends = trends %>%
  arrange(COMMON.NAME,timegroups)

trends = trends %>%
  select(COMMON.NAME,timegroupsf,timegroups,mean_trans,se_trans,lci,mean,rci,
         lci_std,mean_std,rci_std,lci_std_2014,mean_std_2014,rci_std_2014)

write.csv(trends,"trends.csv",row.names=F)
