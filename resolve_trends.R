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
  group_by(COMMON.NAME) %>% mutate(s1 = first(se_trans)) %>% ungroup() %>%
  mutate(mean_std = mean/m2) %>%
  mutate(se_trans_stdx = as.numeric(errordiv(mean_trans,m1,se_trans,s1)[,2]))

modtrends_2014 = modtrends_2014 %>%
  group_by(COMMON.NAME) %>% mutate(m1 = first(mean_trans)) %>% ungroup() %>%
  group_by(COMMON.NAME) %>% mutate(m2_2014 = first(mean)) %>% ungroup() %>%
  group_by(COMMON.NAME) %>% mutate(s1 = first(se_trans)) %>% ungroup() %>%
  mutate(mean_std_2014 = mean/m2_2014) %>%
  mutate(se_trans_stdx_2014 = as.numeric(errordiv(mean_trans,m1,se_trans,s1)[,2]))

modtrends = modtrends %>%
  group_by(COMMON.NAME) %>% 
  mutate(se_trans_std = case_when(timegroups == first(timegroups) ~ 0,
                                  TRUE ~ se_trans_stdx)) %>%
  ungroup()

modtrends_2014 = modtrends_2014 %>%
  group_by(COMMON.NAME) %>% 
  mutate(se_trans_std_2014 = case_when(timegroups == first(timegroups) ~ 0,
                                  TRUE ~ se_trans_stdx_2014)) %>%
  ungroup()
         
modtrends = modtrends %>%
  dplyr::select(timegroupsf,timegroups,COMMON.NAME,se_trans_std,mean_std,m2)

modtrends_2014 = modtrends_2014 %>%
  dplyr::select(timegroupsf,timegroups,COMMON.NAME,se_trans_std_2014,mean_std_2014,m2_2014)

trends = left_join(trends,modtrends)
trends = left_join(trends,modtrends_2014)

trends = trends %>%
  mutate(lci_std = clogloglink(mean_trans - 1.96*se_trans_std,inverse = T)/m2,
         rci_std = clogloglink(mean_trans + 1.96*se_trans_std,inverse = T)/m2,
         lci_std_2014 = clogloglink(mean_trans - 1.96*se_trans_std_2014,inverse = T)/m2_2014,
         rci_std_2014 = clogloglink(mean_trans + 1.96*se_trans_std_2014,inverse = T)/m2_2014)


trends$mean_std = trends$mean_std*100
trends$lci_std = trends$lci_std*100
trends$rci_std = trends$rci_std*100

trends$mean_std_2014 = trends$mean_std_2014*100
trends$lci_std_2014 = trends$lci_std_2014*100
trends$rci_std_2014 = trends$rci_std_2014*100


trends$COMMON.NAME = factor(trends$COMMON.NAME, levels = specieslist$COMMON.NAME)
trends = trends %>%
  arrange(COMMON.NAME,timegroups)

trends = trends %>%
  select(COMMON.NAME,timegroupsf,timegroups,mean_trans,se_trans,lci,mean,rci,
         se_trans_std,lci_std,mean_std,rci_std,lci_std_2014,mean_std_2014,rci_std_2014)

write.csv(trends,"trends.csv",row.names=F)
