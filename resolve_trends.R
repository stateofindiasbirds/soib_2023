source('SoIB_v2 functions.R')
library(tidyverse)
library(VGAM)

load("specieslists.RData")
main = read.csv("SoIB_mapping_2022.csv")
base = read.csv("fullspecieslist.csv")
base = base %>% select(-SCIENTIFIC.NAME)
recentcutoff = 2015

main = left_join(main,base,by=c("eBird.English.Name.2022"="COMMON.NAME"))

main$longtermrci = main$longtermmean = main$longtermlci = NA 
main$currentsloperci = main$currentslopemean = main$currentslopelci = NA

file_names = dir("trends") #where you have your files
trends = do.call(rbind,lapply(paste("trends/",file_names,sep=""),read.csv))

## remove problem species from current and long-term trends
# long-term
trendsa = trends %>%
  filter(timegroups < 2015) %>%
  filter(COMMON.NAME %in% main$eBird.English.Name.2022[main$Long.Term.Analysis == "X"]) %>%
  group_by(sl,COMMON.NAME) %>%  
  filter(any(is.na(se) | se > 2*abs(freq))) %>%
  distinct(sl,COMMON.NAME)
specs_lt_remove = unique(trendsa$COMMON.NAME)

main$Long.Term.Analysis[main$eBird.English.Name.2022 %in% specs_lt_remove] = ""

# current
trendsb = trends %>%
  filter(timegroups >= 2015) %>%
  filter(COMMON.NAME %in% main$eBird.English.Name.2022[main$Current.Analysis == "X"]) %>%
  group_by(sl,COMMON.NAME) %>%  
  filter(any(is.na(se) | se > 2*abs(freq))) %>%
  distinct(sl,COMMON.NAME)
specs_ct_remove = unique(trendsb$COMMON.NAME)

main$Long.Term.Analysis[main$eBird.English.Name.2022 %in% specs_ct_remove] = ""
main$Current.Analysis[main$eBird.English.Name.2022 %in% specs_ct_remove] = ""


trends = trends %>%
  group_by(COMMON.NAME,timegroupsf,timegroups) %>% 
  reframe(mean_trans = mean(freq), se_trans = sd(freq) + sqrt(sum(se^2))/n()) %>%
  group_by(COMMON.NAME,timegroupsf,timegroups) %>% 
  mutate(lci = clogloglink(mean_trans - 1.96*se_trans,inverse = T),
         mean = clogloglink(mean_trans,inverse = T),
         rci = clogloglink(mean_trans + 1.96*se_trans,inverse = T)) %>%
  ungroup()

trends_framework = data.frame(timegroups = rep(c(unique(trends$timegroups),2023:2072),
                                               length(unique(trends$COMMON.NAME))),
                              COMMON.NAME = rep(unique(trends$COMMON.NAME),each=64))
trends_framework = left_join(trends_framework,trends[,1:3])
trends_framework$timegroupsf[is.na(trends_framework$timegroupsf)] = trends_framework$timegroups[is.na(trends_framework$timegroupsf)]
  
modtrends = na.omit(trends)
modtrends_recent = trends %>% filter(timegroups >= recentcutoff)

modtrends = modtrends %>%
  arrange(COMMON.NAME,timegroups)

modtrend_recents = modtrends_recent %>%
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
    
    tp0 = simerrordiv(mean_trans,m1,se_trans,s1)
    tp0$timegroups = j
    
    l = quantile(tp0$rat,0.025)
    r = quantile(tp0$rat,0.975)

    modtrends$lci_std[modtrends$COMMON.NAME == i & modtrends$timegroups == j] = 
      100*as.numeric(l)
    modtrends$rci_std[modtrends$COMMON.NAME == i & modtrends$timegroups == j] = 
      100*as.numeric(r)
    
    if (j == 2021)
    {
      longtermlci = modtrends$lci_std[modtrends$COMMON.NAME == i & modtrends$timegroups == j]
      longtermmean = modtrends$mean_std[modtrends$COMMON.NAME == i & modtrends$timegroups == j]
      longtermrci = modtrends$rci_std[modtrends$COMMON.NAME == i & modtrends$timegroups == j]
      
      main$longtermlci[main$eBird.English.Name.2022 == i] = longtermlci
      main$longtermmean[main$eBird.English.Name.2022 == i] = longtermmean
      main$longtermrci[main$eBird.English.Name.2022 == i] = longtermrci
    }
  }
  print(i)
}


modtrends_recent = modtrends_recent %>%
  group_by(COMMON.NAME) %>% mutate(m1 = first(mean_trans)) %>% ungroup() %>%
  group_by(COMMON.NAME) %>% mutate(m2 = first(mean)) %>% ungroup() %>%
  group_by(COMMON.NAME) %>% mutate(s1 = first(se_trans)) %>% ungroup()

modtrends_recent$lci_std_recent = NA
modtrends_recent$mean_std_recent = 100*modtrends_recent$mean/modtrends_recent$m2
modtrends_recent$rci_std_recent = NA

flag = 0
newdata = data.frame(timegroups = 2023:2072)
for (i in unique(modtrends_recent$COMMON.NAME))
{
  ct = 0
  for (j in unique(modtrends_recent$timegroups[modtrends_recent$COMMON.NAME == i]))
  {
    ct = ct + 1
    mean_trans = modtrends_recent$mean_trans[modtrends_recent$COMMON.NAME == i & modtrends_recent$timegroups == j]
    se_trans = modtrends_recent$se_trans[modtrends_recent$COMMON.NAME == i & modtrends_recent$timegroups == j]
    m1 = modtrends_recent$m1[modtrends_recent$COMMON.NAME == i & modtrends_recent$timegroups == j]
    s1 = modtrends_recent$s1[modtrends_recent$COMMON.NAME == i & modtrends_recent$timegroups == j]
    
    tp0 = simerrordiv(mean_trans,m1,se_trans,s1)
    tp0$timegroups = j
    
    l = quantile(tp0$rat,0.025)
    r = quantile(tp0$rat,0.975)
    
    modtrends_recent$lci_std_recent[modtrends_recent$COMMON.NAME == i & modtrends_recent$timegroups == j] = 
      100*as.numeric(l)
    modtrends_recent$rci_std_recent[modtrends_recent$COMMON.NAME == i & modtrends_recent$timegroups == j] = 
      100*as.numeric(r)
    
    if (ct == 1)
      tp = tp0
    if (ct > 1)
      tp = rbind(tp,tp0)
  }
  
  tp$rat = 100*tp$rat
  pred0 = newdata
  flag = flag + 1
  
  ct = 0
  if (!i %in% c("Rufous-vented Laughingthrush","Pale-footed Bush Warbler"))
  {
    sl = numeric(1000)
    slse = numeric(1000)
    for (z in 1:1000)
    {
      ct = ct + 1
      temp = tp %>% 
        group_by(timegroups) %>%
        reframe(val = sample(rat,1))
      temp$val1 = temp$val - 100
      temp$timegroups1 = temp$timegroups - recentcutoff
      
      fit = with(temp,lm(log(val)~timegroups))
      fit1 = with(temp,lm(val1~0+timegroups1))
      sl[z] = summary(fit1)$coefficients[1,1]
      slse[z] = summary(fit1)$coefficients[1,2]
      
      pd = predict(fit,newdata,se = T)
      
      pred0$mean = pd$fit
      pred0$se = pd$se.fit
      
      pred0$COMMON.NAME = i
      
      if (ct == 1)
        pred = pred0
      if (ct > 1)
        pred = rbind(pred,pred0)
    }
    
    se.slope = sd(sl) + sqrt(sum(slse^2)/length(slse))
    
    main$currentslopelci[main$eBird.English.Name.2022 == i] = mean(sl) - 1.96*se.slope
    main$currentslopemean[main$eBird.English.Name.2022 == i] = mean(sl)
    main$currentsloperci[main$eBird.English.Name.2022 == i] = mean(sl) + 1.96*se.slope
    
    pred = pred %>%
      mutate(lci_bt = exp(mean-1.96*se), mean_bt = exp(mean), rci_bt = exp(mean+1.96*se)) %>%
      group_by(COMMON.NAME,timegroups) %>% 
      reframe(lci_ext_std = mean(lci_bt),
             mean_ext_std = mean(mean_bt),
             rci_ext_std = mean(rci_bt))
    
    if (flag == 1)
      ext_trends = pred
    if (flag > 1)
      ext_trends = rbind(ext_trends,pred)
  }
  print(i)
}

#ext_trends = ext_trends %>% select(-c(lci_bt,mean_bt,rci_bt))


modtrends = modtrends %>%
  group_by(COMMON.NAME) %>% 
  mutate(lci_std = case_when(timegroups == first(timegroups) ~ mean_std,
                                  TRUE ~ lci_std)) %>%
  mutate(rci_std = case_when(timegroups == first(timegroups) ~ mean_std,
                                  TRUE ~ rci_std)) %>%
  ungroup()

modtrends_recent = modtrends_recent %>%
  group_by(COMMON.NAME) %>% 
  mutate(lci_std_recent = case_when(timegroups == first(timegroups) ~ mean_std_recent,
                             TRUE ~ lci_std_recent)) %>%
  mutate(rci_std_recent = case_when(timegroups == first(timegroups) ~ mean_std_recent,
                             TRUE ~ rci_std_recent)) %>%
  ungroup()
         
modtrends = modtrends %>%
  dplyr::select(timegroupsf,timegroups,COMMON.NAME,lci_std,mean_std,rci_std)

modtrends_recent = modtrends_recent %>%
  dplyr::select(timegroupsf,timegroups,COMMON.NAME,lci_std_recent,mean_std_recent,rci_std_recent)

trends = left_join(trends,modtrends)
trends = left_join(trends,modtrends_recent)

trends = trends %>%
  select(COMMON.NAME,timegroupsf,timegroups,mean_trans,se_trans,lci,mean,rci,
         lci_std,mean_std,rci_std,lci_std_recent,mean_std_recent,rci_std_recent)

trends = left_join(trends_framework,trends)
trends = left_join(trends,ext_trends)

trends$COMMON.NAME = factor(trends$COMMON.NAME, levels = specieslist$COMMON.NAME)
trends = trends %>%
  arrange(COMMON.NAME,timegroups)

trends$rci_comb_std = trends$mean_comb_std = trends$lci_comb_std = NA
trends$lci_comb_std[is.na(trends$lci_ext_std)] = trends$lci_std_recent[is.na(trends$lci_ext_std)]
trends$lci_comb_std[!is.na(trends$lci_ext_std)] = trends$lci_ext_std[!is.na(trends$lci_ext_std)]  
trends$mean_comb_std[is.na(trends$mean_ext_std)] = trends$mean_std_recent[is.na(trends$mean_ext_std)]
trends$mean_comb_std[!is.na(trends$mean_ext_std)] = trends$mean_ext_std[!is.na(trends$mean_ext_std)] 
trends$rci_comb_std[is.na(trends$rci_ext_std)] = trends$rci_std_recent[is.na(trends$rci_ext_std)]
trends$rci_comb_std[!is.na(trends$rci_ext_std)] = trends$rci_ext_std[!is.na(trends$rci_ext_std)] 


trends$lci_comb_std[trends$lci_comb_std<0] = 0

write.csv(trends,"trends.csv",row.names=F)

proj2028 = trends %>% filter(timegroups == 2028) %>% select(COMMON.NAME,rci_comb_std)
names(proj2028)[2] = "proj2028"
main = left_join(main,proj2028,by=c("eBird.English.Name.2022"="COMMON.NAME"))

proj2040 = trends %>% filter(timegroups == 2040) %>% select(COMMON.NAME,rci_comb_std)
names(proj2040)[2] = "proj2040"
main = left_join(main,proj2040,by=c("eBird.English.Name.2022"="COMMON.NAME"))

proj2050 = trends %>% filter(timegroups == 2050) %>% select(COMMON.NAME,rci_comb_std)
names(proj2050)[2] = "proj2050"
main = left_join(main,proj2050,by=c("eBird.English.Name.2022"="COMMON.NAME"))

write.csv(main,"SoIB_main.csv",row.names=F)
