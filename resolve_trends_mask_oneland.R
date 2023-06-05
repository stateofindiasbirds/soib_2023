source('SoIB_v2 functions.R')
library(tidyverse)
library(VGAM)

main = read.csv("SoIB_mapping_2022.csv")
base = read.csv("fullspecieslist_mask_oneland.csv")
base = base %>% select(-SCIENTIFIC.NAME)
recentcutoff = 2015

main = left_join(main,base,by=c("eBird.English.Name.2022"="COMMON.NAME"))

main$longtermrci = main$longtermmean = main$longtermlci = NA 
main$currentsloperci = main$currentslopemean = main$currentslopelci = NA

#################### sensitivity

sens = main %>% select(eBird.English.Name.2022)
sens$currentsloperci1 = sens$currentslopemean1 = sens$currentslopelci1 = NA
sens$currentsloperci2 = sens$currentslopemean2 = sens$currentslopelci2 = NA
sens$currentsloperci3 = sens$currentslopemean3 = sens$currentslopelci3 = NA
sens$currentsloperci4 = sens$currentslopemean4 = sens$currentslopelci4 = NA
sens$currentsloperci5 = sens$currentslopemean5 = sens$currentslopelci5 = NA
sens$currentsloperci6 = sens$currentslopemean6 = sens$currentslopelci6 = NA
sens$currentsloperci7 = sens$currentslopemean7 = sens$currentslopelci7 = NA
sens$currentsloperci8 = sens$currentslopemean8 = sens$currentslopelci8 = NA

################### sensitivity

file_names = dir("trends_mask_oneland") #where you have your files
trends = do.call(rbind,lapply(paste("trends_mask_oneland/",file_names,sep=""),read.csv))
totsims = length(unique(trends$sl))

## remove problem species from current and long-term trends
# long-term
trendsa = trends %>%
  filter(timegroups < 2015) %>%
  filter(COMMON.NAME %in% main$eBird.English.Name.2022[main$Long.Term.Analysis == "X"]) %>%
  group_by(sl,COMMON.NAME) %>%  
  filter(any(is.na(se) | se > abs(freq))) %>%
  distinct(sl,COMMON.NAME)

if (length(trendsa$sl) > 0)
{
  tab_lt_rem = data.frame(table(trendsa$COMMON.NAME))
  names(tab_lt_rem) = c("COMMON.NAME","count")
  tab_lt_rem$COMMON.NAME = as.character(tab_lt_rem$COMMON.NAME)
  
  specs_lt_remove = tab_lt_rem$COMMON.NAME[tab_lt_rem$count >= round(totsims/2)]
  specs_lt_remove_part = tab_lt_rem$COMMON.NAME[tab_lt_rem$count < round(totsims/2)]
  
  trends = trends %>%
    mutate(freq = 
             case_when(timegroups < 2015 & COMMON.NAME %in% specs_lt_remove ~ NA,
                       TRUE ~ freq),
           se = 
             case_when(timegroups < 2015 & COMMON.NAME %in% specs_lt_remove ~ NA,
                       TRUE ~ se))
  main$Long.Term.Analysis[main$eBird.English.Name.2022 %in% specs_lt_remove] = ""
  
  trendsa = trendsa %>%
    filter(COMMON.NAME %in% specs_lt_remove_part) %>%
    distinct(sl,COMMON.NAME)
  trendsa = trendsa %>%
    mutate(comb = paste(sl,COMMON.NAME))
  trends = trends %>%
    mutate(comb = paste(sl,COMMON.NAME)) %>%
    filter(!comb %in% trendsa$comb) %>%
    select(-comb)
}







# current


trendsb = trends %>%
  filter(timegroups >= 2015) %>%
  filter(COMMON.NAME %in% main$eBird.English.Name.2022[main$Current.Analysis == "X"]) %>%
  group_by(sl,COMMON.NAME) %>%  
  filter(any(is.na(se) | se > abs(freq))) %>%
  distinct(sl,COMMON.NAME)
  
if (length(trendsb$sl) > 0)
  {
  tab_ct_rem = data.frame(table(trendsb$COMMON.NAME))
  names(tab_ct_rem) = c("COMMON.NAME","count")
  tab_ct_rem$COMMON.NAME = as.character(tab_ct_rem$COMMON.NAME)
  
  specs_ct_remove = tab_ct_rem$COMMON.NAME[tab_ct_rem$count >= round(totsims/2)]
  specs_ct_remove_part = tab_ct_rem$COMMON.NAME[tab_ct_rem$count < round(totsims/2)]
  
  trends = trends %>%
    filter(!COMMON.NAME %in% specs_ct_remove)
  
  main$Long.Term.Analysis[main$eBird.English.Name.2022 %in% specs_ct_remove] = ""
  main$Current.Analysis[main$eBird.English.Name.2022 %in% specs_ct_remove] = ""
  
  trendsb = trendsb %>%
    filter(COMMON.NAME %in% specs_ct_remove_part) %>%
    distinct(sl,COMMON.NAME)
  trendsb = trendsb %>%
    mutate(comb = paste(sl,COMMON.NAME))
  trends = trends %>%
    mutate(comb = paste(sl,COMMON.NAME)) %>%
    filter(!comb %in% trendsb$comb) %>%
    select(-comb)
}



## remove species based on 2 extra metrics

## number of sampled 5km within (not for PAs)

specsc1 = main$eBird.English.Name.2022[!is.na(main$mean5km) & main$mean5km < 8 &
                                         (main$Long.Term.Analysis == "X" |
                                            main$Current.Analysis == "X") &
                                         !main$Endemic.Region %in% c("Andaman and Nicobar Islands",
                                                                     "Andaman Islands",
                                                                     "Nicobar Islands")]

specsc2 = main$eBird.English.Name.2022[!is.na(main$ci5km) & (main$ci5km/main$mean5km) > 0.25 &
                                         (main$Long.Term.Analysis == "X" |
                                            main$Current.Analysis == "X") &
                                         !main$Endemic.Region %in% c("Andaman and Nicobar Islands",
                                                                     "Andaman Islands",
                                                                     "Nicobar Islands")]
specsc = union(specsc1,specsc2)

trends = trends %>%
  filter(!COMMON.NAME %in% specsc)

main$Long.Term.Analysis[main$eBird.English.Name.2022 %in% specsc] = ""
main$Current.Analysis[main$eBird.English.Name.2022 %in% specsc] = ""



## proportion of 25km sampled

specsd1 = main$eBird.English.Name.2022[!is.na(main$proprange25km2000) & 
                                         (main$proprange25km2000/main$proprange25km2022) < 0.075 &
                                         (main$Long.Term.Analysis == "X")]

specsd2 = main$eBird.English.Name.2022[!is.na(main$proprange25km2000) & 
                                         main$proprange25km2000 < 0.04 &
                                         (main$Long.Term.Analysis == "X")]

specsd = union(specsd1,specsd2)


trends = trends %>%
  mutate(freq = 
           case_when(timegroups < 2015 & COMMON.NAME %in% specsd ~ NA,
                     TRUE ~ freq),
         se = 
           case_when(timegroups < 2015 & COMMON.NAME %in% specsd ~ NA,
                     TRUE ~ se))
main$Long.Term.Analysis[main$eBird.English.Name.2022 %in% specsd] = ""



specsd3 = main$eBird.English.Name.2022[!is.na(main$proprange25km.current) & 
                                         (main$proprange25km.current/main$proprange25km2022) < 0.6 &
                                         (main$Current.Analysis == "X")]

trends = trends %>%
  filter(!COMMON.NAME %in% specsd)

main$Long.Term.Analysis[main$eBird.English.Name.2022 %in% specsd] = ""
main$Current.Analysis[main$eBird.English.Name.2022 %in% specsd] = ""





## calculations

trends = trends %>%
  group_by(COMMON.NAME,timegroupsf,timegroups) %>% 
  reframe(mean_trans = mean(freq), se_trans = sd(freq) + sqrt(sum(se^2))/n()) %>%
  group_by(COMMON.NAME,timegroupsf,timegroups) %>% 
  mutate(lci = clogloglink(mean_trans - 1.96*se_trans,inverse = T),
         mean = clogloglink(mean_trans,inverse = T),
         rci = clogloglink(mean_trans + 1.96*se_trans,inverse = T)) %>%
  ungroup()

extra.years = 2023:2029

trends_framework = data.frame(timegroups = rep(c(unique(trends$timegroups),extra.years),
                                               length(unique(trends$COMMON.NAME))),
                              COMMON.NAME = 
                                rep(unique(trends$COMMON.NAME),
                                    each=(length(unique(trends$timegroups))+length(extra.years))))
trends_framework = left_join(trends_framework,trends[,1:3])
trends_framework$timegroupsf[is.na(trends_framework$timegroupsf)] = trends_framework$timegroups[is.na(trends_framework$timegroupsf)]

modtrends = na.omit(trends)
modtrends_recent = trends %>% filter(timegroups >= recentcutoff)

modtrends = modtrends %>%
  filter(COMMON.NAME %in% main$eBird.English.Name.2022[main$Long.Term.Analysis == "X"]) %>%
  arrange(COMMON.NAME,timegroups)

modtrends_recent = modtrends_recent %>%
  filter(COMMON.NAME %in% main$eBird.English.Name.2022[main$Current.Analysis == "X"]) %>%
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
    
    if (j == 2022)
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
newdata = data.frame(timegroups = extra.years)
newdata1 = data.frame(timegroups = unique(modtrends_recent$timegroups))

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
  
  pred0 = newdata
  flag = flag + 1
  
  ct = 0
  sl = numeric(1000)
  slse = numeric(1000)
  
  sl.sens1 = sl.sens2 = sl.sens3 = sl.sens4 = sl.sens5 = sl.sens6 = sl.sens7 = sl.sens8 = numeric(1000)
  slse.sens1 = slse.sens2 = slse.sens3 = slse.sens4 = slse.sens5 = slse.sens6 = slse.sens7 = slse.sens8 = numeric(1000)
  
  for (z in 1:1000)
  {
    ct = ct + 1
    temp = tp %>% 
      group_by(timegroups) %>%
      reframe(val = sample(val,1))
    
    fit = with(temp,lm(log(val)~timegroups))
    fit1 = with(temp,lm(val~timegroups)) #CHANGE
    
    pd1 = predict(fit1,newdata1,se = T)
    num = pd1$fit[2]-pd1$fit[1]
    den = abs(pd1$fit[1])
    numse = sqrt(pd1$se.fit[1]^2 + pd1$se.fit[2]^2)
    dense = pd1$se.fit[1]
    sl[z] = 100*errordiv(num,den,numse,dense)[1]
    slse[z] = errordiv(num,den,numse,dense)[2]
    
    #################################### sensitivity analysis
    
    fit1.sens = with(temp[temp$timegroups != 2015,],lm(val~timegroups))
    
    pd1 = predict(fit1.sens,
                  data.frame(timegroups = unique(temp$timegroups[temp$timegroups != 2015])),se = T)
    num = pd1$fit[2]-pd1$fit[1]
    den = abs(pd1$fit[1])
    numse = sqrt(pd1$se.fit[1]^2 + pd1$se.fit[2]^2)
    dense = pd1$se.fit[1]
    sl.sens1[z] = 100*errordiv(num,den,numse,dense)[1]
    slse.sens1[z] = errordiv(num,den,numse,dense)[2]
    
    
    fit1.sens = with(temp[temp$timegroups != 2016,],lm(val~timegroups))
    
    pd1 = predict(fit1.sens,
                  data.frame(timegroups = unique(temp$timegroups[temp$timegroups != 2016])),se = T)
    num = pd1$fit[2]-pd1$fit[1]
    den = abs(pd1$fit[1])
    numse = sqrt(pd1$se.fit[1]^2 + pd1$se.fit[2]^2)
    dense = pd1$se.fit[1]
    sl.sens2[z] = 100*errordiv(num,den,numse,dense)[1]
    slse.sens2[z] = errordiv(num,den,numse,dense)[2]
    
    
    fit1.sens = with(temp[temp$timegroups != 2017,],lm(val~timegroups))
    
    pd1 = predict(fit1.sens,
                  data.frame(timegroups = unique(temp$timegroups[temp$timegroups != 2017])),se = T)
    num = pd1$fit[2]-pd1$fit[1]
    den = abs(pd1$fit[1])
    numse = sqrt(pd1$se.fit[1]^2 + pd1$se.fit[2]^2)
    dense = pd1$se.fit[1]
    sl.sens3[z] = 100*errordiv(num,den,numse,dense)[1]
    slse.sens3[z] = errordiv(num,den,numse,dense)[2]
    
    
    fit1.sens = with(temp[temp$timegroups != 2018,],lm(val~timegroups))
    
    pd1 = predict(fit1.sens,
                  data.frame(timegroups = unique(temp$timegroups[temp$timegroups != 2018])),se = T)
    num = pd1$fit[2]-pd1$fit[1]
    den = abs(pd1$fit[1])
    numse = sqrt(pd1$se.fit[1]^2 + pd1$se.fit[2]^2)
    dense = pd1$se.fit[1]
    sl.sens4[z] = 100*errordiv(num,den,numse,dense)[1]
    slse.sens4[z] = errordiv(num,den,numse,dense)[2]
    
    
    
    fit1.sens = with(temp[temp$timegroups != 2019,],lm(val~timegroups))
    
    pd1 = predict(fit1.sens,
                  data.frame(timegroups = unique(temp$timegroups[temp$timegroups != 2019])),se = T)
    num = pd1$fit[2]-pd1$fit[1]
    den = abs(pd1$fit[1])
    numse = sqrt(pd1$se.fit[1]^2 + pd1$se.fit[2]^2)
    dense = pd1$se.fit[1]
    sl.sens5[z] = 100*errordiv(num,den,numse,dense)[1]
    slse.sens5[z] = errordiv(num,den,numse,dense)[2]
    
    
    
    fit1.sens = with(temp[temp$timegroups != 2020,],lm(val~timegroups))
    
    pd1 = predict(fit1.sens,
                  data.frame(timegroups = unique(temp$timegroups[temp$timegroups != 2020])),se = T)
    num = pd1$fit[2]-pd1$fit[1]
    den = abs(pd1$fit[1])
    numse = sqrt(pd1$se.fit[1]^2 + pd1$se.fit[2]^2)
    dense = pd1$se.fit[1]
    sl.sens6[z] = 100*errordiv(num,den,numse,dense)[1]
    slse.sens6[z] = errordiv(num,den,numse,dense)[2]
    
    
    
    fit1.sens = with(temp[temp$timegroups != 2021,],lm(val~timegroups))
    
    pd1 = predict(fit1.sens,
                  data.frame(timegroups = unique(temp$timegroups[temp$timegroups != 2021])),se = T)
    num = pd1$fit[2]-pd1$fit[1]
    den = abs(pd1$fit[1])
    numse = sqrt(pd1$se.fit[1]^2 + pd1$se.fit[2]^2)
    dense = pd1$se.fit[1]
    sl.sens7[z] = 100*errordiv(num,den,numse,dense)[1]
    slse.sens7[z] = errordiv(num,den,numse,dense)[2]
    
    
    
    fit1.sens = with(temp[temp$timegroups != 2022,],lm(val~timegroups))
    
    pd1 = predict(fit1.sens,
                  data.frame(timegroups = unique(temp$timegroups[temp$timegroups != 2022])),se = T)
    num = pd1$fit[2]-pd1$fit[1]
    den = abs(pd1$fit[1])
    numse = sqrt(pd1$se.fit[1]^2 + pd1$se.fit[2]^2)
    dense = pd1$se.fit[1]
    sl.sens8[z] = 100*errordiv(num,den,numse,dense)[1]
    slse.sens8[z] = errordiv(num,den,numse,dense)[2]
    
    
    
    #####################################
    
    pd = predict(fit,newdata,se = T)
    
    pred0$mean = pd$fit
    pred0$se = pd$se.fit
    
    pred0$COMMON.NAME = i
    pred0$val = temp$val[temp$timegroups == recentcutoff]
    
    if (ct == 1)
      pred = pred0
    if (ct > 1)
      pred = rbind(pred,pred0)
  }
  
  sl = as.numeric(sl)
  slse = as.numeric(slse)
  se.slope = sd(sl) + sqrt(sum(slse^2)/length(slse))
  
  main$currentslopelci[main$eBird.English.Name.2022 == i] = mean(sl) - 1.96*se.slope
  main$currentslopemean[main$eBird.English.Name.2022 == i] = mean(sl)
  main$currentsloperci[main$eBird.English.Name.2022 == i] = mean(sl) + 1.96*se.slope
  
  ######################### sensitivity
  
  sl.sens1 = as.numeric(sl.sens1)
  slse.sens1 = as.numeric(slse.sens1)
  se.slope.sens1 = sd(sl.sens1) + sqrt(sum(slse.sens1^2)/length(slse.sens1))
  
  sens$currentslopelci1[sens$eBird.English.Name.2022 == i] = mean(sl.sens1) - 1.96*se.slope.sens1
  sens$currentslopemean1[sens$eBird.English.Name.2022 == i] = mean(sl.sens1)
  sens$currentsloperci1[sens$eBird.English.Name.2022 == i] = mean(sl.sens1) + 1.96*se.slope.sens1
  
  sl.sens2 = as.numeric(sl.sens2)
  slse.sens2 = as.numeric(slse.sens2)
  se.slope.sens2 = sd(sl.sens2) + sqrt(sum(slse.sens2^2)/length(slse.sens2))
  
  sens$currentslopelci2[sens$eBird.English.Name.2022 == i] = mean(sl.sens2) - 1.96*se.slope.sens2
  sens$currentslopemean2[sens$eBird.English.Name.2022 == i] = mean(sl.sens2)
  sens$currentsloperci2[sens$eBird.English.Name.2022 == i] = mean(sl.sens2) + 1.96*se.slope.sens2
  
  sl.sens3 = as.numeric(sl.sens3)
  slse.sens3 = as.numeric(slse.sens3)
  se.slope.sens3 = sd(sl.sens3) + sqrt(sum(slse.sens3^2)/length(slse.sens3))
  
  sens$currentslopelci3[sens$eBird.English.Name.2022 == i] = mean(sl.sens3) - 1.96*se.slope.sens3
  sens$currentslopemean3[sens$eBird.English.Name.2022 == i] = mean(sl.sens3)
  sens$currentsloperci3[sens$eBird.English.Name.2022 == i] = mean(sl.sens3) + 1.96*se.slope.sens3
  
  sl.sens4 = as.numeric(sl.sens4)
  slse.sens4 = as.numeric(slse.sens4)
  se.slope.sens4 = sd(sl.sens4) + sqrt(sum(slse.sens4^2)/length(slse.sens4))
  
  sens$currentslopelci4[sens$eBird.English.Name.2022 == i] = mean(sl.sens4) - 1.96*se.slope.sens4
  sens$currentslopemean4[sens$eBird.English.Name.2022 == i] = mean(sl.sens4)
  sens$currentsloperci4[sens$eBird.English.Name.2022 == i] = mean(sl.sens4) + 1.96*se.slope.sens4
  
  sl.sens5 = as.numeric(sl.sens5)
  slse.sens5 = as.numeric(slse.sens5)
  se.slope.sens5 = sd(sl.sens5) + sqrt(sum(slse.sens5^2)/length(slse.sens5))
  
  sens$currentslopelci5[sens$eBird.English.Name.2022 == i] = mean(sl.sens5) - 1.96*se.slope.sens5
  sens$currentslopemean5[sens$eBird.English.Name.2022 == i] = mean(sl.sens5)
  sens$currentsloperci5[sens$eBird.English.Name.2022 == i] = mean(sl.sens5) + 1.96*se.slope.sens5
  
  sl.sens6 = as.numeric(sl.sens6)
  slse.sens6 = as.numeric(slse.sens6)
  se.slope.sens6 = sd(sl.sens6) + sqrt(sum(slse.sens6^2)/length(slse.sens6))
  
  sens$currentslopelci6[sens$eBird.English.Name.2022 == i] = mean(sl.sens6) - 1.96*se.slope.sens6
  sens$currentslopemean6[sens$eBird.English.Name.2022 == i] = mean(sl.sens6)
  sens$currentsloperci6[sens$eBird.English.Name.2022 == i] = mean(sl.sens6) + 1.96*se.slope.sens6
  
  sl.sens7 = as.numeric(sl.sens7)
  slse.sens7 = as.numeric(slse.sens7)
  se.slope.sens7 = sd(sl.sens7) + sqrt(sum(slse.sens7^2)/length(slse.sens7))
  
  sens$currentslopelci7[sens$eBird.English.Name.2022 == i] = mean(sl.sens7) - 1.96*se.slope.sens7
  sens$currentslopemean7[sens$eBird.English.Name.2022 == i] = mean(sl.sens7)
  sens$currentsloperci7[sens$eBird.English.Name.2022 == i] = mean(sl.sens7) + 1.96*se.slope.sens7
  
  sl.sens8 = as.numeric(sl.sens8)
  slse.sens8 = as.numeric(slse.sens8)
  se.slope.sens8 = sd(sl.sens8) + sqrt(sum(slse.sens8^2)/length(slse.sens8))
  
  sens$currentslopelci8[sens$eBird.English.Name.2022 == i] = mean(sl.sens8) - 1.96*se.slope.sens8
  sens$currentslopemean8[sens$eBird.English.Name.2022 == i] = mean(sl.sens8)
  sens$currentsloperci8[sens$eBird.English.Name.2022 == i] = mean(sl.sens8) + 1.96*se.slope.sens8
  
  ########################
  
  pred = pred %>%
    mutate(lci_bt = 100*exp(mean-1.96*se)/val, mean_bt = 100*exp(mean)/val, rci_bt = 100*exp(mean+1.96*se)/val) %>%
    group_by(COMMON.NAME,timegroups) %>% 
    reframe(lci_ext_std = mean(lci_bt),
            mean_ext_std = mean(mean_bt),
            rci_ext_std = mean(rci_bt))
  
  if (flag == 1)
    ext_trends = pred
  if (flag > 1)
    ext_trends = rbind(ext_trends,pred)
  
  print(i)
}


write.csv(sens,"trends_results/mask_one/current_sensitivity.csv",row.names=F)


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

trends$COMMON.NAME = factor(trends$COMMON.NAME, levels = base$COMMON.NAME)
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

write.csv(trends,"trends_results/mask_one/trends_mask_oneland.csv",row.names=F)

# 2023

proj2023.lci = trends %>% filter(timegroups == 2023) %>% select(COMMON.NAME,lci_comb_std)
names(proj2023.lci)[2] = "proj2023.lci"
main = left_join(main,proj2023.lci,by=c("eBird.English.Name.2022"="COMMON.NAME"))

proj2023.mean = trends %>% filter(timegroups == 2023) %>% select(COMMON.NAME,mean_comb_std)
names(proj2023.mean)[2] = "proj2023.mean"
main = left_join(main,proj2023.mean,by=c("eBird.English.Name.2022"="COMMON.NAME"))

proj2023.rci = trends %>% filter(timegroups == 2023) %>% select(COMMON.NAME,rci_comb_std)
names(proj2023.rci)[2] = "proj2023.rci"
main = left_join(main,proj2023.rci,by=c("eBird.English.Name.2022"="COMMON.NAME"))

# 2024

proj2024.lci = trends %>% filter(timegroups == 2024) %>% select(COMMON.NAME,lci_comb_std)
names(proj2024.lci)[2] = "proj2024.lci"
main = left_join(main,proj2024.lci,by=c("eBird.English.Name.2022"="COMMON.NAME"))

proj2024.mean = trends %>% filter(timegroups == 2024) %>% select(COMMON.NAME,mean_comb_std)
names(proj2024.mean)[2] = "proj2024.mean"
main = left_join(main,proj2024.mean,by=c("eBird.English.Name.2022"="COMMON.NAME"))

proj2024.rci = trends %>% filter(timegroups == 2024) %>% select(COMMON.NAME,rci_comb_std)
names(proj2024.rci)[2] = "proj2024.rci"
main = left_join(main,proj2024.rci,by=c("eBird.English.Name.2022"="COMMON.NAME"))

# 2025

proj2025.lci = trends %>% filter(timegroups == 2025) %>% select(COMMON.NAME,lci_comb_std)
names(proj2025.lci)[2] = "proj2025.lci"
main = left_join(main,proj2025.lci,by=c("eBird.English.Name.2022"="COMMON.NAME"))

proj2025.mean = trends %>% filter(timegroups == 2025) %>% select(COMMON.NAME,mean_comb_std)
names(proj2025.mean)[2] = "proj2025.mean"
main = left_join(main,proj2025.mean,by=c("eBird.English.Name.2022"="COMMON.NAME"))

proj2025.rci = trends %>% filter(timegroups == 2025) %>% select(COMMON.NAME,rci_comb_std)
names(proj2025.rci)[2] = "proj2025.rci"
main = left_join(main,proj2025.rci,by=c("eBird.English.Name.2022"="COMMON.NAME"))

# 2026

proj2026.lci = trends %>% filter(timegroups == 2026) %>% select(COMMON.NAME,lci_comb_std)
names(proj2026.lci)[2] = "proj2026.lci"
main = left_join(main,proj2026.lci,by=c("eBird.English.Name.2022"="COMMON.NAME"))

proj2026.mean = trends %>% filter(timegroups == 2026) %>% select(COMMON.NAME,mean_comb_std)
names(proj2026.mean)[2] = "proj2026.mean"
main = left_join(main,proj2026.mean,by=c("eBird.English.Name.2022"="COMMON.NAME"))

proj2026.rci = trends %>% filter(timegroups == 2026) %>% select(COMMON.NAME,rci_comb_std)
names(proj2026.rci)[2] = "proj2026.rci"
main = left_join(main,proj2026.rci,by=c("eBird.English.Name.2022"="COMMON.NAME"))

# 2027

proj2027.lci = trends %>% filter(timegroups == 2027) %>% select(COMMON.NAME,lci_comb_std)
names(proj2027.lci)[2] = "proj2027.lci"
main = left_join(main,proj2027.lci,by=c("eBird.English.Name.2022"="COMMON.NAME"))

proj2027.mean = trends %>% filter(timegroups == 2027) %>% select(COMMON.NAME,mean_comb_std)
names(proj2027.mean)[2] = "proj2027.mean"
main = left_join(main,proj2027.mean,by=c("eBird.English.Name.2022"="COMMON.NAME"))

proj2027.rci = trends %>% filter(timegroups == 2027) %>% select(COMMON.NAME,rci_comb_std)
names(proj2027.rci)[2] = "proj2027.rci"
main = left_join(main,proj2027.rci,by=c("eBird.English.Name.2022"="COMMON.NAME"))

# 2028

proj2028.lci = trends %>% filter(timegroups == 2028) %>% select(COMMON.NAME,lci_comb_std)
names(proj2028.lci)[2] = "proj2028.lci"
main = left_join(main,proj2028.lci,by=c("eBird.English.Name.2022"="COMMON.NAME"))

proj2028.mean = trends %>% filter(timegroups == 2028) %>% select(COMMON.NAME,mean_comb_std)
names(proj2028.mean)[2] = "proj2028.mean"
main = left_join(main,proj2028.mean,by=c("eBird.English.Name.2022"="COMMON.NAME"))

proj2028.rci = trends %>% filter(timegroups == 2028) %>% select(COMMON.NAME,rci_comb_std)
names(proj2028.rci)[2] = "proj2028.rci"
main = left_join(main,proj2028.rci,by=c("eBird.English.Name.2022"="COMMON.NAME"))

# 2029

proj2029.lci = trends %>% filter(timegroups == 2029) %>% select(COMMON.NAME,lci_comb_std)
names(proj2029.lci)[2] = "proj2029.lci"
main = left_join(main,proj2029.lci,by=c("eBird.English.Name.2022"="COMMON.NAME"))

proj2029.mean = trends %>% filter(timegroups == 2029) %>% select(COMMON.NAME,mean_comb_std)
names(proj2029.mean)[2] = "proj2029.mean"
main = left_join(main,proj2029.mean,by=c("eBird.English.Name.2022"="COMMON.NAME"))

proj2029.rci = trends %>% filter(timegroups == 2029) %>% select(COMMON.NAME,rci_comb_std)
names(proj2029.rci)[2] = "proj2029.rci"
main = left_join(main,proj2029.rci,by=c("eBird.English.Name.2022"="COMMON.NAME"))

write.csv(main,"trends_results/mask_one/SoIB_main_mask_oneland_wocats.csv",row.names=F)







## assign categories to trends and occupancy


main = read.csv("trends_results/mask_one/SoIB_main_wocats.csv")

main = main %>%
  mutate(SOIBv2.Long.Term.Status = 
           case_when(is.na(longtermmean) ~ "eBird Data Deficient",
                     (longtermrci-longtermmean)/longtermmean > 0.5 ~ "eBird Data Inconclusive",
                     longtermrci <= 50 ~ "Rapid Decline",
                     longtermrci <= 75 ~ "Decline",
                     longtermlci >= 150 ~ "Rapid Increase",
                     longtermlci >= 125 ~ "Increase",
                     longtermrci < 100 ~ "eBird Data Inconclusive",
                     longtermlci <= 50 ~ "eBird Data Inconclusive",
                     longtermlci > 100 ~ "eBird Data Inconclusive",
                     longtermrci >= 150 ~ "eBird Data Inconclusive",
                     TRUE ~ "Stable")
  ) %>%
  mutate(SOIBv2.Current.Status = 
           case_when(is.na(currentslopemean) ~ "eBird Data Deficient",
                     (currentsloperci-currentslopelci) > 6 ~ "eBird Data Inconclusive",
                     currentsloperci <= -2.7 ~ "Rapid Decline",
                     currentsloperci <= -1.1 ~ "Decline",
                     currentslopelci >= 1.6 ~ "Rapid Increase",
                     currentslopelci >= 0.9 ~ "Increase",
                     currentsloperci < 0 ~ "eBird Data Inconclusive",
                     currentslopelci > 0 ~ "eBird Data Inconclusive",
                     TRUE ~ "Stable")
  )

## sensitivity check

sens = read.csv("trends_results/mask_one/current_sensitivity.csv")

for (i in 1:8)
{
  sens$currentslopelci = sens[,2+(i-1)*3]
  sens$currentslopemean = sens[,3+(i-1)*3]
  sens$currentsloperci = sens[,4+(i-1)*3]
  
  sensx = sens %>%
    mutate(SOIBv2.Current.Status.Sens = 
             case_when(is.na(currentslopemean) ~ "eBird Data Deficient",
                       (currentsloperci-currentslopelci) > 6 ~ "eBird Data Inconclusive",
                       currentsloperci <= -2.7 ~ "Rapid Decline",
                       currentsloperci <= -1.1 ~ "Decline",
                       currentslopelci >= 1.6 ~ "Rapid Increase",
                       currentslopelci >= 0.9 ~ "Increase",
                       currentsloperci < 0 ~ "eBird Data Inconclusive",
                       currentslopelci > 0 ~ "eBird Data Inconclusive",
                       TRUE ~ "Stable")
    ) %>%
    select(eBird.English.Name.2022,SOIBv2.Current.Status.Sens)
  
  if (i == 1)
  {
    sensy = left_join(main,sensx)
    sensy = sensy %>% select(eBird.English.Name.2022,SOIBv2.Current.Status,SOIBv2.Current.Status.Sens)
    names(sensy)[i+2] = paste("s",i,sep='')
  }
  
  if (i > 1)
  {
    sensy = left_join(sensy,sensx)
    names(sensy)[i+2] = paste("s",i,sep='')
  }
}

sensy = sensy %>%
  filter(!SOIBv2.Current.Status %in% c("eBird Data Deficient","eBird Data Inconclusive"))

ind1 = numeric(0)
ind2 = numeric(0)
ind3 = numeric(0)
ind4 = numeric(0)
ind5 = numeric(0)
ind6 = numeric(0)
ind7 = numeric(0)

for (i in 1:length(sensy$eBird.English.Name.2022))
{
  cts = as.vector(sensy[i,-1])
  if (length(unique(cts)) == 1)
    ind1 = c(ind1,i)
  
  if (length(unique(cts)) == 2 & "eBird Data Inconclusive" %in% cts)
    ind2 = c(ind2,i)
  
  if (("Decline" %in% cts | "Rapid Decline" %in% cts) & 
      ("Stable" %in% cts | "Increase" %in% cts | "Rapid Increase" %in% cts))
    ind3 = c(ind3,i)
  
  if (("Increase" %in% cts | "Rapid Increase" %in% cts) & 
      ("Stable" %in% cts | "Decline" %in% cts | "Rapid Decline" %in% cts))
    ind4 = c(ind4,i)
  
  if (length(cts[cts == "eBird Data Inconclusive"]) >= 4)
    ind5 = c(ind5,i)
  
  if (cts[1] == "Rapid Decline" & "Decline" %in% cts)
    ind6 = c(ind6,i)
  
  if (cts[1] == "Rapid Increase" & "Increase" %in% cts)
    ind7 = c(ind7,i)
}

ind.rem = union(ind3,ind4)
ind.rem = union(ind.rem,ind5)

ind6 = setdiff(ind6,ind.rem)
ind7 = setdiff(ind7,ind.rem)


main$SOIBv2.Current.Status[main$eBird.English.Name.2022 %in% sensy$eBird.English.Name.2022[ind.rem]] = "eBird Data Inconclusive"
main$SOIBv2.Current.Status[main$eBird.English.Name.2022 %in% sensy$eBird.English.Name.2022[ind6]] = "Decline"
main$SOIBv2.Current.Status[main$eBird.English.Name.2022 %in% sensy$eBird.English.Name.2022[ind7]] = "Increase"

main$SOIBv2.Long.Term.Status[main$Selected.SOIB != "X"] = NA
main$SOIBv2.Current.Status[main$Selected.SOIB != "X"] = NA
main$SOIB.Range.Status[main$Selected.SOIB != "X"] = NA



trendcats = c("Rapid Decline","Decline","eBird Data Deficient","eBird Data Inconclusive",
              "Stable","Increase","Rapid Increase")
rangecats = c("eBird Data Deficient","Very Restricted","Restricted","Moderate",
              "Large","Very Large")


priorityrules = read.csv("priorityclassificationrules.csv")
main = left_join(main,priorityrules)


unce = c("eBird Data Deficient","eBird Data Inconclusive")
rest = c("Very Restricted","Restricted")
decl = c("Decline","Rapid Decline")

main = main %>%
  mutate(SOIBv2.Priority.Status = as.character(SOIBv2.Priority.Status)) %>%
  mutate(SOIBv2.Priority.Status = 
           case_when(SOIBv2.Long.Term.Status %in% unce & SOIBv2.Current.Status %in% unce &
                       IUCN.Category %in% c("Endangered","Critically Endangered") ~ "High",
                     SOIBv2.Long.Term.Status %in% decl & SOIBv2.Current.Status %in% decl &
                       IUCN.Category %in% c("Endangered","Critically Endangered") ~ "High",
                     SOIBv2.Long.Term.Status %in% unce & SOIBv2.Current.Status %in% unce &
                       SOIB.Range.Status %in% rest &
                       IUCN.Category %in% c("Vulnerable") ~ "High",
                     SOIBv2.Long.Term.Status %in% unce & SOIBv2.Current.Status %in% unce &
                       IUCN.Category %in% c("Near Threatened","Vulnerable") & SOIBv2.Priority.Status == "Low" ~ "Moderate",
                     TRUE ~ SOIBv2.Priority.Status))

main = main %>%
  mutate(longtermlci = longtermlci-100,
         longtermmean = longtermmean-100,
         longtermrci = longtermrci-100)

write.csv(main,"trends_results/mask_one/SoIB_main_mask_oneland.csv",row.names=F)

## small investigations

summary_status = data.frame(Category = trendcats)
a = data.frame(table(main$SOIBv2.Long.Term.Status))
names(a) = c("Category","Long.Term")
b = data.frame(table(main$SOIBv2.Current.Status))
names(b) = c("Category","Current")
summary_status = left_join(summary_status,a)
summary_status = left_join(summary_status,b)

priority_summary = data.frame(table(main$SOIBv2.Priority.Status))
names(priority_summary) = c("Category","N.species")

species_summary = data.frame(Category = c("Selected for SoIB","Long-term Analysis","Current Analysis"),
                             N.species = c(as.vector(table(main$Selected.SOIB))[2],
                                           as.vector(table(main$Long.Term.Analysis))[2],
                                           as.vector(table(main$Current.Analysis))[2]))

write.csv(summary_status,"trends_results/mask_one/summary_status_mask_oneland.csv",row.names=F)
write.csv(priority_summary,"trends_results/mask_one/priority_status_mask_oneland.csv",row.names=F)
write.csv(species_summary,"trends_results/mask_one/species_status_mask_oneland.csv",row.names=F)