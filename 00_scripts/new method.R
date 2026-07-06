library(tidyverse)
library(lme4)
library(VGAM)

source('00_scripts/00_functions.R')

load("01_analyses_full/specieslists.RData")
load("01_analyses_full/dataforanalyses.RData")

data = data %>%
  # converting months to seasons
  mutate(season = as.numeric(month)) %>% 
  mutate(season = case_when(season %in% c(12,1,2) ~ "Win",
                           season %in% c(3,4,5) ~ "Sum",
                           season %in% c(6,7,8) ~ "Mon",
                           season %in% c(9,10,11) ~ "Aut")) %>% 
  mutate(season = as.factor(season))

## species-wise

speclist_ht = specieslist$COMMON.NAME[!is.na(specieslist$ht)]

c = 0
for (species in speclist_ht)
{
  c = c + 1
  print(c)
  datas = data
  
  datas = datas %>%
    filter(COMMON.NAME == species) %>%
    distinct(gridg3, month) %>% 
    left_join(datas) %>%
    suppressMessages()
  
  tm = datas %>% distinct(timegroups)
  
  data_freq = datas %>%
    filter(COMMON.NAME == species, ALL.SPECIES.REPORTED == 1) %>%
    group_by(timegroups,season,gridg0) %>%
    reframe(s_lists = n_distinct(group.id))
  
  data_samp = datas %>%
    filter(ALL.SPECIES.REPORTED == 1) %>%
    group_by(timegroups,season,gridg0,gridg1,gridg3) %>%
    reframe(n_lists = n_distinct(group.id),
            no.sp = mean(no.sp))
  
  datay = data_samp %>%
    group_by(gridg3, gridg1) %>% 
    reframe(medianlla = median(no.sp)) %>%
    group_by(gridg3) %>% 
    reframe(medianlla = mean(medianlla)) %>%
    reframe(medianlla = round(mean(medianlla)))
  medianlla = datay$medianlla
  gg1 = data_samp$gridg1[1]
  gg3 = data_samp$gridg3[1]
  
  # dataframe to predict
  ltemp = data_samp %>%
    group_by(., season) %>% 
    reframe(., timegroups = unique(tm$timegroups)) %>%
    mutate(no.sp = medianlla,
           gridg1 = gg1, 
           gridg3 = gg3)
  
  data_tot = data_samp %>% left_join(data_freq)  %>%
    mutate(s_lists = case_when(is.na(s_lists) ~ 0,
                               TRUE ~ as.numeric(s_lists))) %>%
    mutate(freq = s_lists/n_lists)
  
  m = glmer(freq ~ timegroups + season + season:log(no.sp) + (1|gridg3/gridg1), 
            data = data_tot, family = binomial(link = 'cloglog'),
            weights = n_lists,
            nAGQ = 0, control = glmerControl(optimizer = "bobyqa"))  
  
  # bootMer
  
  pred_fun <- function(input_model) {
    predict(input_model, newdata = ltemp, re.form = NA, allow.new.levels = TRUE)
    # not specifying type = "response" because will later transform prediction along with SE
  }
  
  # tictoc::tic("bootMer 1000 sims")
  par_cores = 12
  pred_bootMer = bootMer(m, 
                         nsim = 100, # for faster compute, estimate doesn't change much with high sims
                         FUN = pred_fun, 
                         seed = 1000, use.u = FALSE, type = "parametric", 
                         parallel = "yes", ncpus = par_cores)
  
  f2 = ltemp %>% 
    dplyr::select(., timegroups) %>%
    mutate(freq = 0, se = 0)  # this is not actually needed
  
  f2$freqt = colMeans(pred_bootMer$t)
  f2$set = apply(pred_bootMer$t,2,sd)
  # tictoc::toc()
  
  f1 = f2 %>%
    filter(!is.na(freqt) & !is.na(set)) %>%
    # average across season
    group_by(., timegroups) %>% 
    reframe(mean_trans = mean(freqt), se_trans = mean(set)) %>% 
    right_join(tm) %>% 
    left_join(databins %>% distinct(timegroups, year)) %>% 
    rename(timegroupsf = timegroups,
           timegroups = year) %>% 
    mutate(timegroupsf = factor(timegroupsf, 
                                levels = soib_year_info("timegroup_lab", "FALSE"))) %>%
    complete(timegroupsf) %>% 
    arrange(timegroupsf) %>%
    suppressMessages()
  
  # Years to project for IUCN comparison
  extra.years = soib_year_info("iucn_projection")
  
  f1 = f1 %>%
    mutate(COMMON.NAME = species) %>%
    mutate(lci = clogloglink(mean_trans - 1.96*se_trans, inverse = T),
           mean = clogloglink(mean_trans, inverse = T),
           rci = clogloglink(mean_trans + 1.96*se_trans, inverse = T)) %>%
    ungroup()
  
  modtrends = na.omit(f1) %>% # NAs are all spp. not included in long-term
    # _trans are link-scale, "mean" is back-transformed
    mutate(m1 = first(mean_trans),
           mean_year1 = first(mean),
           s1 = first(se_trans)) %>% 
    ungroup() %>% 
    # for calculating change in abundance index (as % change)
    mutate(mean_std = 100*mean/mean_year1) # back-transformed so value is % of year1 value
  
  # "main" simulated CIs
  set.seed(10) 
  modtrends = modtrends %>% 
    # calculating CIs
    group_by(timegroups) %>% 
    # 1000 simulations of transformed ratio of present:original values
    # quantiles*100 from these gives us our CI limits for mean_std
    reframe(tp0 = simerrordiv(mean_trans, m1, se_trans, s1)$rat) %>% 
    group_by(timegroups) %>% 
    reframe(lci_std = 100*as.numeric(quantile(tp0, 0.025)),
            rci_std = 100*as.numeric(quantile(tp0, 0.975))) %>% 
    right_join(modtrends, by = c("timegroups"))
  
  if (c == 1)
  {
    temp = modtrends
  }
  
  if (c > 1)
  {
    temp = temp %>%
      rbind(modtrends)
  }
  
  write.csv(temp,"new_method_results.csv",row.names=F)
}

