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
  
  pred_funSE <- function(input_model) {
    predict(input_model, newdata = ltemp, re.form = NA, 
            allow.new.levels = TRUE)
    # not specifying type = "response" because will later transform prediction along with SE
  }
  
  pred_fun <- function(input_model) {
    predict(input_model, newdata = ltemp, re.form = NA, 
            allow.new.levels = TRUE, type = "response")
  }
  
  # tictoc::tic("bootMer 1000 sims")
  par_cores = 12
  pred_bootMerSE = bootMer(m, 
                           nsim = 100, # for faster compute, estimate doesn't change much with high sims
                           FUN = pred_funSE, 
                           seed = 1000, use.u = FALSE, type = "parametric", 
                           parallel = "yes", ncpus = par_cores)
  
  pred_bootMer = bootMer(m, 
                         nsim = 100, # for faster compute, estimate doesn't change much with high sims
                         FUN = pred_fun, 
                         seed = 1000, use.u = FALSE, type = "parametric", 
                         parallel = "yes", ncpus = par_cores)
  
  
  ltemp_predSE = ltemp %>%
    slice(rep(1:n(), times = nrow(pred_bootMerSE$t))) %>%
    mutate(
      sim = rep(1:nrow(pred_bootMerSE$t), each = nrow(ltemp)),
      predSE = c(t(pred_bootMerSE$t))
    )  
  
  ltemp_pred = ltemp %>%
    slice(rep(1:n(), times = nrow(pred_bootMer$t))) %>%
    mutate(
      sim = rep(1:nrow(pred_bootMer$t), each = nrow(ltemp)),
      pred = c(t(pred_bootMer$t))
    )
  
  ltemp_pred_comb = ltemp_predSE %>%
    left_join(ltemp_pred) %>%
    group_by(sim,timegroups) %>%
    reframe(predSE = mean(predSE),
            pred = mean(pred)) %>% 
    right_join(tm) %>% 
    left_join(databins %>% distinct(timegroups, year)) %>%
    rename(timegroupsf = timegroups,
           timegroups = year) %>% 
    mutate(timegroupsf = factor(timegroupsf, 
                                levels = soib_year_info("timegroup_lab", "FALSE"))) %>%
    complete(timegroupsf) %>% 
    arrange(sim,timegroupsf)
  
  
  ratios = ltemp_pred_comb %>%
    filter(timegroupsf == soib_year_info("timegroup_lab")[1]) %>%
    dplyr::select(-predSE,-timegroupsf,-timegroups) %>%
    rename(first = pred)
  
  f1_rats = ltemp_pred_comb %>%
    left_join(ratios) %>%
    mutate(tp0 = pred/first) %>% 
    group_by(timegroups) %>% 
    reframe(lci_std = 100*as.numeric(quantile(tp0, 0.025)),
            mean_std = 100*median(tp0),
            rci_std = 100*as.numeric(quantile(tp0, 0.975)))
  
  
  f1_freqs = ltemp_pred_comb %>%
    mutate(COMMON.NAME = species) %>%
    group_by(COMMON.NAME,timegroups,timegroupsf) %>%
    reframe(mean_trans = mean(predSE), 
            se_trans = sd(predSE),
            lci = quantile(pred, 0.025), 
            mean = mean(pred), 
            rci = quantile(pred, 0.975)) %>%
    left_join(f1_rats) %>%
    suppressMessages()
  
  f1_freqs = f1_freqs %>%
    mutate(lci_alt = clogloglink(mean_trans - 1.96*se_trans, inverse = T),
           mean_alt = clogloglink(mean_trans, inverse = T),
           rci_alt = clogloglink(mean_trans + 1.96*se_trans, inverse = T))
  
  # tictoc::toc()
  
  # Years to project for IUCN comparison
  extra.years = soib_year_info("iucn_projection")
  
  
  modtrends = na.omit(f1_freqs) %>% # NAs are all spp. not included in long-term
    # _trans are link-scale, "mean" is back-transformed
    mutate(m1 = first(mean_trans),
           mean_year1 = first(mean_alt),
           s1 = first(se_trans)) %>% 
    ungroup()
  
  # "main" simulated CIs
  
  set.seed(10) 
  modtrends_alt = modtrends %>% 
    # calculating CIs
    group_by(timegroups) %>% 
    # 1000 simulations of transformed ratio of present:original values
    # quantiles*100 from these gives us our CI limits for mean_std
    reframe(tp0 = simerrordiv(mean_trans, m1, se_trans, s1)$rat) %>% 
    group_by(timegroups) %>% 
    reframe(lci_std_alt = 100*as.numeric(quantile(tp0, 0.025)),
            mean_std_alt = 100*as.numeric(median(tp0)),
            rci_std_alt = 100*as.numeric(quantile(tp0, 0.975)))
  
  f1 = f1_freqs %>%
    left_join(modtrends_alt)
  
  if (c == 1)
  {
    temp = f1
  }
  
  if (c > 1)
  {
    temp = temp %>%
      rbind(f1)
  }
  
  write.csv(temp,"new_method_results.csv",row.names=F)
}