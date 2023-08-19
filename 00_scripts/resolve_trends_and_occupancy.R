# preparing data for specific mask (this is the only part that changes, but automatically)
cur_metadata <- analyses_metadata %>% filter(MASK == cur_mask)

# read paths
base_path <- cur_metadata$FULLSPECLIST.PATH
speclist_path <- cur_metadata$SPECLISTDATA.PATH

trends_pathonly <- cur_metadata$TRENDS.PATHONLY
occu_pres_pathonly <- cur_metadata$OCCU.PRES.PATHONLY
occu_mod_pathonly <- cur_metadata$OCCU.MOD.PATHONLY

# write paths
cursens_path <- cur_metadata$CURSENS.PATH 
trends_outpath <- cur_metadata$TRENDS.OUTPATH
occu_outpath <- cur_metadata$OCCU.OUTPATH

mainwocats_path <- cur_metadata$SOIBMAIN.WOCATS.PATH
main_path <- cur_metadata$SOIBMAIN.PATH
summaries_path <- cur_metadata$SUMMARY.PATH

###

library(tidyverse)
library(VGAM)
library(sf)
library(writexl)

source('00_scripts/00_functions.R')

recentcutoff = 2015

load("00_data/spec_misid.RData") # to remove from LTT and CAT "selection" later
# for occupancy
load("00_data/maps_sf.RData")
load(speclist_path)
# for classification
priorityrules = read.csv("00_data/priorityclassificationrules.csv") 
load("00_data/vagrantdata.RData")


# if habitat/conservation area mask, we skip resolve_occu completely (take from full-country)
if (!cur_metadata$MASK.TYPE %in% c("country", "state")) {
  
  skip_res_occu <- TRUE 
  
} else {
  
  skip_res_occu <- FALSE 
  
  if (cur_metadata$MASK.TYPE == "state") {
    
    # if state, we have own occu-presence but we take full country occu-model
    # latter needs to be filtered for grid cells of interest
    
    load("00_data/grids_st_sf.RData")
    
    cur_grid_filt <- g1_st_sf %>% 
      filter(STATE.NAME == cur_mask) %>% 
      transmute(gridg1 = GRID.G1)
    
  }
  
}

###

# don't run resolve_species_trends if no species selected
to_run <- (1 %in% specieslist$ht) | (1 %in% specieslist$rt) |
  (1 %in% restrictedspecieslist$ht) | (1 %in% restrictedspecieslist$rt)

if (to_run == FALSE) {
  
  # list of columns that need to be created since we have skipped steps
  na_columns <- c("longtermlci", "longtermmean", "longtermrci",     
                  "currentslopelci", "currentslopemean", "currentsloperci", 
                  "proj2023.lci", "proj2023.mean", "proj2023.rci",    
                  "proj2024.lci", "proj2024.mean", "proj2024.rci",    
                  "proj2025.lci", "proj2025.mean", "proj2025.rci",    
                  "proj2026.lci", "proj2026.mean", "proj2026.rci",    
                  "proj2027.lci", "proj2027.mean", "proj2027.rci",    
                  "proj2028.lci", "proj2028.mean", "proj2028.rci",    
                  "proj2029.lci", "proj2029.mean", "proj2029.rci",    
                  "rangelci", "rangemean", "rangerci")
  
  base = read.csv(base_path) %>% dplyr::select(-SCIENTIFIC.NAME)
  
  main = read.csv("00_data/SoIB_mapping_2022.csv") %>% 
    left_join(base, by = c("eBird.English.Name.2022" = "COMMON.NAME"))
  
  # creating NA columns to match structure of "normal" main data
  main[, na_columns] <- NA_real_
  
  
  print(glue("Skipping resolving species trends for {cur_mask}"))
  
} else {
  
  # data processing and prep ------------------------------------------------
  
  base = read.csv(base_path) %>% dplyr::select(-SCIENTIFIC.NAME)
  
  main = read.csv("00_data/SoIB_mapping_2022.csv") %>% 
    left_join(base, by = c("eBird.English.Name.2022" = "COMMON.NAME"))
  
  # separate object for sensitivity analysis
  sens = main %>% dplyr::select(eBird.English.Name.2022)
  
  
  # trends files
  trends <- list.files(path = trends_pathonly, 
                       # Generate the full file paths
                       full.names = T) %>% 
    # Read each CSV file and combine them into a single data frame
    map_df(read.csv) 
  
  totsims = n_distinct(trends$sl)
  
  
  # data filtering: problem species -----------------------------------------
  
  # removing problem species from current and long-term analyses
  # (model output contains both species types together)
  
  spec_lt <- main %>% 
    filter(Long.Term.Analysis == "X") %>% 
    pull(eBird.English.Name.2022)
  spec_ct <- main %>% 
    filter(Current.Analysis == "X") %>% 
    pull(eBird.English.Name.2022)
  
  # long-term (problematic species) ###
  
  trendsa = trends %>%
    filter(timegroups < 2015,
           COMMON.NAME %in% spec_lt) %>%
    group_by(sl, COMMON.NAME) %>%  
    # is any simulation of any species problematic: unable to calc. SE or SE > |mean|
    filter(any(is.na(se) | se > abs(freq))) %>%
    distinct(sl, COMMON.NAME)
  
  # for a species, if problematic sims are more than half of total sims, ignore species completely.
  # if less than half of total sims, we ignore those particular sims so that those values don't affect 
  # overall estimate.
  if (length(trendsa$sl) > 0)
  {
    tab_lt_rem = table(trendsa$COMMON.NAME) %>% 
      as.data.frame() %>% 
      magrittr::set_colnames(c("COMMON.NAME","count")) %>% 
      mutate(COMMON.NAME = as.character(COMMON.NAME))
    
    specs_lt_remove <- tab_lt_rem %>% 
      filter(count >= round(totsims/2)) %>% 
      pull(COMMON.NAME)
    
    specs_lt_remove_part = tab_lt_rem %>% 
      filter(count < round(totsims/2)) %>% 
      pull(COMMON.NAME)
    
    
    trends = trends %>%
      mutate(freq = case_when(timegroups < 2015 & 
                                COMMON.NAME %in% specs_lt_remove ~ NA,
                              TRUE ~ freq),
             se = case_when(timegroups < 2015 & 
                              COMMON.NAME %in% specs_lt_remove ~ NA,
                            TRUE ~ se))
    main <- main %>% 
      mutate(Long.Term.Analysis = if_else(eBird.English.Name.2022 %in% specs_lt_remove,
                                          "",
                                          Long.Term.Analysis))
    
    # when species problematic in less than half of total sims, ignoring those sims
    trendsa = trendsa %>%
      filter(COMMON.NAME %in% specs_lt_remove_part) %>%
      distinct(sl, COMMON.NAME) %>%
      mutate(comb = paste(sl, COMMON.NAME))
    
    trends = trends %>%
      mutate(comb = paste(sl, COMMON.NAME)) %>%
      filter(!comb %in% trendsa$comb) %>%
      dplyr::select(-comb)
    
  }
  
  
  # current (problematic species) ###
  
  trendsb = trends %>%
    filter(timegroups >= 2015,
           COMMON.NAME %in% spec_ct) %>%
    group_by(sl, COMMON.NAME) %>%  
    # is any simulation of any species problematic: unable to calc. SE or SE > |mean|
    filter(any(is.na(se) | se > abs(freq))) %>%
    distinct(sl, COMMON.NAME)
  
  
  if (length(trendsb$sl) > 0)
  {
    tab_ct_rem = table(trendsb$COMMON.NAME) %>% 
      as.data.frame() %>% 
      magrittr::set_colnames(c("COMMON.NAME","count")) %>% 
      mutate(COMMON.NAME = as.character(COMMON.NAME))
    
    specs_ct_remove <- tab_ct_rem %>% 
      filter(count >= round(totsims/2)) %>% 
      pull(COMMON.NAME)
    
    specs_ct_remove_part = tab_ct_rem %>% 
      filter(count < round(totsims/2)) %>% 
      pull(COMMON.NAME)
    
    
    trends = trends %>%
      filter(!COMMON.NAME %in% specs_ct_remove)
    
    main <- main %>% 
      mutate(Long.Term.Analysis = if_else(eBird.English.Name.2022 %in% specs_ct_remove,
                                          "", Long.Term.Analysis),
             Current.Analysis = if_else(eBird.English.Name.2022 %in% specs_ct_remove,
                                        "", Current.Analysis))
    
    
    trendsb = trendsb %>%
      filter(COMMON.NAME %in% specs_ct_remove_part) %>%
      distinct(sl, COMMON.NAME) %>%
      mutate(comb = paste(sl, COMMON.NAME))
    
    trends = trends %>%
      mutate(comb = paste(sl, COMMON.NAME)) %>%
      filter(!comb %in% trendsb$comb) %>%
      dplyr::select(-comb)
    
  }
  
  
  # data filtering: extra metrics -------------------------------------------
  
  # remove species based on 2 extra metrics (only 1 for PAs and states)
  
  
  # 1. number of sampled 5kmx5km cells within (not for PAs or states)
  
  if (cur_mask %in% c("none", "woodland", "cropland", "ONEland")) {
    
    specsc1 = main %>%
      # <annotation_pending_AV> rationale for 0.8 (even if arbitrary, what does it mean?)
      filter(!is.na(mean5km) & mean5km < 8 &
               (Long.Term.Analysis == "X" | Current.Analysis == "X") &
               is.na(Restricted.Islands)) %>% 
      pull(eBird.English.Name.2022)

    specsc2 = main %>%
      # <annotation_pending_AV> rationale for 0.25 (even if arbitrary, what does it mean?)
      filter(!is.na(ci5km) & (main$ci5km/main$mean5km) > 0.25 &
               (Long.Term.Analysis == "X" | Current.Analysis == "X") &
               is.na(Restricted.Islands)) %>% 
      pull(eBird.English.Name.2022)
    
    specsc = union(specsc1, specsc2)
    
    
    trends = trends %>%
      filter(!COMMON.NAME %in% specsc)
    
    main <- main %>% 
      mutate(Long.Term.Analysis = if_else(eBird.English.Name.2022 %in% specsc,
                                          "", Long.Term.Analysis),
             Current.Analysis = if_else(eBird.English.Name.2022 %in% specsc,
                                        "", Current.Analysis))
    
  }
  
  
  # 2. proportion of 25kmx25km cells sampled
  
  # <annotation_pending_AV> this isn't used anywhere, can be removed?
  specsd3 = main %>%
    # <annotation_pending_AV> rationale for 0.6 (even if arbitrary, what does it mean?)
    filter(!is.na(proprange25km.current) & 
             (proprange25km.current/proprange25km2022) < 0.6 &
             (Current.Analysis == "X")) %>% 
    pull(eBird.English.Name.2022)
  
  # <annotation_pending_AV> should these lines be referencing specsd3?
  trends = trends %>%
    filter(!COMMON.NAME %in% specsd3)
  
  
  main <- main %>%
    # remove these species from both analyses, since unlikely that it qualifies for long-term
    # but not for current
    mutate(Long.Term.Analysis = if_else(eBird.English.Name.2022 %in% specsd3,
                                        "", Long.Term.Analysis),
           Current.Analysis = if_else(eBird.English.Name.2022 %in% specsd3,
                                      "", Current.Analysis))
  
  
  # rewriting selected species for LTT and CAT
  spec_lt = main %>% 
    filter(Long.Term.Analysis == "X") %>% 
    pull(eBird.English.Name.2022)
  
  spec_ct = main %>% 
    filter(Current.Analysis == "X") %>% 
    pull(eBird.English.Name.2022)
  
  
  # checkpoint-object "main"
  main1_postfilt <- main
  
  
  # calculations: prep --------------------------------------------------------
  
  # This section performs calculations on the trends data frame to derive new columns, 
  # such as lci, mean, and rci. It also creates a data frame called trends_framework to 
  # define the timegroups and species combinations. (Summary from ChatGPT)
  
  # Mean is calculated first, then CI. This is done for long-term and current 
  # trend separately.
  
  
  # Years to project for PJ's IUCN comparison
  extra.years = 2023:2029
  
  trends = trends %>%
    group_by(COMMON.NAME, timegroupsf, timegroups) %>% 
    reframe(mean_trans = mean(freq), 
            # <annotation_pending_AV> SE calculation worth a small comment?
            se_trans = sd(freq) + sqrt(sum(se^2))/n()) %>%
    group_by(COMMON.NAME, timegroupsf, timegroups) %>% 
    mutate(lci = clogloglink(mean_trans - 1.96*se_trans, inverse = T),
           mean = clogloglink(mean_trans, inverse = T),
           rci = clogloglink(mean_trans + 1.96*se_trans, inverse = T)) %>%
    ungroup()
  
  # adding extra years to dataframe (and completing species combinations)
  trends_framework <- trends %>%
    distinct(timegroups, COMMON.NAME) %>%
    tidyr::expand(timegroups = c(unique(timegroups), extra.years),
                  COMMON.NAME) %>%
    complete(timegroups, COMMON.NAME) %>% 
    left_join(trends %>% distinct(COMMON.NAME, timegroupsf, timegroups)) %>% 
    mutate(timegroupsf = ifelse(is.na(timegroupsf), timegroups, timegroupsf))
  
  
  # calculations: trends (long-term) ----------------------------------------
  
  modtrends = na.omit(trends) %>% # NAs are all spp. not included in long-term
    filter(COMMON.NAME %in% spec_lt) %>%
    arrange(COMMON.NAME, timegroups) %>%
    # getting trends values of first year (only for long-term, i.e., pre-2000)
    group_by(COMMON.NAME) %>% 
    # _trans are link-scale, "mean" is back-transformed
    mutate(m1 = first(mean_trans),
           mean_year1 = first(mean),
           s1 = first(se_trans)) %>% 
    ungroup() %>% 
    # for calculating change in abundance index (as % change)
    mutate(mean_std = 100*mean/mean_year1) # back-transformed so value is % of year1 value
  
  
  # sensitivity check to ensure edge species are later converted to the conservative status
  modtrends1 <- ltt_sens_sim(my_seed = 1)
  modtrends2 <- ltt_sens_sim(my_seed = 2)
  modtrends3 <- ltt_sens_sim(my_seed = 3)
  modtrends4 <- ltt_sens_sim(my_seed = 4)
  modtrends5 <- ltt_sens_sim(my_seed = 5)
  
  # "main" simulated CIs
  set.seed(10) 
  modtrends = modtrends %>% 
    # calculating CIs
    group_by(COMMON.NAME, timegroups) %>% 
    # 1000 simulations of transformed ratio of present:original values
    # quantiles*100 from these gives us our CI limits for mean_std
    reframe(tp0 = simerrordiv(mean_trans, m1, se_trans, s1)$rat) %>% 
    group_by(COMMON.NAME, timegroups) %>% 
    reframe(lci_std = 100*as.numeric(quantile(tp0, 0.025)),
            rci_std = 100*as.numeric(quantile(tp0, 0.975))) %>% 
    right_join(modtrends, by = c("COMMON.NAME", "timegroups"))
  
  # saving the values for 2022 in "main" as well:
  # temp object then left_join instead of right_join because species order in main
  # needs to be preserved
  temp <- modtrends %>%
    filter(timegroups == 2022) %>%
    dplyr::select(COMMON.NAME, lci_std, mean_std, rci_std) %>%
    rename(longtermlci = lci_std,
           longtermmean = mean_std,
           longtermrci = rci_std)
  
  main <- main %>%
    left_join(temp, by = c("eBird.English.Name.2022" = "COMMON.NAME"))
  
  
  modtrends = modtrends %>%
    group_by(COMMON.NAME) %>%
    # making CI band zero for first year
    mutate(lci_std = case_when(timegroups == first(timegroups) ~ mean_std,
                               TRUE ~ lci_std),
           rci_std = case_when(timegroups == first(timegroups) ~ mean_std,
                               TRUE ~ rci_std)) %>%
    ungroup() %>%
    dplyr::select(timegroupsf, timegroups, COMMON.NAME, lci_std, mean_std, rci_std)
  
  
  # checkpoint-object "main"
  main2_postLTT <- main
  
  
  # calculations: trends (current) ------------------------------------------
  
  # Unlike trends for long-term, this is not last year divided by first year, instead a slope.
  # For each year, sim 1000 points within CI for each year. In each sim, line is fitted.
  # In each case, three models fitted (main, exponential for projection, sensitivity).
  # Exponential is for Praveen's IUCN estimation using annual change, not used here.
  
  tic("Calculating current trends")
  
  # First, getting mean + CI for each year
  modtrends_recent = trends %>% 
    filter(COMMON.NAME %in% spec_ct &
             timegroups >= recentcutoff) %>%
    arrange(COMMON.NAME, timegroups) %>%
    # getting trends values of first year (only for current)
    group_by(COMMON.NAME) %>% 
    mutate(m1 = first(mean_trans),
           mean_year1 = first(mean),
           s1 = first(se_trans)) %>% 
    ungroup() %>% 
    # for calculating change in abundance index (as % change)
    mutate(mean_std_recent = 100*mean/mean_year1)
  
  # Getting CIs for each year
  set.seed(10)
  modtrends_recent = modtrends_recent %>% 
    # calculating CIs
    group_by(COMMON.NAME, timegroups) %>% 
    # 1000 simulations of transformed ratio of present:original values
    # quantiles*100 from these gives us our CI limits for mean_std
    reframe(simerrordiv(mean_trans, m1, se_trans, s1)) %>%  # gives rat and val columns
    group_by(COMMON.NAME, timegroups) %>% 
    reframe(lci_std_recent = 100*as.numeric(quantile(rat, 0.025)),
            rci_std_recent = 100*as.numeric(quantile(rat, 0.975)),
            rat = rat,
            val = val) %>% 
    right_join(modtrends_recent, by = c("COMMON.NAME", "timegroups"))
  
  
  # Now running simulations for:
  #   - slope and SE of main trend
  #   - 8x slopes and SEs for sensitivity analyses
  #   - exponential model predictions into future years
  
  set.seed(1)
  temp_sims <- modtrends_recent %>%
    group_by(COMMON.NAME) %>%
    dplyr::select(timegroups, rat, val) %>%
    group_by(COMMON.NAME, timegroups) %>%
    mutate(sim = 1:n(),
           # for each sim, sampling from 1000 val values within species-year group
           val_sample = map_dbl(sim, ~ sample(val, 1)))
  
  
  # IN NEXT SECTION:
  # Take these simulated values of reporting frequency for each year,
  # fit lm through them, then predict new values based on that lm.
  
  # Numerator and denominator for slope calculation, along with error propagation
  # Uncertainty between 2015 and 2016 is highest, hence indexing them, i.e.,
  # using that uncertainty for entire slope (being cautious).
  
  # errordiv calculates and returns slope and SE from predicted values.
  
  
  # making CI band zero for first year
  modtrends_recent = modtrends_recent %>%
    # now we don't need rat and val
    distinct(COMMON.NAME, timegroupsf, timegroups, 
             lci_std_recent, mean_std_recent, rci_std_recent) %>% 
    group_by(COMMON.NAME) %>% 
    mutate(lci_std_recent = case_when(timegroups == first(timegroups) ~ mean_std_recent,
                                      TRUE ~ lci_std_recent)) %>%
    mutate(rci_std_recent = case_when(timegroups == first(timegroups) ~ mean_std_recent,
                                      TRUE ~ rci_std_recent)) %>%
    ungroup() %>%
    dplyr::select(timegroupsf, timegroups, COMMON.NAME,
                  lci_std_recent, mean_std_recent, rci_std_recent)
  
  
  # calculations: trends (current): MAIN ------------------------------------
  
  sl_data_main <- temp_sims %>% 
    group_by(COMMON.NAME, sim) %>% 
    # group_modify() performs function per grouping, so sl and slse calculated for all sim
    group_modify(~ {
      
      datatopred <- .x %>% dplyr::select(timegroups)
      
      modelfit <- lm(val_sample ~ timegroups, data = .x)
      pred <- predict(modelfit, newdata = datatopred, se = TRUE)
      
      num <- pred$fit[2] - pred$fit[1]
      den <- abs(pred$fit[1])
      numse <- sqrt(pred$se.fit[1]^2 + pred$se.fit[2]^2)
      dense <- pred$se.fit[1]
      
      # indexing for mean and se
      sl <- 100 * errordiv(num, den, numse, dense)[1] %>% as.numeric()
      slse <- errordiv(num, den, numse, dense)[2] %>% as.numeric()
      
      .x %>%
        reframe(sl = sl, 
                slse = slse)
      
    }) %>%
    group_by(COMMON.NAME) %>%
    reframe(mean.slope = mean(sl),
            se.slope = sd(sl) + sqrt(sum(slse^2)/length(slse))) %>%
    group_by(COMMON.NAME) %>%
    reframe(currentslopelci = mean.slope - 1.96*se.slope,
            currentslopemean = mean.slope,
            currentsloperci = mean.slope + 1.96*se.slope)
  
  # joining to main data
  main <- main %>%
    left_join(sl_data_main, by = c("eBird.English.Name.2022" = "COMMON.NAME"))
  
  
  # checkpoint-object "main"
  main3_postCATmain <- main
  
  # calculations: trends (current): SENS ------------------------------------
  
  # here, fitting the same linear model as for main trend, but for data in which
  # one year is dropped each time.
  # this is to see how much each year affects the estimate.
  sl_data_sens <- temp_sims %>%
    group_by(COMMON.NAME, sim) %>%
    group_modify(~ {
      
      modelfit <- lm(val_sample ~ timegroups, 
                     # one year dropped
                     data = .x[.x$timegroups != 2015,])
      
      pred <- predict(modelfit, se = TRUE,
                      # one year dropped
                      newdata = data.frame(timegroups = .x$timegroups[.x$timegroups != 2015]))
      
      num <- pred$fit[2] - pred$fit[1]
      den <- abs(pred$fit[1])
      numse <- sqrt(pred$se.fit[1]^2 + pred$se.fit[2]^2)
      dense <- pred$se.fit[1]
      
      sl1 = 100 * errordiv(num, den, numse, dense)[1] %>% as.numeric()
      slse1 = errordiv(num, den, numse, dense)[2] %>% as.numeric()
      
      
      modelfit <- lm(val_sample ~ timegroups, 
                     # one year dropped
                     data = .x[.x$timegroups != 2016,])
      
      pred <- predict(modelfit, se = TRUE,
                      # one year dropped
                      newdata = data.frame(timegroups = .x$timegroups[.x$timegroups != 2016]))
      
      num <- pred$fit[2] - pred$fit[1]
      den <- abs(pred$fit[1])
      numse <- sqrt(pred$se.fit[1]^2 + pred$se.fit[2]^2)
      dense <- pred$se.fit[1]
      
      sl2 = 100 * errordiv(num, den, numse, dense)[1] %>% as.numeric()
      slse2 = errordiv(num, den, numse, dense)[2] %>% as.numeric()
      
      
      modelfit <- lm(val_sample ~ timegroups, 
                     # one year dropped
                     data = .x[.x$timegroups != 2017,])
      
      pred <- predict(modelfit, se = TRUE,
                      # one year dropped
                      newdata = data.frame(timegroups = .x$timegroups[.x$timegroups != 2017]))
      
      num <- pred$fit[2] - pred$fit[1]
      den <- abs(pred$fit[1])
      numse <- sqrt(pred$se.fit[1]^2 + pred$se.fit[2]^2)
      dense <- pred$se.fit[1]
      
      sl3 = 100 * errordiv(num, den, numse, dense)[1] %>% as.numeric()
      slse3 = errordiv(num, den, numse, dense)[2] %>% as.numeric()
      
      
      modelfit <- lm(val_sample ~ timegroups, 
                     # one year dropped
                     data = .x[.x$timegroups != 2018,])
      
      pred <- predict(modelfit, se = TRUE,
                      # one year dropped
                      newdata = data.frame(timegroups = .x$timegroups[.x$timegroups != 2018]))
      
      num <- pred$fit[2] - pred$fit[1]
      den <- abs(pred$fit[1])
      numse <- sqrt(pred$se.fit[1]^2 + pred$se.fit[2]^2)
      dense <- pred$se.fit[1]
      
      sl4 = 100 * errordiv(num, den, numse, dense)[1] %>% as.numeric()
      slse4 = errordiv(num, den, numse, dense)[2] %>% as.numeric()
      
      
      modelfit <- lm(val_sample ~ timegroups, 
                     # one year dropped
                     data = .x[.x$timegroups != 2019,])
      
      pred <- predict(modelfit, se = TRUE,
                      # one year dropped
                      newdata = data.frame(timegroups = .x$timegroups[.x$timegroups != 2019]))
      
      num <- pred$fit[2] - pred$fit[1]
      den <- abs(pred$fit[1])
      numse <- sqrt(pred$se.fit[1]^2 + pred$se.fit[2]^2)
      dense <- pred$se.fit[1]
      
      sl5 = 100 * errordiv(num, den, numse, dense)[1] %>% as.numeric()
      slse5 = errordiv(num, den, numse, dense)[2] %>% as.numeric()
      
      
      modelfit <- lm(val_sample ~ timegroups, 
                     # one year dropped
                     data = .x[.x$timegroups != 2020,])
      
      pred <- predict(modelfit, se = TRUE,
                      # one year dropped
                      newdata = data.frame(timegroups = .x$timegroups[.x$timegroups != 2020]))
      
      num <- pred$fit[2] - pred$fit[1]
      den <- abs(pred$fit[1])
      numse <- sqrt(pred$se.fit[1]^2 + pred$se.fit[2]^2)
      dense <- pred$se.fit[1]
      
      sl6 = 100 * errordiv(num, den, numse, dense)[1] %>% as.numeric()
      slse6 = errordiv(num, den, numse, dense)[2] %>% as.numeric()
      
      
      modelfit <- lm(val_sample ~ timegroups, 
                     # one year dropped
                     data = .x[.x$timegroups != 2021,])
      
      pred <- predict(modelfit, se = TRUE,
                      # one year dropped
                      newdata = data.frame(timegroups = .x$timegroups[.x$timegroups != 2021]))
      
      num <- pred$fit[2] - pred$fit[1]
      den <- abs(pred$fit[1])
      numse <- sqrt(pred$se.fit[1]^2 + pred$se.fit[2]^2)
      dense <- pred$se.fit[1]
      
      sl7 = 100 * errordiv(num, den, numse, dense)[1] %>% as.numeric()
      slse7 = errordiv(num, den, numse, dense)[2] %>% as.numeric()
      
      
      modelfit <- lm(val_sample ~ timegroups, 
                     # one year dropped
                     data = .x[.x$timegroups != 2022,])
      
      pred <- predict(modelfit, se = TRUE,
                      # one year dropped
                      newdata = data.frame(timegroups = .x$timegroups[.x$timegroups != 2022]))
      
      num <- pred$fit[2] - pred$fit[1]
      den <- abs(pred$fit[1])
      numse <- sqrt(pred$se.fit[1]^2 + pred$se.fit[2]^2)
      dense <- pred$se.fit[1]
      
      sl8 = 100 * errordiv(num, den, numse, dense)[1] %>% as.numeric()
      slse8 = errordiv(num, den, numse, dense)[2] %>% as.numeric()
      
      
      .x %>%
        reframe(sl1 = sl1, slse1 = slse1,
                sl2 = sl2, slse2 = slse2,
                sl3 = sl3, slse3 = slse3,
                sl4 = sl4, slse4 = slse4,
                sl5 = sl5, slse5 = slse5,
                sl6 = sl6, slse6 = slse6,
                sl7 = sl7, slse7 = slse7,
                sl8 = sl8, slse8 = slse8)
      
      
    }) %>%
    
    # mean and SE of slope
    group_by(COMMON.NAME) %>%
    reframe(across(
      
      .cols = matches("^sl\\d+$"), # sl1 to sl8 but not "slse"s
      .fn = list(mean.slope = ~ mean(.),
                 se.slope = ~ sd(.) + sqrt(sum(get(str_replace(cur_column(), "sl", "slse"))^2) / 
                                             length(.))),
      .names = c("{.fn}_{.col}")
      
    )) %>% 
    rename_with(~ str_remove(., "_sl")) %>%
    
    # (mean + SE) to (LCI, mean, RCI)
    group_by(COMMON.NAME) %>%
    reframe(across(
      
      .cols = starts_with("mean.slope"), 
      .fn = list(lci = ~ . - 1.96 * get(str_replace(cur_column(), "mean", "se")),
                 mean = ~ .,
                 rci = ~ . + 1.96 * get(str_replace(cur_column(), "mean", "se"))),
      .names = c("currentslope{.fn}{.col}")
      
    )) %>%
    rename_with(~ str_remove(., "mean.slope"))
  
  # joining to data object
  sens <- sens %>%
    left_join(sl_data_sens, by = c("eBird.English.Name.2022" = "COMMON.NAME"))
  
  write.csv(sens, file = cursens_path, row.names = F)
  
  
  # calculations: trends (current): PROJ ------------------------------------
  
  ext_trends <- temp_sims %>%
    group_by(COMMON.NAME, sim) %>%
    group_modify(~ {
      
      datatopred <- data.frame(timegroups = extra.years)
      
      modelfit <- lm(log(val_sample) ~ timegroups, data = .x)
      pred <- predict(modelfit, newdata = datatopred, se = TRUE)
      
      # No need to calculate slope here. 
      
      first_year <- .x %>% filter(timegroups == recentcutoff)
      
      .x %>%
        reframe(timegroups = datatopred$timegroups,
                mean = pred$fit,
                se = pred$se.fit,
                # value in first year
                val = first_year$val)
      
    }) %>%
    # we want mean and SE of change in repfreq between years, so divide by first year value
    mutate(lci_bt = 100*exp(mean - 1.96 * se)/val, 
           mean_bt = 100*exp(mean)/val,
           rci_bt = 100*exp(mean + 1.96 * se)/val) %>%
    group_by(COMMON.NAME, timegroups) %>%
    reframe(lci_ext_std = mean(lci_bt),
            mean_ext_std = mean(mean_bt),
            rci_ext_std = mean(rci_bt))
  
  # ext_trends = ext_trends %>% select(-c(lci_bt,mean_bt,rci_bt))
  
  toc()
  
  # calculations: combining two trends --------------------------------------
  
  trends = trends %>% 
    left_join(modtrends) %>% 
    left_join(modtrends_recent) %>%
    dplyr::select(timegroups, COMMON.NAME, timegroupsf, mean_trans, se_trans,
                  lci, mean, rci, lci_std, mean_std, rci_std,
                  lci_std_recent, mean_std_recent, rci_std_recent) %>% 
    right_join(trends_framework) %>% 
    # projected trends
    left_join(ext_trends) %>% 
    # changing names to factor, so they maintain order
    mutate(COMMON.NAME = factor(COMMON.NAME, levels = base$COMMON.NAME)) %>%
    arrange(COMMON.NAME, timegroups) %>% 
    # <annotation_pending_AV>
    mutate(lci_comb_std = case_when(is.na(lci_ext_std) ~ lci_std_recent,
                                    !is.na(lci_ext_std) ~ lci_ext_std),
           mean_comb_std = case_when(is.na(mean_ext_std) ~ mean_std_recent,
                                     !is.na(mean_ext_std) ~ mean_ext_std),
           rci_comb_std = case_when(is.na(rci_ext_std) ~ rci_std_recent,
                                    !is.na(rci_ext_std) ~ rci_ext_std)) %>% 
    # truncating LCI at 0
    mutate(lci_comb_std = case_when(lci_comb_std < 0 ~ 0, 
                                    TRUE ~ lci_comb_std)) %>% 
    # ensuring correct order of columns
    relocate(timegroups, COMMON.NAME, timegroupsf, mean_trans, se_trans,
             lci, mean, rci, lci_std, mean_std, rci_std,
             lci_std_recent, mean_std_recent, rci_std_recent,
             lci_ext_std, mean_ext_std, rci_ext_std,
             lci_comb_std, mean_comb_std, rci_comb_std)
  
  write.csv(trends, file = trends_outpath, row.names = F)
  
  
  
  # joining future projected trends to main dataframe ###
  
  tojoin <- map(2023:2029, ~ trends %>%
                  filter(timegroups == .x) %>%
                  dplyr::select(COMMON.NAME, lci_comb_std, mean_comb_std, rci_comb_std) %>%
                  magrittr::set_colnames(c("eBird.English.Name.2022", 
                                           glue("proj{.x}.lci"),
                                           glue("proj{.x}.mean"),
                                           glue("proj{.x}.rci")))) %>% 
    reduce(full_join, by = "eBird.English.Name.2022")
  
  main <- main %>% 
    left_join(tojoin) %>% 
    # removing misIDd species "selection" for long-term and current analyses
    mutate(Long.Term.Analysis = ifelse(eBird.English.Name.2022 %in% spec_misid, 
                                       "", Long.Term.Analysis),
           Current.Analysis = ifelse(eBird.English.Name.2022 %in% spec_misid, 
                                     "", Current.Analysis))  
  
  
  # checkpoint-object "main"
  main4_postCATcomb <- main
  
  
}

# calculations: occupancy -------------------------------------------------

tic("Calculating occupancy")

if (skip_res_occu == TRUE) {
  
  # take relevant columns from wocats file of full-country
  tojoin <- read.csv("01_analyses_full/results/SoIB_main_wocats.csv") %>% 
    distinct(eBird.English.Name.2022, rangelci, rangemean, rangerci)
  
  # joining to main object
  main <- main %>% left_join(tojoin)
  
  # checkpoint-object "main"
  main5_postoccu <- main
  
  write.csv(main, file = mainwocats_path, row.names = F)
  
} else {

  # occupancy-model files
  # (path is same for all masks--the full-country folder)
  occu_model <- list.files(path = occu_mod_pathonly, full.names = T) %>% 
    map_df(read.csv)
  
  # in state, filtering for relevant grids
  if (cur_metadata$MASK.TYPE == "state") {
    occu_model <- occu_model %>% 
      filter(gridg1 %in% cur_grid_filt$gridg1,
             # we don't want species that have been reported from the state but aren't 
             # selected for the state
             COMMON.NAME %in% specieslist$COMMON.NAME)
  }
  
  # occupancy-presence files
  occu_presence <- list.files(path = occu_pres_pathonly, full.names = T) %>% 
    map_df(read.csv)

  # taking modelled occupancy values for species in cell where "absent"
  occ.full1 = occu_model %>% 
    filter(presence == 0) 
  
  # "presences"
  occ.full2 = occu_presence %>% 
    left_join(occu_model) %>% 
    dplyr::select(names(occ.full1))
  
  
  occu_full = rbind(occ.full1, occ.full2) %>% 
    mutate(gridg1 = as.character(gridg1)) %>% 
    # joining areas of each grid cell
    left_join(g1_in_sf %>% 
                st_drop_geometry() %>% 
                transmute(gridg1 = GRID.G1, area = AREA.G1)) %>% 
    # for grid cells where species present, taking overall occupancy to be 1
    mutate(occupancy = case_when(presence == 1 ~ 1, TRUE ~ occupancy),
           se = case_when(presence == 1 ~ 0, TRUE ~ se)) %>% 
    filter(!is.na(occupancy), !is.na(se), !is.na(gridg1))
  
  
  occu_summary = occu_full %>%
    filter(presence != 0 | prop_nb != 0) %>%
    group_by(COMMON.NAME, status) %>% 
    # <annotation_pending_AV>
    reframe(occ = sum(occupancy * area),
            occ.ci = round((erroradd(se * area)) * 1.96))
  
  est = array(data = NA, 
              dim = c(length(main$eBird.English.Name.2022), 2),
              dimnames = list(main$eBird.English.Name.2022, c("occ", "occ.ci")))
  
  
  # <annotation_pending_AV> full steps below
  
  for (i in main$eBird.English.Name.2022)
  {
    write_path <- glue("{occu_outpath}{i}.csv")
    cur_occu_full = occu_full %>% filter(COMMON.NAME == i)
    cur_occu_summary = occu_summary %>% filter(COMMON.NAME == i)
    
    # move to next species if this one empty
    if (length(cur_occu_full$COMMON.NAME) == 0)
      next
    
    # file to be used for creating maps later
    write.csv(cur_occu_full, file = write_path, row.names = F)
    
    l = length(cur_occu_summary$status)
    
    # occu_full may sometimes include an isolated grid cell where occupancy of a species
    # has been modelled, but where there are no eBird reports in either that cell or 
    # any of its neighbours. This happens because of edge cases in states, where 
    # one edge cell would have presence == 1 in the full country, but when looking at states, 
    # the part of the cell in that state would have presence == 0.
    if (l == 0) next
    
    for (j in 1:l)
    {
      if (cur_occu_summary$status[j] %in% c("MP") & 
          (is.na(est[i,"occ"]) | (as.numeric(cur_occu_summary$occ[j])>est[i,"occ"])))
      {
        est[i,"occ"] = cur_occu_summary$occ[j]
        est[i,"occ.ci"] = cur_occu_summary$occ.ci[j]
      }
      
      if (cur_occu_summary$status[j] %in% c("R","MS") & 
          (is.na(est[i,"occ"]) | (as.numeric(cur_occu_summary$occ[j]) > est[i,"occ"])))
      {
        est[i,"occ"] = cur_occu_summary$occ[j]
        est[i,"occ.ci"] = cur_occu_summary$occ.ci[j]
      }
      
      if (cur_occu_summary$status[j] %in% c("M","MW") & 
          (is.na(est[i,"occ"]) | (as.numeric(cur_occu_summary$occ[j]) > est[i,"occ"])))
      {
        est[i,"occ"] = cur_occu_summary$occ[j]
        est[i,"occ.ci"] = cur_occu_summary$occ.ci[j]
      }
      
    }
  }
  
  
  tojoin = data.frame(eBird.English.Name.2022 = rownames(est)) %>% 
    mutate(rangemean = round(as.numeric(est[, 1]) / 10000, 3),
           rangeci = round(as.numeric(est[, 2]) / 10000, 3)) %>% 
    mutate(rangelci = rangemean - rangeci,
           rangerci = rangemean + rangeci, 
           rangeci = NULL) %>% 
    mutate(rangemean = case_when(is.na(rangemean) &
                                   eBird.English.Name.2022 %in% specieslist$COMMON.NAME ~ 0,
                                 TRUE ~ rangemean)) %>% 
    mutate(across(c("rangelci", "rangerci"), ~ case_when(rangemean == 0 ~ 0, 
                                                         TRUE ~ .)))
  
  
  # joining to main object
  main <- main %>% left_join(tojoin)
  
  # checkpoint-object "main"
  main5_postoccu <- main
  
  write.csv(main, file = mainwocats_path, row.names = F)
  
}

toc()


# classification: assign SoIB Status categories ------------------------------

# any vagrant reported recently
spec_vagrants <- d %>% 
  filter(year > 2017) %>% 
  distinct(COMMON.NAME) %>%
  pull(COMMON.NAME)

# classifying into SoIB Status for long-term and current trends and range

# taking upper limit of CI for declines, and lower limit for increases

main = read.csv(mainwocats_path) %>%
  mutate(
    
    SOIBv2.Long.Term.Status = case_when(
      is.na(longtermmean) ~ "Insufficient Data",
      (longtermrci-longtermmean)/longtermmean > 0.5 ~ "Trend Inconclusive", # arbitrary
      # else
      # for declines
      longtermrci <= 50 ~ "Rapid Decline", # -100% to -50%
      longtermrci > 50 & longtermrci <= 75 ~ "Decline", # -50% to -25%
      # for increases
      longtermlci >= 150 ~ "Rapid Increase", # +50% to inf
      longtermlci < 150 & longtermlci >= 125 ~ "Increase", # +25% to +50%
      # stable vs inconclusive:
      # if CI is completely below or above the baseline, can't be stable
      longtermlci > 100 | longtermrci < 100 ~ "Trend Inconclusive",
      # if one limit is in the Stable zone but other limit passes to Rapid X, can't be stable
      longtermlci <= 50 | longtermrci >= 150 ~ "Trend Inconclusive",
      TRUE ~ "Stable"
      # OR:
      # (longtermlci > 50 & longtermlci <= 100) &
      #   (longtermrci >= 100 & longtermrci < 150) ~ "Stable",
      # TRUE ~ "Trend Inconclusive"
    ),
    
    SOIBv2.Current.Status = case_when(
      is.na(currentslopemean) ~ "Insufficient Data",
      (currentsloperci-currentslopelci) > 6 ~ "Trend Inconclusive", # arbitrary
      # <annotation_pending_AV> decline and increase values?
      # decreases
      currentsloperci <= -2.7 ~ "Rapid Decline",
      currentsloperci > -2.7 & currentsloperci <= -1.1 ~ "Decline",
      # increases
      currentslopelci >= 1.6 ~ "Rapid Increase",
      currentslopelci < 1.6 & currentslopelci >= 0.9 ~ "Increase",
      # if slope with SE is fully positive or negative, can't be stable
      currentsloperci < 0 | currentslopelci > 0 ~ "Trend Inconclusive",
      TRUE ~ "Stable"
      ),
    
    SOIBv2.Range.Status = case_when(
      is.na(rangemean) ~ NA_character_,
      rangemean == 0 & !(eBird.English.Name.2022 %in% spec_vagrants) ~ "Historical",
      # above is to prevent species that are not historical but classified as vagrants
      # from being classified as Historical (instead, Very Restricted)
      rangerci < 0.0625 ~ "Very Restricted",
      # larger threshold for species that are not island endemics
      (is.na(Restricted.Islands) & rangerci < 0.75) ~ "Very Restricted",
      rangerci < 4.25 ~ "Restricted",
      rangelci > 100 ~ "Very Large",
      rangelci > 25 ~ "Large",
      TRUE ~ "Moderate"
    )
    
  )


# for states, due to obviously smaller ranges within states, Range Status categories
# lose meaning. Only Historical is meaningful, and we want to capture this information.
# so, for states we retain NA and Historical classifications, but others are reverted to 
# full-country Range Status categories

if (cur_metadata$MASK.TYPE == "state") {
  
  main_tokeep <- main %>% filter(is.na(SOIBv2.Range.Status) | SOIBv2.Range.Status == "Historical")
  
  main_toupdate <- anti_join(main, main_tokeep) %>% dplyr::select(-SOIBv2.Range.Status)
  
  main_nat <- analyses_metadata %>% 
    filter(MASK == "none") %>% 
    pull(SOIBMAIN.PATH) %>% 
    read.csv() %>% 
    distinct(eBird.English.Name.2022, SOIBv2.Range.Status)
  
  main_update <- left_join(main_toupdate, main_nat)
  
  main <- bind_rows(main_tokeep, main_update)
  rm(main_tokeep, main_toupdate, main_nat, main_update)
  
}


# classification: adjust SoIB Status based on sensitivity for trends ----------------

# sensitivity check for long-term trends ###
if (to_run == TRUE) {
  
  modtrends1 = ltt_sens_class(modtrends1)
  modtrends2 = ltt_sens_class(modtrends2)
  modtrends3 = ltt_sens_class(modtrends3)
  modtrends4 = ltt_sens_class(modtrends4)
  modtrends5 = ltt_sens_class(modtrends5)
  
  sens_ltt <- main %>% 
    dplyr::select(eBird.English.Name.2022, SOIBv2.Long.Term.Status) %>% 
    # the modtrendsN files only have species for which we have run LTT
    filter(!is.na(SOIBv2.Long.Term.Status),
           SOIBv2.Long.Term.Status != "Insufficient Data") %>% 
    rename(COMMON.NAME = eBird.English.Name.2022) %>% 
    bind_rows(modtrends1, modtrends2, modtrends3, modtrends4, modtrends5) %>% 
    group_by(COMMON.NAME) %>% 
    # how many different status categories have been assigned?
    reframe(NO.STATUS = n_distinct(SOIBv2.Long.Term.Status),
            
            CONSERVATIVE.STATUS = case_when(
              
              NO.STATUS > 2 ~ "Trend Inconclusive",
              
              "Trend Inconclusive" %in% SOIBv2.Long.Term.Status ~ "Trend Inconclusive",
              
              NO.STATUS == 2 & 
                # if all Status assignments are either of the two increases
                ("Rapid Increase" %in% SOIBv2.Long.Term.Status &
                   "Increase" %in% SOIBv2.Long.Term.Status) ~ "Increase",
              NO.STATUS == 2 & 
                # if all Status assignments are either of the two decreases
                ("Rapid Decline" %in% SOIBv2.Long.Term.Status &
                   "Decline" %in% SOIBv2.Long.Term.Status) ~ "Decline",
              
              NO.STATUS == 2 ~ "Trend Inconclusive",
              TRUE ~ min(SOIBv2.Long.Term.Status)
              
            )) %>% 
    mutate(ROBUST = if_else(NO.STATUS == 1, 1, 0)) %>% 
    dplyr::select(-NO.STATUS)
  
  
  main <- main %>% 
    left_join(sens_ltt, by = c("eBird.English.Name.2022" = "COMMON.NAME")) %>% 
    # if Status assignment is not robust, take the most conservative one
    mutate(SOIBv2.Long.Term.Status = case_when(ROBUST == 0 ~ CONSERVATIVE.STATUS, 
                                               TRUE ~ SOIBv2.Long.Term.Status)) %>% 
    dplyr::select(-CONSERVATIVE.STATUS, -ROBUST)
  

  # sensitivity check for current trends ###
  
  # changes classifications based on sensitivity analyses
  
  sens <- read.csv(cursens_path)
  
  # classifying the sens values to SoIB categories
  sens_cat <- map(1:8, ~ {
    
    sens %>%
      # selecting the corresponding column each time
      mutate(currentslopelci = .[[2 + (.x - 1) * 3]],
             currentslopemean = .[[3 + (.x - 1) * 3]],
             currentsloperci = .[[4 + (.x - 1) * 3]]) %>%
      mutate(SOIBv2.Current.Status.Sens = case_when(
        
        is.na(currentslopemean) ~ "Insufficient Data",
        (currentsloperci - currentslopelci) > 6 ~ "Trend Inconclusive",
        currentsloperci <= -2.7 ~ "Rapid Decline",
        currentsloperci <= -1.1 ~ "Decline",
        currentslopelci >= 1.6 ~ "Rapid Increase",
        currentslopelci >= 0.9 ~ "Increase",
        currentsloperci < 0 ~ "Trend Inconclusive",
        currentslopelci > 0 ~ "Trend Inconclusive",
        TRUE ~ "Stable"
        
      )) %>%
      dplyr::select(eBird.English.Name.2022, SOIBv2.Current.Status.Sens) %>% 
      magrittr::set_colnames(c("eBird.English.Name.2022", glue("s{.x}")))
    
  }) %>%
    reduce(full_join) 
  
  sens_cat <- main %>% 
    dplyr::select(eBird.English.Name.2022, SOIBv2.Current.Status) %>% 
    left_join(sens_cat) %>%
    filter(!SOIBv2.Current.Status %in% c("Insufficient Data", "Trend Inconclusive"))
  
  
  # creating empty vectors that will be filled with indices of species that fall
  # under 7 criteria
  
  ind1 = numeric(0)
  ind2 = numeric(0)
  ind3 = numeric(0)
  ind4 = numeric(0)
  ind5 = numeric(0)
  ind6 = numeric(0)
  ind7 = numeric(0)
  
  for (i in 1:length(sens_cat$eBird.English.Name.2022)) {
    
    categs = as.vector(sens_cat[i,-1])
    
    # species with same category across all columns
    if (length(unique(categs)) == 1) 
      (ind1 = c(ind1, i))
    
    # species with any one category but also Inconclusive
    if (length(unique(categs)) == 2 & "Trend Inconclusive" %in% categs) 
      (ind2 = c(ind2, i))
    
    # species classed as some decline, but elsewhere classed as some non-decline
    if (("Decline" %in% categs | "Rapid Decline" %in% categs) & 
        ("Stable" %in% categs | "Increase" %in% categs | "Rapid Increase" %in% categs)) 
      (ind3 = c(ind3, i))
    
    # species classed as some increase, but elsewhere classed as some non-increase
    if (("Increase" %in% categs | "Rapid Increase" %in% categs) & 
        ("Stable" %in% categs | "Decline" %in% categs | "Rapid Decline" %in% categs))
      (ind4 = c(ind4, i))
    
    # species classed Inconclusive in >= 4 out of 9 columns
    if (length(categs[categs == "Trend Inconclusive"]) >= 4)
      (ind5 = c(ind5, i))
    
    # species with Rapid Decline in main trend and only Decline in drop-trends
    if (categs[1] == "Rapid Decline" & "Decline" %in% categs)
      (ind6 = c(ind6, i))
    
    # species with Rapid Increase in main trend and only Increase in drop-trends
    if (categs[1] == "Rapid Increase" & "Increase" %in% categs)
      (ind7 = c(ind7, i))
    
  }
  
  ind.rem = ind3 %>% union(ind4) %>% union(ind5)# to be removed (Inconclusive)
  
  ind6 = ind6 %>% setdiff(ind.rem)
  ind7 = ind7 %>% setdiff(ind.rem)
  
  spec_ind.rem <- sens_cat$eBird.English.Name.2022[ind.rem]
  spec_ind6 <- sens_cat$eBird.English.Name.2022[ind6]
  spec_ind7 <- sens_cat$eBird.English.Name.2022[ind7]
  
  
  
  
  main <- main %>%
    # changing classification where needed
    mutate(SOIBv2.Current.Status = case_when(
      
      eBird.English.Name.2022 %in% spec_ind.rem ~ "Trend Inconclusive",
      eBird.English.Name.2022 %in% spec_ind6 ~ "Decline",
      eBird.English.Name.2022 %in% spec_ind7 ~ "Increase",
      TRUE ~ SOIBv2.Current.Status
      
    )) %>% 
    mutate(across(c(SOIBv2.Long.Term.Status, SOIBv2.Current.Status, SOIB.Range.Status),
                  ~ if_else(Selected.SOIB != "X", NA_character_, .)))
  
} else {

  print(glue("Skipping sensitivity-check-based adjustments to Status for {cur_mask}"))
  
}

# classification: assign SoIB Priority status (based on trends and occupancy) -----

cats_trend = c("Rapid Decline", "Decline", "Insufficient Data", 
               "Trend Inconclusive", "Stable", "Increase", "Rapid Increase")
cats_range = c("Historical", "Very Restricted", "Restricted", 
               "Moderate", "Large", "Very Large")

cats_decline = c("Decline", "Rapid Decline")
cats_uncertain = c("Insufficient Data", "Trend Inconclusive")
cats_restricted = c("Historical", "Very Restricted", "Restricted")


# old categories
cats_trend_soib1 = c("Strong Decline", "Moderate Decline", "Data Deficient", 
                     "Uncertain", "Stable", "Moderate Increase", "Strong Increase")

cats_decline_soib1 = c("Moderate Decline", "Strong Decline")
cats_uncertain_soib1 <- c("Data Deficient", "Uncertain")


main = main %>% 
  left_join(priorityrules) %>%
  mutate(SOIBv2.Priority.Status = as.character(SOIBv2.Priority.Status)) %>%
  # changing priority rules based on IUCN category (which isn't considered in rules)
  mutate(SOIBv2.Priority.Status = case_when(
    
    IUCN.Category %in% c("Extinct in the Wild", "Extinct") ~ "Low",
    
    SOIBv2.Long.Term.Status %in% cats_uncertain & 
      SOIBv2.Current.Status %in% cats_uncertain &
      IUCN.Category %in% c("Endangered", "Critically Endangered") ~ "High",
    
    SOIBv2.Long.Term.Status %in% cats_decline & 
      SOIBv2.Current.Status %in% cats_decline &
      IUCN.Category %in% c("Endangered", "Critically Endangered") ~ "High",
    
    SOIBv2.Long.Term.Status %in% cats_uncertain & 
      SOIBv2.Current.Status %in% cats_uncertain &
      SOIBv2.Range.Status %in% cats_restricted &
      IUCN.Category %in% c("Vulnerable") ~ "High",
    
    SOIBv2.Long.Term.Status %in% cats_uncertain & 
      SOIBv2.Current.Status %in% cats_uncertain &
      IUCN.Category %in% c("Near Threatened", "Vulnerable") & 
      SOIBv2.Priority.Status == "Low" ~ "Moderate",
    
    SOIBv2.Long.Term.Status == "Insufficient Data" & 
      SOIBv2.Current.Status == "Insufficient Data" &
      Endemic.Region != "None" & 
      SOIBv2.Priority.Status == "Low" ~ "Moderate",
    
    TRUE ~ SOIBv2.Priority.Status
    
  )) %>%
  # converting percentage-of-year1 to percentage-change 
  mutate(across(c(longtermlci, longtermmean, longtermrci),
                ~ . - 100)) %>% 
  # ensuring correct order of columns
  relocate(eBird.English.Name.2022, eBird.Scientific.Name.2022, eBird.Code, Order, Family, 
           SOIB.Concern.Status, SOIB.Long.Term.Status, SOIB.Current.Status, SOIB.Range.Status, 
           Breeding.Activity.Period, Non.Breeding.Activity.Period,
           Diet.Guild, India.Endemic, Subcontinent.Endemic, Himalayas.Endemic, Endemic.Region, 
           Habitat.Specialization, Migratory.Status.Within.India, Essential, Discard, 
           Restricted.Islands,
           India.Checklist.Common.Name, India.Checklist.Scientific.Name, 
           BLI.Common.Name, BLI.Scientific.Name, IUCN.Category, WPA.Schedule,
           CITES.Appendix, CMS.Appendix, Onepercent.Estimates, 
           Long.Term.Analysis, Current.Analysis, Selected.SOIB, 
           totalrange25km, proprange25km2000, proprange25km.current, proprange25km2022, 
           mean5km, ci5km, 
           proj2023.lci, proj2023.mean, proj2023.rci, proj2024.lci, proj2024.mean, proj2024.rci, 
           proj2025.lci, proj2025.mean, proj2025.rci, proj2026.lci, proj2026.mean, proj2026.rci, 
           proj2027.lci, proj2027.mean, proj2027.rci, proj2028.lci, proj2028.mean, proj2028.rci, 
           proj2029.lci, proj2029.mean, proj2029.rci, 
           longtermlci, longtermmean, longtermrci, 
           currentslopelci, currentslopemean, currentsloperci, 
           rangelci, rangemean, rangerci,
           SOIBv2.Long.Term.Status, SOIBv2.Current.Status, SOIBv2.Range.Status, 
           SOIBv2.Priority.Status)

main = main %>%
  mutate(
    Long.Term.Analysis = case_when(
    SOIBv2.Long.Term.Status == "Insufficient Data" ~ "",
    TRUE ~ Long.Term.Analysis),
    Current.Analysis = case_when(
      SOIBv2.Current.Status == "Insufficient Data" ~ "",
      TRUE ~ Current.Analysis)
    )

write.csv(main, file = main_path, row.names = F)


# summaries -------------------------------------------------------------------

# species qualifications
species_qual0 <- main %>%
  mutate(Range.Analysis = if_else(is.na(SOIBv2.Range.Status), "", "X")) %>% 
  summarise(across(c(Selected.SOIB, Long.Term.Analysis, Current.Analysis, Range.Analysis),
                   # adds up cases where condition is true
                   ~ sum(. == "X")))

# number of species with conclusive trends
nspec_trend_inconc <- data.frame(Category = cats_trend) %>% 
  magrittr::set_colnames("Trend Status") %>% 
  left_join(main %>% 
              count(SOIBv2.Long.Term.Status) %>% 
              magrittr::set_colnames(c("Trend Status", "Long-term species (no.)"))) %>% 
  left_join(main %>% 
              count(SOIBv2.Current.Status) %>% 
              magrittr::set_colnames(c("Trend Status", "Current species (no.)"))) %>% 
  filter(`Trend Status` == "Trend Inconclusive")
nspec_trend_conc_ltt = species_qual0$Long.Term.Analysis - nspec_trend_inconc$`Long-term species (no.)`
nspec_trend_conc_cat = species_qual0$Current.Analysis - nspec_trend_inconc$`Current species (no.)`

species_qual <- species_qual0 %>% 
  magrittr::set_colnames(c("SoIB 2023 Assessment", "Long-term Analysis",
                           "Current Analysis", "Range Analysis")) %>% 
  pivot_longer(everything(), 
               names_to = "No. of species in:", 
               values_to = "Selected for analysis") %>% 
  mutate(`With conclusive trends` = case_when(
    `No. of species in:` == "Long-term Analysis" ~ nspec_trend_conc_ltt,
    `No. of species in:` == "Current Analysis" ~ nspec_trend_conc_cat
  ))


# status tabulations
status_trends = data.frame(Category = cats_trend) %>% 
  magrittr::set_colnames("Trend Status") %>% 
  left_join(main %>% 
              count(SOIBv2.Long.Term.Status) %>% 
              magrittr::set_colnames(c("Trend Status", "Long-term species (no.)"))) %>% 
  left_join(main %>% 
              count(SOIBv2.Current.Status) %>% 
              magrittr::set_colnames(c("Trend Status", "Current species (no.)"))) %>% 
  # percentages
  mutate(`Long-term species conclusive (perc.)` = case_when(
    `Trend Status` %in% cats_uncertain ~ NA_real_,
    TRUE ~ round(100 * (`Long-term species (no.)` / nspec_trend_conc_ltt), 1)
    ),
    `Current species conclusive (perc.)` = case_when(
      `Trend Status` %in% cats_uncertain ~ NA_real_,
      TRUE ~ round(100 * (`Current species (no.)` / nspec_trend_conc_cat), 1)
    )) %>% 
  mutate(`Trend Status` = factor(`Trend Status`,
                                 levels = c("Rapid Decline", "Decline", "Stable", 
                                            "Increase", "Rapid Increase", 
                                            "Trend Inconclusive", "Insufficient Data"))) %>% 
  arrange(`Trend Status`)

status_range <- data.frame(Category = cats_range) %>% 
  magrittr::set_colnames("Range Status") %>% 
  left_join(main %>% 
              count(SOIBv2.Range.Status) %>% 
              magrittr::set_colnames(c("Range Status", "Species (no.)"))) %>% 
  mutate(`Species (perc.)` = round(100 * (`Species (no.)` / sum(`Species (no.)`)), 1))

status_priority <- main %>% 
  filter(!is.na(SOIBv2.Priority.Status)) %>% # count() counts NA also
  mutate(SOIBv2.Priority.Status = factor(SOIBv2.Priority.Status, 
                                         levels = c("High", "Moderate", "Low"))) %>% 
  count(SOIBv2.Priority.Status) %>% 
  magrittr::set_colnames(c("Priority Status", "No. of species"))


# break-up of how SoIB 2023 High Priority species which were not High in 2020 were attained
high_priority_breakup_new = main %>%
  filter(SOIBv2.Priority.Status == "High",
         SOIB.Concern.Status != "High" | is.na(SOIB.Concern.Status)) %>%
  transmute(Breakup = case_when(
    
    # we have conclusive trend this time, but last time was inconclusive or NA
    (!SOIBv2.Long.Term.Status %in% cats_uncertain | !SOIBv2.Current.Status %in% cats_uncertain) &
      (SOIB.Long.Term.Status %in% cats_uncertain_soib1 | is.na(SOIB.Long.Term.Status) |
         SOIB.Current.Status %in% cats_uncertain_soib1 | is.na(SOIB.Current.Status)) ~ "Trend New",
    
    # had conclusive trends both times, but this time trend different
    (!SOIBv2.Long.Term.Status %in% cats_uncertain | !SOIBv2.Current.Status %in% cats_uncertain) ~ 
      "Trend Different",
    
    # if trends not different or new, assigned high priority based on range
    SOIBv2.Range.Status == "Very Restricted" ~ "Range",
    
    # if not even by range, then assigned high priority based on IUCN category
    TRUE ~ "IUCN"
    
  )) %>% 
  mutate(Breakup = factor(Breakup, 
                          levels = c("Trend New", "Trend Different", "Range", "IUCN"))) %>%
  count(Breakup) %>% 
  mutate(Perc = round(100 * (n / sum(n)), 1)) %>% 
  magrittr::set_colnames(c("Break-up", "New High Species (no.)", "New High Species (perc.)"))

# break-up of how SoIB 2023 High Priority species were attained
high_priority_breakup = main %>%
  filter(SOIBv2.Priority.Status == "High") %>%
  transmute(Breakup = case_when(
    
    # we have conclusive trend this time, but last time was inconclusive or NA
    (!SOIBv2.Long.Term.Status %in% cats_uncertain | !SOIBv2.Current.Status %in% cats_uncertain) &
      (SOIB.Long.Term.Status %in% cats_uncertain_soib1 | is.na(SOIB.Long.Term.Status) |
         SOIB.Current.Status %in% cats_uncertain_soib1 | is.na(SOIB.Current.Status)) ~ "Trend New",
    
    # had conclusive trends both times, but this time trend different
    (!SOIBv2.Long.Term.Status %in% cats_uncertain | !SOIBv2.Current.Status %in% cats_uncertain) ~ 
      "Trend Different",
    
    # if trends not different or new, assigned high priority based on range
    SOIBv2.Range.Status == "Very Restricted" ~ "Range",
    
    # if not even by range, then assigned high priority based on IUCN category
    TRUE ~ "IUCN"
    
  )) %>% 
  mutate(Breakup = factor(Breakup, 
                          levels = c("Trend New", "Trend Different", "Range", "IUCN"))) %>%
  count(Breakup) %>% 
  mutate(Perc = round(100 * (n / sum(n)), 1)) %>% 
  magrittr::set_colnames(c("Break-up", "High Species (no.)", "High Species (perc.)")) %>% 
  left_join(high_priority_breakup_new)



if (cur_metadata$MASK.TYPE == "country") {
  
  # comparing two SoIBs
  SoIB1_SoIB2 = main %>%
    filter(!is.na(SOIB.Concern.Status) & SOIB.Concern.Status != "") %>%
    group_by(SOIB.Concern.Status) %>% 
    mutate(n = n()) %>%
    group_by(SOIB.Concern.Status, SOIBv2.Priority.Status) %>%
    reframe(NO.SPEC = n(),
            PERC.SPEC = round(100 * (NO.SPEC / max(n)), 1)) %>%
    magrittr::set_colnames(c("SOIB Concern Status", "SOIBv2 Priority Status", 
                             "Species (no.)", "Species (perc.)")) 
  
  SoIB2_SoIB1 = main %>%
    filter(!is.na(SOIBv2.Priority.Status) & SOIBv2.Priority.Status != "") %>%
    group_by(SOIBv2.Priority.Status) %>% 
    mutate(n = n()) %>%
    group_by(SOIBv2.Priority.Status, SOIB.Concern.Status) %>%
    reframe(NO.SPEC = n(),
            PERC.SPEC = round(100 * (NO.SPEC / max(n)), 1)) %>%
    magrittr::set_colnames(c("SOIBv2 Priority Status", "SOIB Concern Status", 
                             "Species (no.)", "Species (perc.)")) 
  
  
  # cross-tab of SoIB and IUCN assessments
  SoIB_vs_IUCN_0 <- main %>% 
    filter(Selected.SOIB == "X") %>% 
    mutate(SOIBv2.Priority.Status = factor(SOIBv2.Priority.Status, 
                                           levels = c("High", "Moderate", "Low")),
           IUCN.Category = factor(IUCN.Category, levels = c(
             "Critically Endangered", "Endangered", "Vulnerable", "Near Threatened", 
             "Least Concern", "Not Recognised"
           ))) %>% 
    group_by(SOIBv2.Priority.Status, IUCN.Category) %>% 
    tally() %>% 
    pivot_wider(names_from = SOIBv2.Priority.Status, values_from = n) %>% 
    replace(is.na(.), 0) %>% 
    magrittr::set_colnames(c(" ", "High", "Moderate", "Low")) 
  
  temp <- SoIB_vs_IUCN_0 %>% 
    reframe(across(c("High", "Moderate", "Low"), sum)) %>% 
    mutate(new = "Sum") %>% 
    relocate(new, High, Moderate, Low) %>% 
    magrittr::set_colnames(c(" ", "High", "Moderate", "Low"))
  
  SoIB_vs_IUCN <- SoIB_vs_IUCN_0 %>% 
    bind_rows(temp) %>% 
    mutate(Sum = High + Low + Moderate)
  
  SoIB_vs_IUCN_percIUCN = SoIB_vs_IUCN_0 %>% 
    mutate(Sum = High + Low + Moderate) %>%
    mutate(across(c("High", "Moderate", "Low"), 
                  ~ round(100 * (. / Sum), 1))) %>% 
    mutate(Sum = NULL)
  
  SoIB_vs_IUCN_percSoIB = SoIB_vs_IUCN_0 %>% 
    column_to_rownames(" ") %>% 
    t() %>% 
    as.data.frame() %>% 
    rownames_to_column("SoIB") %>% 
    rowwise() %>% 
    mutate(Sum = sum(c_across(c(everything(), - SoIB)))) %>%
    mutate(across(c(everything(), - SoIB), 
                  ~ round(100 * (. / Sum), 1))) %>% 
    mutate(Sum = NULL) %>% 
    rename(` ` = SoIB)


# reasons for uplisting or downlisting
reason.uplist.high = main %>%
  filter(SOIB.Concern.Status %in% c("Low", "Moderate"),
         SOIBv2.Priority.Status == "High") %>%
  mutate(Breakup = case_when(
    
    # LTT and CAT were uncertain in 2020 but we have some trend now
    ((!SOIBv2.Long.Term.Status %in% cats_uncertain | !SOIBv2.Current.Status %in% cats_uncertain) &
       SOIB.Long.Term.Status %in% cats_uncertain_soib1 & 
       SOIB.Current.Status %in% cats_uncertain_soib1)  ~ "First-time trend",
    
    # stronger decline this time (LTT)
    (SOIBv2.Long.Term.Status == "Rapid Decline" &
       SOIB.Long.Term.Status %in% cats_trend_soib1[!cats_trend_soib1 == "Strong Decline" & 
                                                   !cats_trend_soib1 %in% cats_uncertain_soib1]) |
      (SOIBv2.Long.Term.Status %in% cats_decline &
         SOIB.Long.Term.Status %in% cats_trend_soib1[!cats_trend_soib1 %in% cats_decline_soib1 & 
                                                       !cats_trend_soib1 %in% cats_uncertain_soib1])
       ~ "More decline in LTT",
    
    # stronger decline this time (CAT)
    (SOIBv2.Current.Status == "Rapid Decline" &
       SOIB.Current.Status %in% cats_trend_soib1[!cats_trend_soib1 == "Strong Decline" & 
                                                     !cats_trend_soib1 %in% cats_uncertain_soib1]) |
      (SOIBv2.Current.Status %in% cats_decline &
         SOIB.Current.Status %in% cats_trend_soib1[!cats_trend_soib1 %in% cats_decline_soib1 & 
                                                       !cats_trend_soib1 %in% cats_uncertain_soib1])
    ~ "More decline in CAT",
    
    # first-time LTT & first-time CAT
    (SOIB.Long.Term.Status %in% cats_uncertain_soib1 &
       !SOIBv2.Long.Term.Status %in% cats_uncertain) ~ "First-time LTT",
    (SOIB.Current.Status %in% cats_uncertain_soib1 &
       !SOIBv2.Current.Status %in% cats_uncertain) ~ "First-time CAT",
    
    # both times, had at least one of two trends
    (!SOIB.Long.Term.Status %in% cats_uncertain_soib1 | 
       !SOIB.Current.Status %in% cats_uncertain_soib1) &
      (!SOIBv2.Long.Term.Status %in% cats_uncertain | 
         !SOIBv2.Current.Status %in% cats_uncertain)  ~ "Other changes in trends",
    
    # earlier had at least one trend, but this time both trends uncertain
    (!SOIB.Long.Term.Status %in% cats_uncertain_soib1 | 
       !SOIB.Current.Status %in% cats_uncertain_soib1) &
      (SOIBv2.Long.Term.Status %in% cats_uncertain & 
         SOIBv2.Current.Status %in% cats_uncertain) ~ "Loss of trends",
    
    (SOIB.Range.Status == "Restricted" & SOIBv2.Range.Status == "Very Restricted") |
    (SOIB.Range.Status == "Moderate" & SOIBv2.Range.Status == "Restricted") |
    (SOIB.Range.Status == "Large" & SOIBv2.Range.Status == "Moderate") ~ "Reducing range",
    
    TRUE ~ "Others"
    
    )) %>%
  mutate(Breakup = factor(Breakup, 
                          levels = c("More decline in LTT", "More decline in CAT",
                                     "First-time trend", "First-time LTT", "First-time CAT",
                                     "Other changes in trends", "Loss of trends",
                                     "Reducing range", "Others"))) %>%
  count(Breakup) %>% 
  mutate(Perc = round(100 * (n / sum(n)), 1)) %>% 
  complete(Breakup, fill = list(n = 0, Perc = 0)) %>% 
  magrittr::set_colnames(c("Break-up", "Species (no.)", "Species (perc.)"))


reason.downlist.high = main %>%
  filter(SOIBv2.Priority.Status %in% c("Low", "Moderate"),
         SOIB.Concern.Status == "High") %>%
  mutate(Breakup = case_when(
    
    # LTT and CAT were uncertain in 2020 but we have some trend now
    ((!SOIBv2.Long.Term.Status %in% cats_uncertain | !SOIBv2.Current.Status %in% cats_uncertain) &
       SOIB.Long.Term.Status %in% cats_uncertain_soib1 & 
       SOIB.Current.Status %in% cats_uncertain_soib1)  ~ "First-time trend",
    
    # weaker decline this time (LTT)
    (SOIB.Long.Term.Status == "Strong Decline" &
       SOIBv2.Long.Term.Status %in% cats_trend[!cats_trend == "Rapid Decline" & 
                                                 !cats_trend %in% cats_uncertain]) |
      (SOIB.Long.Term.Status %in% cats_decline_soib1 &
         SOIBv2.Long.Term.Status %in% cats_trend[!cats_trend %in% cats_decline & 
                                                   !cats_trend %in% cats_uncertain]) |
      (SOIB.Long.Term.Status %in% c(cats_decline_soib1, "Stable") &
         SOIBv2.Long.Term.Status %in% c("Increase", "Rapid Increase")) |
      (SOIB.Long.Term.Status %in% c(cats_decline_soib1, "Stable", "Moderate Increase") &
         SOIBv2.Long.Term.Status == "Rapid Increase")
    ~ "Less decline in LTT",
    
    # stronger decline this time (CAT)
    (SOIB.Current.Status == "Strong Decline" &
       SOIBv2.Current.Status %in% cats_trend[!cats_trend == "Rapid Decline" & 
                                                 !cats_trend %in% cats_uncertain]) |
      (SOIB.Current.Status %in% cats_decline_soib1 &
         SOIBv2.Current.Status %in% cats_trend[!cats_trend %in% cats_decline & 
                                                   !cats_trend %in% cats_uncertain]) |
      (SOIB.Current.Status %in% c(cats_decline_soib1, "Stable") &
         SOIBv2.Current.Status %in% c("Increase", "Rapid Increase")) |
      (SOIB.Current.Status %in% c(cats_decline_soib1, "Stable", "Moderate Increase") &
         SOIBv2.Current.Status == "Rapid Increase")
    ~ "Less decline in CAT",

    # first-time LTT & first-time CAT
    (SOIB.Long.Term.Status %in% cats_uncertain_soib1 &
       !SOIBv2.Long.Term.Status %in% cats_uncertain) ~ "First-time LTT",
    (SOIB.Current.Status %in% cats_uncertain_soib1 &
       !SOIBv2.Current.Status %in% cats_uncertain) ~ "First-time CAT",

    # both times, had at least one of two trends
    (!SOIB.Long.Term.Status %in% cats_uncertain_soib1 | 
       !SOIB.Current.Status %in% cats_uncertain_soib1) &
      (!SOIBv2.Long.Term.Status %in% cats_uncertain | 
         !SOIBv2.Current.Status %in% cats_uncertain)  ~ "Other changes in trends",
    
    # earlier had at least one trend, but this time both trends uncertain
    (!SOIB.Long.Term.Status %in% cats_uncertain_soib1 | 
       !SOIB.Current.Status %in% cats_uncertain_soib1) &
      (SOIBv2.Long.Term.Status %in% cats_uncertain & 
         SOIBv2.Current.Status %in% cats_uncertain) ~ "Loss of trends",
    
    (SOIBv2.Range.Status == "Restricted" & SOIB.Range.Status == "Very Restricted") |
      (SOIBv2.Range.Status == "Moderate" & SOIB.Range.Status == "Restricted") |
      (SOIBv2.Range.Status == "Large" & SOIB.Range.Status == "Moderate") |
      (SOIBv2.Range.Status == "Very Large" & SOIB.Range.Status == "Large") ~ "Increasing range",
    
    TRUE ~ "Others"
    
  )) %>%
  mutate(Breakup = factor(Breakup, 
                          levels = c("Less decline in LTT", "Less decline in CAT",
                                     "First-time trend", "First-time LTT", "First-time CAT",
                                     "Other changes in trends", "Loss of trends",
                                     "Increasing range", "Others"))) %>%
  count(Breakup) %>% 
  mutate(Perc = round(100 * (n / sum(n)), 1)) %>% 
  complete(Breakup, fill = list(n = 0, Perc = 0)) %>% 
  magrittr::set_colnames(c("Break-up", "Species (no.)", "Species (perc.)"))

}


if (cur_metadata$MASK.TYPE == "country") {
  write_xlsx(path = summaries_path,
             list("Trends Status" = status_trends,
                  "Range Status" = status_range,
                  "Priority Status" = status_priority,
                  "Species qualification" = species_qual,
                  "High Priority break-up" = high_priority_breakup,
                  "SoIB 2020 vs 2023" = SoIB1_SoIB2,
                  "SoIB 2023 vs 2020" = SoIB2_SoIB1,
                  "SoIB vs IUCN (no.)" = SoIB_vs_IUCN,
                  "SoIB vs IUCN (IUCN %)" = SoIB_vs_IUCN_percIUCN,
                  "SoIB vs IUCN (SoIB %)" = SoIB_vs_IUCN_percSoIB,
                  "Reason for uplisting" = reason.uplist.high,
                  "Reason for downlisting" = reason.downlist.high))
} else {
  write_xlsx(path = summaries_path,
             list("Trends Status" = status_trends,
                  "Range Status" = status_range,
                  "Priority Status" = status_priority,
                  "Species qualification" = species_qual,
                  "High Priority break-up" = high_priority_breakup))
}