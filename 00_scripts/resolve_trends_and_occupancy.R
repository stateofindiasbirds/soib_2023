library(tidyverse)
library(VGAM)
library(sf)
library(writexl)

load("00_data/analyses_metadata.RData")


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

###

source('00_scripts/00_functions.R')

recentcutoff = 2015

load("00_data/spec_misid.RData") # to remove from LTT and CAT "selection" later
# for occupancy
load("00_data/maps_sf.RData")
load(speclist_path)


###

# don't run resolve_species_trends if no species selected
run_res_trends <- ((1 %in% specieslist$ht) | (1 %in% specieslist$rt) |
                     (1 %in% restrictedspecieslist$ht) | (1 %in% restrictedspecieslist$rt)) &
  # edge cases (Tripura, Nagaland, Puducherry) where species selected, but trends could not be generated
  (length(list.files(trends_pathonly)) != 0)


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

if (run_res_trends == FALSE) {
  
  # list of columns that need to be created since we have skipped steps
  na_columns <- c("longtermlci", "longtermmean", "longtermrci",     
                  "currentslopelci", "currentslopemean", "currentsloperci", 
                  "proj2023.lci", "proj2023.mean", "proj2023.rci",    
                  "proj2024.lci", "proj2024.mean", "proj2024.rci",    
                  "proj2025.lci", "proj2025.mean", "proj2025.rci",    
                  "proj2026.lci", "proj2026.mean", "proj2026.rci",    
                  "proj2027.lci", "proj2027.mean", "proj2027.rci",    
                  "proj2028.lci", "proj2028.mean", "proj2028.rci",    
                  "proj2029.lci", "proj2029.mean", "proj2029.rci")
  
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
  modtrends1 <- ltt_sens_sim(my_seed = 1, data = modtrends)
  modtrends2 <- ltt_sens_sim(my_seed = 2, data = modtrends)
  modtrends3 <- ltt_sens_sim(my_seed = 3, data = modtrends)
  modtrends4 <- ltt_sens_sim(my_seed = 4, data = modtrends)
  modtrends5 <- ltt_sens_sim(my_seed = 5, data = modtrends)

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

# saving info for trends columns
write.csv(main, file = mainwocats_path, row.names = F)

# calculations: occupancy -------------------------------------------------

tic("Calculating occupancy")

if (skip_res_occu == FALSE) {
  
  # if TRUE, simply pulling in columns from full country--happens after all resolves
  # (for masks where skip_res_occu == FALSE) are finished.

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


