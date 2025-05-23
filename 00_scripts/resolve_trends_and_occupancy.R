library(tidyverse)
library(glue)
library(tictoc)
library(VGAM)
library(sf)
library(writexl)

source('00_scripts/00_functions.R')

# setup -------------------------------------------------------------------

# preparing data for specific mask (this is the only part that changes, but automatically)
cur_metadata <- get_metadata(cur_mask)

# read paths
base_path <- cur_metadata$FULLSPECLIST.PATH
speclist_path <- cur_metadata$SPECLISTDATA.PATH

trends_pathonly <- cur_metadata$TRENDS.PATHONLY
occu_pres_pathonly <- cur_metadata$OCCU.PRES.PATHONLY
occu_mod_pathonly <- cur_metadata$OCCU.MOD.PATHONLY

# write paths
lttsens_path <- cur_metadata$LTTSENS.PATH
cursens_path <- cur_metadata$CURSENS.PATH 
trends_outpath <- cur_metadata$TRENDS.OUTPATH
occu_outpath <- cur_metadata$OCCU.OUTPATH

mainwocats_path <- cur_metadata$SOIBMAIN.WOCATS.PATH


# in interannual updates, we need to delete all past-year output files
# because species names change every year with taxonomy updates.
# hence, although most species' files will simply get overwritten, for many
# species we will end up with multiple files, one for each taxonomy update
# (if not interannual update, everything will be in a new repo so no need for this.)
if (interannual_update == TRUE) {
  
  files_to_del <- list.files(occu_outpath, full.names = TRUE)
  
  if (length(files_to_del) != 0) {
    file.remove(files_to_del)
  }
  
}


###

recentcutoff = soib_year_info("cat_start")

load("00_data/spec_misid.RData") # to remove from LTT and CAT "selection" later
# for occupancy
load("00_data/maps_sf.RData")
load(speclist_path)


### for conditionals ###

# don't run resolve_species_trends if no species selected
run_res_trends <- ((1 %in% specieslist$ht) | (1 %in% specieslist$rt) |
                     (1 %in% restrictedspecieslist$ht) | (1 %in% restrictedspecieslist$rt)) &
  # edge cases (Tripura, Nagaland, Puducherry) where species selected, but trends could not be generated
  (length(list.files(trends_pathonly)) != 0)

run_res_trends_LTT <- ((1 %in% specieslist$ht) | (1 %in% restrictedspecieslist$ht)) &
  (length(list.files(trends_pathonly)) != 0)

run_res_trends_CAT <- ((1 %in% specieslist$rt) | (1 %in% restrictedspecieslist$rt)) &
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


# calculations: resolve trends ----------------------------------------------

if (run_res_trends == FALSE) {
  
  # list of columns that need to be created since we have skipped steps
  na_columns <- c("longtermlci", "longtermmean", "longtermrci",     
                  "currentslopelci", "currentslopemean", "currentsloperci", 
                  get_iucn_proj_cols())
  
  base = read.csv(base_path) %>% 
    # if full column has no X at all, gets read as NAs
    mutate(across(c(Long.Term.Analysis, Current.Analysis, Selected.SoIB),
                  ~ as.character(.))) %>%
    mutate(across(c(Long.Term.Analysis, Current.Analysis, Selected.SoIB),
                  ~ replace_na(., ""))) %>%
    dplyr::select(-SCIENTIFIC.NAME)
  
  main = read.csv("00_data/SoIB_mapping_2023.csv") %>% 
    left_join(base, by = c("eBird.English.Name.2023" = "COMMON.NAME"))
  
  # creating NA columns to match structure of "normal" main data
  main[, na_columns] <- NA_real_
  
  
  print(glue("Skipping resolving species trends for {cur_mask}"))
  
} else {
  
  # data processing and prep ------------------------------------------------
  
  base = read.csv(base_path) %>% 
    # if full column has no X at all, gets read as NAs
    mutate(across(c(Long.Term.Analysis, Current.Analysis, Selected.SoIB),
                  ~ as.character(.))) %>%
    mutate(across(c(Long.Term.Analysis, Current.Analysis, Selected.SoIB),
                  ~ replace_na(., ""))) %>%
    dplyr::select(-SCIENTIFIC.NAME)
  
  main = read.csv("00_data/SoIB_mapping_2023.csv") %>% 
    left_join(base, by = c("eBird.English.Name.2023" = "COMMON.NAME"))
  
  # separate object for sensitivity analysis
  sens = main %>% dplyr::select(eBird.English.Name.2023)
  
  
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
    pull(eBird.English.Name.2023)
  spec_ct <- main %>% 
    filter(Current.Analysis == "X") %>% 
    pull(eBird.English.Name.2023)
  
  # long-term (problematic species) ###
  
  trendsa = trends %>%
    filter(timegroups < soib_year_info("cat_start"),
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
      mutate(freq = case_when(timegroups < soib_year_info("cat_start") & 
                                COMMON.NAME %in% specs_lt_remove ~ NA,
                              TRUE ~ freq),
             se = case_when(timegroups < soib_year_info("cat_start") & 
                              COMMON.NAME %in% specs_lt_remove ~ NA,
                            TRUE ~ se))
    main <- main %>% 
      mutate(Long.Term.Analysis = if_else(eBird.English.Name.2023 %in% specs_lt_remove,
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
    filter(timegroups >= soib_year_info("cat_start"),
           COMMON.NAME %in% spec_ct) %>%
    group_by(sl, COMMON.NAME) %>%  
    # is any simulation of any species problematic: unable to calc. SE or SE > |mean|
    filter(any(is.na(se) | se > abs(freq) | 
                 # edge case of CWBB and Mangrove Pitta in PAs (freq very high, SE low)
                 (freq < -100 | freq > 100))) %>%
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
      mutate(Long.Term.Analysis = if_else(eBird.English.Name.2023 %in% specs_ct_remove,
                                          "", Long.Term.Analysis),
             Current.Analysis = if_else(eBird.English.Name.2023 %in% specs_ct_remove,
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
      # identifying species with less than 8/25 5 km cells with data within 25 km grids
      filter(!is.na(mean5km) & mean5km < 8 &
               (Long.Term.Analysis == "X" | Current.Analysis == "X") &
               is.na(Restricted.Islands)) %>% 
      pull(eBird.English.Name.2023)

    specsc2 = main %>%
      # identifying species where the variation of within 25 km cell sampling is too high
      filter(!is.na(ci5km) & (main$ci5km/main$mean5km) > 0.25 &
               (Long.Term.Analysis == "X" | Current.Analysis == "X") &
               is.na(Restricted.Islands)) %>% 
      pull(eBird.English.Name.2023)
    
    specsc = union(specsc1, specsc2)
    
    
    trends = trends %>%
      filter(!COMMON.NAME %in% specsc)
    
    main <- main %>% 
      mutate(Long.Term.Analysis = if_else(eBird.English.Name.2023 %in% specsc,
                                          "", Long.Term.Analysis),
             Current.Analysis = if_else(eBird.English.Name.2023 %in% specsc,
                                        "", Current.Analysis))
    
  }
  
  
  # 2. proportion of 25kmx25km cells sampled
  
  specsd3 = main %>%
    # identifying cells where on an average, less than 60 % of the 2022 coverage
    # was achieved during the last N CAT years
    filter(!is.na(proprange25km.current) & 
             (proprange25km.current/proprange25km.latestyear) < 0.6 &
             (Current.Analysis == "X")) %>% 
    pull(eBird.English.Name.2023)
  
  trends = trends %>%
    filter(!COMMON.NAME %in% specsd3)
  
  
  main <- main %>%
    # remove these species from both analyses, since unlikely that it qualifies for long-term
    # but not for current
    mutate(Long.Term.Analysis = if_else(eBird.English.Name.2023 %in% specsd3,
                                        "", Long.Term.Analysis),
           Current.Analysis = if_else(eBird.English.Name.2023 %in% specsd3,
                                      "", Current.Analysis))
  
  
  # rewriting selected species for LTT and CAT
  spec_lt = main %>% 
    filter(Long.Term.Analysis == "X") %>% 
    pull(eBird.English.Name.2023)
  
  spec_ct = main %>% 
    filter(Current.Analysis == "X") %>% 
    pull(eBird.English.Name.2023)
  
  
  # checkpoint-object "main"
  main1_postfilt <- main
  
  
  # calculations: prep --------------------------------------------------------
  
  # This section performs calculations on the trends data frame to derive new columns, 
  # such as lci, mean, and rci. It also creates a data frame called trends_framework to 
  # define the timegroups and species combinations. (Summary from ChatGPT)
  
  # Mean is calculated first, then CI. This is done for long-term and current 
  # trend separately.
  
  
  # Years to project for PJ's IUCN comparison
  extra.years = soib_year_info("iucn_projection")
  
  trends = trends %>%
    group_by(COMMON.NAME, timegroupsf, timegroups) %>% 
    reframe(mean_trans = mean(freq), 
            # adding se from variation between the means and propagated SE
            se_trans = sd(freq) + sqrt(sum(se^2)/n())) %>%
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
  
  if (run_res_trends_LTT == FALSE) {
    
    # list of columns that need to be created since we have skipped steps
    na_columns <- c("longtermlci", "longtermmean", "longtermrci")
    # creating NA columns to match structure of "normal" main data
    main[, na_columns] <- NA_real_
    
    modtrends <- trends %>% 
      dplyr::select(timegroupsf, timegroups, COMMON.NAME) %>% 
      mutate(lci_std = NA_real_, mean_std = NA_real_, rci_std = NA_real_)
    
    # checkpoint-object "main"
    main2_postLTT <- main
    
    print(glue("Skipping resolving species LTT for {cur_mask}"))
    
  } else {
    
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
    save(modtrends1, modtrends2, modtrends3, modtrends4, modtrends5, file = lttsens_path)
    
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
    
    # saving the values for final year in "main" as well:
    # temp object then left_join instead of right_join because species order in main
    # needs to be preserved
    temp <- modtrends %>%
      filter(timegroups == soib_year_info("latest_year")) %>%
      dplyr::select(COMMON.NAME, lci_std, mean_std, rci_std) %>%
      rename(longtermlci = lci_std,
             longtermmean = mean_std,
             longtermrci = rci_std)
    
    main <- main %>%
      left_join(temp, by = c("eBird.English.Name.2023" = "COMMON.NAME"))
    
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
    
  }

  # calculations: trends (current) ------------------------------------------
  
  if (run_res_trends_CAT == FALSE) {
    
    # list of columns that need to be created since we have skipped steps
    na_columns <- c("currentslopelci", "currentslopemean", "currentsloperci")
    # creating NA columns to match structure of "normal" main data
    main[, na_columns] <- NA_real_
    
    modtrends_recent <- trends %>% 
      filter(timegroups >= recentcutoff) %>% 
      dplyr::select(timegroupsf, timegroups, COMMON.NAME) %>% 
      mutate(lci_std_recent = NA_real_, mean_std_recent = NA_real_, rci_std_recent = NA_real_)
    
    ext_trends <- trends %>% 
      filter(timegroups >= recentcutoff) %>% 
      group_by(COMMON.NAME) %>%
      reframe(timegroups = extra.years) %>% 
      mutate(lci_ext_std = NA_real_, mean_ext_std = NA_real_, rci_ext_std = NA_real_)
    
    # checkpoint-object "main"
    main3_postCATmain <- main
    
    print(glue("Skipping resolving species CAT for {cur_mask}"))
    
  } else {
    
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
      left_join(sl_data_main, by = c("eBird.English.Name.2023" = "COMMON.NAME"))
    
    
    # checkpoint-object "main"
    main3_postCATmain <- main
    
    # calculations: trends (current): SENS ------------------------------------
    
    # here, fitting the same linear model as for main trend, but for data in which
    # one year is dropped each time.
    # this is to see how much each year affects the estimate.
    sl_data_sens <- temp_sims %>%
      group_by(COMMON.NAME, sim) %>%
      group_modify(~ {
        
        temp_df <- .x # to refer inside the second purrr function below
        
        .x %>%
          reframe(
            
            # produce as many iterations as no. of CAT years (that many sl and slse cols)
            !!!imap(soib_year_info("cat_years"), ~ {
              
              modelfit <- lm(val_sample ~ timegroups,
                             # one year dropped
                             data = temp_df[temp_df$timegroups != soib_year_info("cat_years")[.y],])
              
              pred <- predict(
                modelfit, se = TRUE,
                # one year dropped
                newdata = data.frame(
                  timegroups = temp_df$timegroups[temp_df$timegroups != soib_year_info("cat_years")[.y]]
                )
              )
              
              num <- pred$fit[2] - pred$fit[1]
              den <- abs(pred$fit[1])
              numse <- sqrt(pred$se.fit[1]^2 + pred$se.fit[2]^2)
              dense <- pred$se.fit[1]
              
              # create col names based on iteration index
              col_name_sl <- paste0("sl", .y)
              col_name_slse <- paste0("slse", .y)
              
              tibble(
                !!col_name_sl := 100 * errordiv(num, den, numse, dense)[1] %>% as.numeric(),
                !!col_name_slse := errordiv(num, den, numse, dense)[2] %>% as.numeric()
              )
              
            }) %>% 
              bind_cols()
            
          )
        
      }) %>%
      
      # mean and SE of slope
      group_by(COMMON.NAME) %>%
      reframe(across(
        
        .cols = matches("^sl\\d+$"), # "sl"s but not "slse"s
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
      left_join(sl_data_sens, by = c("eBird.English.Name.2023" = "COMMON.NAME"))
    
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
    
  }
  
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
    # replacing NAs
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
  
  
  # in an interannual update, we want to rename the old trends.csv
  # to archive it for future/downstream use while latest will be saved as trends.csv
  
  if (interannual_update == TRUE) {
    
    trends_cur_end_year <- trends %>% 
      filter(!is.na(mean)) %>% # future year rows will have values in some cols but not "mean"
      distinct(timegroups) %>% 
      max()
    trends_prev_end_year <- read.csv(trends_outpath) %>% 
      filter(!is.na(mean)) %>% # future year rows will have values in some cols but not "mean"
      distinct(timegroups) %>% 
      max()
    
    # rename old trends file
    if (trends_cur_end_year != trends_prev_end_year) {
      trends_outpath_old <- glue("{str_remove(trends_outpath, '.csv')}_MY{trends_prev_end_year}.csv")
      file.rename(trends_outpath, trends_outpath_old)
    }
    
  }
  
  # save current trends to file
  write.csv(trends, file = trends_outpath, row.names = F)


  # joining future projected trends to main dataframe ###

  tojoin <- map(extra.years, ~ trends %>%
                  filter(timegroups == .x) %>%
                  dplyr::select(COMMON.NAME, lci_comb_std, mean_comb_std, rci_comb_std) %>%
                  magrittr::set_colnames(c("eBird.English.Name.2023",
                                           glue("proj{.x}.lci"),
                                           glue("proj{.x}.mean"),
                                           glue("proj{.x}.rci")))) %>%
    reduce(full_join, by = "eBird.English.Name.2023")

  main <- main %>%
    left_join(tojoin) %>%
    # removing misIDd species "selection" for long-term and current analyses
    mutate(Long.Term.Analysis = ifelse(eBird.English.Name.2023 %in% spec_misid,
                                       "", Long.Term.Analysis),
           Current.Analysis = ifelse(eBird.English.Name.2023 %in% spec_misid,
                                     "", Current.Analysis))


  # checkpoint-object "main"
  main4_postCATcomb <- main


}

# saving info for trends columns
write.csv(main, file = mainwocats_path, row.names = F)

# calculations: resolve occupancy ----------------------------------------------

tic("Calculating occupancy")

# the if TRUE steps happen later, in classify_and_summarise.R, since that requires 
# to pull in columns from already-resolved full-country file

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
    # calculating expected occupancy by multiplying the occupancy value by the
    # area of the cell
    reframe(occ = sum(occupancy * area),
            occ.ci = round((erroradd(se * area)) * 1.96))

  est = array(data = NA,
              dim = c(length(main$eBird.English.Name.2023), 2),
              dimnames = list(main$eBird.English.Name.2023, c("occ", "occ.ci")))


  # determining which range size value to use for each species based on the range size
  # estimated for each region

  for (i in main$eBird.English.Name.2023)
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


  tojoin = data.frame(eBird.English.Name.2023 = rownames(est)) %>%
    mutate(rangemean = round(as.numeric(est[, 1]), 0),
           rangeci = round(as.numeric(est[, 2]), 0)) %>%
    mutate(rangelci = rangemean - rangeci,
           rangerci = rangemean + rangeci,
           rangeci = NULL) %>%
    mutate(rangemean = case_when(is.na(rangemean) &
                                   eBird.English.Name.2023 %in% specieslist$COMMON.NAME ~ 0,
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


