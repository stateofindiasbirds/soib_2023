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
summary_path <- cur_metadata$SUMMARY.PATH
priority_path <- cur_metadata$PRIORITY.PATH
specsum_path <- cur_metadata$SPECSUM.PATH

###

library(tidyverse)
library(VGAM)
library(sf)

source('00_scripts/00_functions.R')
load("00_data/spec_misid.RData") # to remove from LTT and CAT "selection" later

recentcutoff = 2015

###

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
  dplyr::select(eBird.English.Name.2022) %>% 
  as.vector() %>% list_c()
spec_ct <- main %>% 
  filter(Current.Analysis == "X") %>% 
  dplyr::select(eBird.English.Name.2022) %>% 
  as.vector() %>% list_c()

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
    dplyr::select(COMMON.NAME) %>% 
    as.vector() %>% list_c()
    
  specs_lt_remove_part = tab_lt_rem %>% 
    filter(count < round(totsims/2)) %>% 
    dplyr::select(COMMON.NAME) %>% 
    as.vector() %>% list_c()
  

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
    dplyr::select(COMMON.NAME) %>% 
    as.vector() %>% list_c()
  
  specs_ct_remove_part = tab_ct_rem %>% 
    filter(count < round(totsims/2)) %>% 
    dplyr::select(COMMON.NAME) %>% 
    as.vector() %>% list_c()


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
             !Endemic.Region %in% c("Andaman and Nicobar Islands",
                                    "Andaman Islands",
                                    "Nicobar Islands")) %>% 
    dplyr::select(eBird.English.Name.2022) %>% 
    as.vector() %>% list_c()
  
  specsc2 = main %>%
    # <annotation_pending_AV> rationale for 0.25 (even if arbitrary, what does it mean?)
    filter(!is.na(ci5km) & (main$ci5km/main$mean5km) > 0.25 &
             (Long.Term.Analysis == "X" | Current.Analysis == "X") &
             !Endemic.Region %in% c("Andaman and Nicobar Islands",
                                    "Andaman Islands",
                                    "Nicobar Islands")) %>% 
    dplyr::select(eBird.English.Name.2022) %>% 
    as.vector() %>% list_c()
  
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

specsd1 = main %>%
  # <annotation_pending_AV> rationale for 0.075 (even if arbitrary, what does it mean?)
  filter(!is.na(proprange25km2000) & 
           (proprange25km2000/proprange25km2022) < 0.075 &
           Long.Term.Analysis == "X") %>% 
  dplyr::select(eBird.English.Name.2022) %>% 
  as.vector() %>% list_c()

specsd2 = main %>%
  # <annotation_pending_AV> rationale for 0.04 (even if arbitrary, what does it mean?)
  filter(!is.na(proprange25km2000) & 
           main$proprange25km2000 < 0.04 &
           Long.Term.Analysis == "X") %>% 
  dplyr::select(eBird.English.Name.2022) %>% 
  as.vector() %>% list_c()

specsd = union(specsd1, specsd2)


trends = trends %>%
  mutate(freq = case_when(timegroups < 2015 & COMMON.NAME %in% specsd ~ NA,
                          TRUE ~ freq),
         se = case_when(timegroups < 2015 & COMMON.NAME %in% specsd ~ NA,
                        TRUE ~ se))
main <- main %>% 
  mutate(Long.Term.Analysis = if_else(eBird.English.Name.2022 %in% specsd,
                                      "", Long.Term.Analysis))


# <annotation_pending_AV> this isn't used anywhere, can be removed?
specsd3 = main %>%
  # <annotation_pending_AV> rationale for 0.6 (even if arbitrary, what does it mean?)
  filter(!is.na(proprange25km.current) & 
           (proprange25km.current/proprange25km2022) < 0.6 &
           (Current.Analysis == "X")) %>% 
  dplyr::select(eBird.English.Name.2022) %>% 
  as.vector() %>% list_c()

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
  dplyr::select(eBird.English.Name.2022) %>% 
  as.character()

spec_ct = main %>% 
  filter(Current.Analysis == "X") %>% 
  dplyr::select(eBird.English.Name.2022) %>% 
  as.character()


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

set.seed(10) # for simulations
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
  mutate(Long.Term.Analysis = case_when(COMMON.NAME %in% spec_misid ~ NA_real_,
                                        TRUE ~ Long.Term.Analysis),
         Current.Analysis = case_when(COMMON.NAME %in% spec_misid ~ NA_real_,
                                      TRUE ~ Current.Analysis))  


# calculations: occupancy -------------------------------------------------

load("00_data/maps_sf.RData")
load(speclist_path)


# occupancy-model files
occu_model <- list.files(path = occu_mod_pathonly, full.names = T) %>% 
  map_df(read.csv) %>%
  dplyr::select(-area) %>% # remove this later ###
  rename(gridg1 = gridg)

# occupancy-presence files
occu_presence <- list.files(path = occu_pres_pathonly, full.names = T) %>% 
  map_df(read.csv) %>% 
  rename(presence = actual) # remove this later (will be named "presence" already) ###

# taking modelled occupancy values for species in cell where "absent"
occ.full1 = occu_model %>% 
  left_join(occu_presence) %>% 
  filter(is.na(presence)) 

# "presences"
occ.full2 = occu_presence %>% 
  left_join(occu_model) %>% 
  dplyr::select(names(occ.full1))


occu_full = rbind(occ.full1, occ.full2) %>% 
  mutate(gridg1 = as.character(gridg1)) %>% 
  # joining areas of each grid cell
  left_join(g1_in_sf %>% 
              st_drop_geometry() %>% 
              transmute(gridg1 = GRID.G1, area = AREA.G1))

#   # when present but model values also exist, assume full occupancy
#   mutate(occupancy = if_else(presence == 1, 1, occupancy),
#          se = if_else(presence == 1, 0, se)) %>% 
#   filter(!is.na(occupancy), !is.na(se), !is.na(gridg1)) %>% 
#   mutate(occupancy = if_else(nb == 0 & occupancy != 1, 0, occupancy),
#          se = if_else(nb == 0 & occupancy != 1, 0, se))

occu_full$occupancy[occu_full$presence == 1] = 1
occu_full$se[occu_full$presence == 1] = 0
occu_full = occu_full %>% filter(!is.na(occupancy), !is.na(se), !is.na(gridg1))
occu_full$occupancy[occu_full$nb == 0 & occu_full$occupancy != 1] = 0
occu_full$se[occu_full$nb == 0 & occu_full$occupancy != 1] = 0


occu_summary = occu_full %>%
  group_by(COMMON.NAME, status) %>% 
  reframe(occ = sum(occupancy*area),
          occ.ci = round((erroradd(se*area))*1.96))

est = array(data = NA, 
            dim = c(length(main$eBird.English.Name.2022), 2),
            dimnames = list(main$eBird.English.Name.2022, c("occ", "occ.ci")))



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
  
  for (j in 1:l)
  {
    flag = 0
    
    if (cur_occu_summary$status[j] %in% c("MP") & is.na(est[i,"occ"]))
    {
      est[i,"occ"] = cur_occu_summary$occ[j]
      est[i,"occ.ci"] = cur_occu_summary$occ.ci[j]
      flag = 1
    }
    
    if (cur_occu_summary$status[j] %in% c("R","MS"))
    {
      est[i,"occ"] = cur_occu_summary$occ[j]
      est[i,"occ.ci"] = cur_occu_summary$occ.ci[j]
    }
    
    if (cur_occu_summary$status[j] %in% c("M","MW") & (is.na(est[i,"occ"]) | flag == 1))
    {
      est[i,"occ"] = cur_occu_summary$occ[j]
      est[i,"occ.ci"] = cur_occu_summary$occ.ci[j]
    }
    
  }
}


tojoin = data.frame(rep(rownames(est))) %>% 
  magrittr::set_colnames("eBird.English.Name.2022") 

tojoin$rangelci = round(as.numeric(est[,1])/10000,3) - round(as.numeric(est[,2])/10000,3)
tojoin$rangemean = round(as.numeric(est[,1])/10000,3)
tojoin$rangerci = round(as.numeric(est[,1])/10000,3) + round(as.numeric(est[,2])/10000,3)

tojoin$rangemean[(tojoin$eBird.English.Name.2022 %in% specieslist$COMMON.NAME) & is.na(tojoin$rangemean)] = 0
tojoin$rangelci[tojoin$rangemean == 0] = 0
tojoin$rangerci[tojoin$rangemean == 0] = 0


# joining to main object
main <- main %>% left_join(tojoin)

write.csv(main, file = mainwocats_path, row.names = F)


# classification: assign SoIB Status categories (w/ sensitivity analysis) ----

# classifying into SoIB Status for long-term and current trends and range

# taking upper limit of CI for declines, and lower limit for increases

main = read.csv(mainwocats_path) %>%
  mutate(
    
    SOIBv2.Long.Term.Status = case_when(
      is.na(longtermmean) ~ "eBird Data Deficient",
      (longtermrci-longtermmean)/longtermmean > 0.5 ~ "eBird Data Inconclusive", # arbitrary
      # else
      # for declines
      longtermrci <= 50 ~ "Rapid Decline", # -100% to -50%
      longtermrci > 50 & longtermrci <= 75 ~ "Decline", # -50% to -25%
      # for increases
      longtermlci >= 150 ~ "Rapid Increase", # +50% to inf
      longtermlci < 150 & longtermlci >= 125 ~ "Increase", # +25% to +50%
      # stable vs inconclusive:
      # if CI is completely below or above the baseline, can't be stable
      longtermlci > 100 | longtermrci < 100 ~ "eBird Data Inconclusive",
      # if one limit is in the Stable zone but other limit passes to Rapid X, can't be stable
      longtermlci <= 50 | longtermrci >= 150 ~ "eBird Data Inconclusive",
      TRUE ~ "Stable"
      # OR:
      # (longtermlci > 50 & longtermlci <= 100) &
      #   (longtermrci >= 100 & longtermrci < 150) ~ "Stable",
      # TRUE ~ "eBird Data Inconclusive"
    ),
    
    SOIBv2.Current.Status = case_when(
      is.na(currentslopemean) ~ "eBird Data Deficient",
      (currentsloperci-currentslopelci) > 6 ~ "eBird Data Inconclusive", # arbitrary
      # <annotation_pending_AV> decline and increase values?
      # decreases
      currentsloperci <= -2.7 ~ "Rapid Decline",
      currentsloperci > -2.7 & currentsloperci <= -1.1 ~ "Decline",
      # increases
      currentslopelci >= 1.6 ~ "Rapid Increase",
      currentslopelci < 1.6 & currentslopelci >= 0.9 ~ "Increase",
      # if slope with SE is fully positive or negative, can't be stable
      currentsloperci < 0 | currentslopelci > 0 ~ "eBird Data Inconclusive",
      TRUE ~ "Stable"
      ),
    
    SOIBv2.Range.Status = case_when(
      is.na(rangemean) ~ NA_character_,
      rangemean == 0 ~ "Historical",
      rangerci < 0.0625 ~ "Very Restricted",
      # larger threshold for species that are not island endemics
      (!Endemic.Region %in% c("Andaman and Nicobar Islands", "Andaman Islands", "Nicobar Islands") &
         rangerci < 0.75) ~ "Very Restricted",
      rangerci < 4.25 ~ "Restricted",
      rangelci > 100 ~ "Very Large",
      rangelci > 25 ~ "Large",
      TRUE ~ "Moderate"
    )
    
  )


# sensitivity check ###

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
      
      is.na(currentslopemean) ~ "eBird Data Deficient",
      (currentsloperci - currentslopelci) > 6 ~ "eBird Data Inconclusive",
      currentsloperci <= -2.7 ~ "Rapid Decline",
      currentsloperci <= -1.1 ~ "Decline",
      currentslopelci >= 1.6 ~ "Rapid Increase",
      currentslopelci >= 0.9 ~ "Increase",
      currentsloperci < 0 ~ "eBird Data Inconclusive",
      currentslopelci > 0 ~ "eBird Data Inconclusive",
      TRUE ~ "Stable"
      
    )) %>%
    dplyr::select(eBird.English.Name.2022, SOIBv2.Current.Status.Sens) %>% 
    magrittr::set_colnames(c("eBird.English.Name.2022", glue("s{.x}")))
  
}) %>%
  reduce(full_join) 

sens_cat <- main %>% 
  dplyr::select(eBird.English.Name.2022, SOIBv2.Current.Status) %>% 
  left_join(sens_cat) %>%
  filter(!SOIBv2.Current.Status %in% c("eBird Data Deficient", "eBird Data Inconclusive"))


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
    if (length(unique(categs)) == 2 & "eBird Data Inconclusive" %in% categs) 
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
    if (length(categs[categs == "eBird Data Inconclusive"]) >= 4)
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
    
    eBird.English.Name.2022 %in% spec_ind.rem ~ "eBird Data Inconclusive",
    eBird.English.Name.2022 %in% spec_ind6 ~ "Decline",
    eBird.English.Name.2022 %in% spec_ind7 ~ "Increase",
    TRUE ~ SOIBv2.Current.Status
    
  )) %>% 
  mutate(across(c(SOIBv2.Long.Term.Status, SOIBv2.Current.Status, SOIB.Range.Status),
                ~ if_else(Selected.SOIB != "X", NA_character_, .)))


# classification: assign SoIB Priority status (based on trends and occupancy) -----

cats_trend = c("Rapid Decline", "Decline", "eBird Data Deficient", 
               "eBird Data Inconclusive", "Stable", "Increase", "Rapid Increase")
cats_range = c("Historical", "Very Restricted", "Restricted", 
               "Moderate", "Large", "Very Large")

cats_decline = c("Decline", "Rapid Decline")
cats_uncertain = c("eBird Data Deficient", "eBird Data Inconclusive")
cats_restricted = c("Historical", "Very Restricted", "Restricted")


priorityrules = read.csv("00_data/priorityclassificationrules.csv")


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

write.csv(main, file = main_path, row.names = F)


# summaries -------------------------------------------------------------------

summary_status = data.frame(Category = cats_trend) %>% 
  left_join(main %>% 
              count(SOIBv2.Long.Term.Status) %>% 
              magrittr::set_colnames(c("Category", "Long.Term"))) %>% 
  left_join(main %>% 
              count(SOIBv2.Current.Status) %>% 
              magrittr::set_colnames(c("Category", "Current"))) %>% 
  left_join(main %>% 
              count(SOIBv2.Range.Status) %>% 
              magrittr::set_colnames(c("Category", "Range")))

priority_summary = main %>% 
  filter(!is.na(SOIBv2.Priority.Status)) %>% # count() counts NA also
  count(SOIBv2.Priority.Status) %>% 
  magrittr::set_colnames(c("Category","N.species"))

species_summary <- main %>%
  summarise(across(c(Selected.SOIB, Long.Term.Analysis, Current.Analysis),
                   # adds up cases where condition is true
                   ~ sum(. == "X"))) %>% 
  magrittr::set_colnames(c("Selected for SoIB","Long-term Analysis","Current Analysis")) %>% 
  pivot_longer(everything(), names_to = "Category", values_to = "N.species")


write.csv(summary_status, file = summary_path, row.names = F)
write.csv(priority_summary, file = priority_path, row.names = F)
write.csv(species_summary, file = specsum_path, row.names = F)
