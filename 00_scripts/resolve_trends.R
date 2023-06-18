# preparing data for specific mask (this is the only part that changes, but automatically)
cur_metadata <- analyses_metadata %>% filter(MASK == cur_mask)
base_path <- cur_metadata$FULLSPECLIST.PATH
trends_pathonly <- cur_metadata$TRENDS.PATHONLY

###

library(tidyverse)
library(VGAM)

source('00_scripts/00_functions.R')

recentcutoff = 2015


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
  mutate(m1 = first(mean_trans),
         m2 = first(mean),
         s1 = first(se_trans)) %>% 
  ungroup() %>% 
  # for calculating change in abundance index (as % change)
  mutate(mean_std = 100*mean/m2)

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
         m2 = first(mean),
         s1 = first(se_trans)) %>% 
  ungroup() %>% 
  # for calculating change in abundance index (as % change)
  mutate(mean_std_recent = 100*mean/m2)

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
  group_by(COMMON.NAME) %>%
  reframe(mean.slope1 = mean(sl1),
          se.slope1 = sd(sl1) + sqrt(sum(slse1^2)/length(slse1)),
          mean.slope2 = mean(sl2),
          se.slope2 = sd(sl2) + sqrt(sum(slse2^2)/length(slse2)),
          mean.slope3 = mean(sl3),
          se.slope3 = sd(sl3) + sqrt(sum(slse3^2)/length(slse1)),
          mean.slope4 = mean(sl4),
          se.slope4 = sd(sl4) + sqrt(sum(slse4^2)/length(slse1)),
          mean.slope5 = mean(sl5),
          se.slope5 = sd(sl5) + sqrt(sum(slse5^2)/length(slse1)),
          mean.slope6 = mean(sl6),
          se.slope6 = sd(sl6) + sqrt(sum(slse6^2)/length(slse1)),
          mean.slope7 = mean(sl7),
          se.slope7 = sd(sl7) + sqrt(sum(slse7^2)/length(slse1)),
          mean.slope8 = mean(sl8),
          se.slope8 = sd(sl8) + sqrt(sum(slse8^2)/length(slse1))) %>%
  group_by(COMMON.NAME) %>%
  reframe(currentslopelci1 = mean.slope1 - 1.96*se.slope1,
          currentslopemean1 = mean.slope1,
          currentsloperci1 = mean.slope1 + 1.96*se.slope1,
          currentslopelci2 = mean.slope2 - 1.96*se.slope2,
          currentslopemean2 = mean.slope2,
          currentsloperci2 = mean.slope2 + 1.96*se.slope2,
          currentslopelci3 = mean.slope3 - 1.96*se.slope3,
          currentslopemean3 = mean.slope3,
          currentsloperci3 = mean.slope3 + 1.96*se.slope3,
          currentslopelci4 = mean.slope4 - 1.96*se.slope4,
          currentslopemean4 = mean.slope4,
          currentsloperci4 = mean.slope4 + 1.96*se.slope4,
          currentslopelci5 = mean.slope5 - 1.96*se.slope5,
          currentslopemean5 = mean.slope5,
          currentsloperci5 = mean.slope5 + 1.96*se.slope5,
          currentslopelci6 = mean.slope6 - 1.96*se.slope6,
          currentslopemean6 = mean.slope6,
          currentsloperci6 = mean.slope6 + 1.96*se.slope6,
          currentslopelci7 = mean.slope7 - 1.96*se.slope7,
          currentslopemean7 = mean.slope7,
          currentsloperci7 = mean.slope7 + 1.96*se.slope7,
          currentslopelci8 = mean.slope8 - 1.96*se.slope8,
          currentslopemean8 = mean.slope8,
          currentsloperci8 = mean.slope8 + 1.96*se.slope8)

# joining to data object
sens <- sens %>%
  left_join(sl_data_sens, by = c("eBird.English.Name.2022" = "COMMON.NAME"))

write.csv(sens, "trends_results/full_results/current_sensitivity.csv", row.names = F)


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
  dplyr::select(COMMON.NAME, timegroupsf, timegroups, mean_trans, se_trans,
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
                                  TRUE ~ lci_comb_std))

write.csv(trends, "01_analyses_full/trends.csv", row.names = F)



# joining future projected trends to main dataframe ###

tojoin <- map(2023:2029, ~ trends %>%
           filter(timegroups == .x) %>%
           dplyr::select(COMMON.NAME, lci_comb_std, mean_comb_std, rci_comb_std) %>%
           magrittr::set_colnames(c("eBird.English.Name.2022", 
                                    glue("proj{.x}.lci"),
                                    glue("proj{.x}.mean"),
                                    glue("proj{.x}.rci")))) %>% 
  reduce(full_join, by = "eBird.English.Name.2022")

main <- main %>% left_join(tojoin)

write.csv(main, "trends_results/full_results/SoIB_main_wocats.csv", row.names = F)


# calculations: assign SoIB categories to trends and occupancy (w/ sensitivity analysis) ----

main = read.csv("trends_results/full_results/SoIB_main_wocats.csv")

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


# sensitivity check ###

# changes classifications based on sensitivity analyses ???

sens = read.csv("trends_results/full_results/current_sensitivity.csv")

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


priorityrules = read.csv("00_data/priorityclassificationrules.csv")
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

write.csv(main,"01_analyses_full/SoIB_main.csv",row.names=F)


# calculations: small investigations --------------------------------------


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

write.csv(summary_status,"trends_results/full_results/summary_status.csv",row.names=F)
write.csv(priority_summary,"trends_results/full_results/priority_status.csv",row.names=F)
write.csv(species_summary,"trends_results/full_results/species_status.csv",row.names=F)
