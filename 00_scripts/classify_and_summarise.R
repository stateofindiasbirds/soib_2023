library(tidyverse)
library(glue)
library(tictoc)
library(writexl)

load("00_data/analyses_metadata.RData")


# setup -------------------------------------------------------------------

# preparing data for specific mask (this is the only part that changes, but automatically)
cur_metadata <- analyses_metadata %>% filter(MASK == cur_mask)

# read paths
speclist_path <- cur_metadata$SPECLISTDATA.PATH
trends_pathonly <- cur_metadata$TRENDS.PATHONLY

# write paths
lttsens_path <- cur_metadata$LTTSENS.PATH
cursens_path <- cur_metadata$CURSENS.PATH 

mainwocats_path <- cur_metadata$SOIBMAIN.WOCATS.PATH
main_path <- cur_metadata$SOIBMAIN.PATH
summaries_path <- cur_metadata$SUMMARY.PATH

###

source('00_scripts/00_functions.R')

# for classification
priorityrules = read.csv("00_data/priorityclassificationrules.csv") 

load(speclist_path)
load("00_data/vagrantdata.RData")

main = read.csv(mainwocats_path) %>%
  # if full column has no X at all, gets read as NAs
  mutate(across(c(Long.Term.Analysis, Current.Analysis, Selected.SOIB),
                ~ as.character(.))) %>%
  mutate(across(c(Long.Term.Analysis, Current.Analysis, Selected.SOIB),
                ~ replace_na(., "")))


### for conditionals ###

# don't run resolve_species_trends (and its sens check) if no species selected
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
}

###

# calculations: resolve occupancy for masks where skip_res_occu == TRUE -------

# for masks where resolve_occupancy not run (habitat/CA masks),
# need to pull in range size info from full country (after it has been resolved, i.e.)

if (skip_res_occu == TRUE) {
  
  # take relevant columns from wocats file of full-country
  tojoin <- read.csv("01_analyses_full/results/SoIB_main_wocats.csv") %>%
    distinct(eBird.English.Name.2022, rangelci, rangemean, rangerci)
  
  # joining to main object
  main <- main %>% left_join(tojoin)
  
  # checkpoint-object "main"
  main5_postoccu <- main
  
  write.csv(main, file = mainwocats_path, row.names = F)
  
}

# classification: assign SoIB Status categories ------------------------------

# any vagrant reported recently
spec_vagrants <- d %>%
  filter(year > 2017) %>%
  distinct(COMMON.NAME) %>%
  pull(COMMON.NAME)

# classifying into SoIB Status for long-term and current trends and range

# taking upper limit of CI for declines, and lower limit for increases

main = main %>%
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
  
  main_order = main %>% select(eBird.English.Name.2022)
  main <- main_order %>% 
    left_join(bind_rows(main_tokeep, main_update))
  
  rm(main_tokeep, main_toupdate, main_nat, main_update, main_order)
  
}


# classification: adjust SoIB Status based on sensitivity for trends ----------------

if (run_res_trends == TRUE) {

  if (run_res_trends_LTT == TRUE) {
    
    # sensitivity check for long-term trends ###
    
    load(lttsens_path)
    
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
    
  } else {
    print(glue("Skipping sensitivity-check-based adjustments to Trend Status (LTT) for {cur_mask} (LTT)"))
  }
  
  
  if (run_res_trends_CAT == TRUE) {
    
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
      
    ))
  
  } else {
    print(glue("Skipping sensitivity-check-based adjustments to Trend Status (CAT) for {cur_mask}"))
  }
  
} else {
  
  print(glue("Skipping sensitivity-check-based adjustments to Trend Status for {cur_mask}"))
  
}


# classification: converting all non-selected to NA -----------------------

main <- main %>%
  mutate(across(c(SOIBv2.Long.Term.Status, SOIBv2.Current.Status, SOIB.Range.Status),
                ~ if_else(Selected.SOIB != "X", NA_character_, .)))

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

###

# some species change Trend Status categories in each iteration, so we fix it to
# values at time of printing SoIB 2023

if (cur_mask == "none") {
  
  # 13 columns from main file written at time of printing SoIB 2023
  # no. of Status changes: 14 LTT, 20 CAT
  main_repair = read.csv("01_analyses_full/results/print_fix.csv")
  
  main <- main %>% 
    dplyr::select(-c("longtermlci", "longtermmean", "longtermrci",
                     "currentslopelci", "currentslopemean", "currentsloperci",
                     "rangelci", "rangemean", "rangerci",
                     "SOIBv2.Long.Term.Status", "SOIBv2.Current.Status", "SOIBv2.Range.Status",
                     "SOIBv2.Priority.Status")) %>% 
    left_join(main_repair)
  
}

###

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
  filter(`Trend Status` == "Trend Inconclusive") %>%
  mutate(across(everything(), ~ replace_na(., 0)))

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
  mutate(across(everything(), ~ replace_na(., 0))) %>%
  # percentages
  mutate(`Long-term species conclusive (perc.)` = case_when(
    `Trend Status` %in% cats_uncertain ~ NA_real_,
    nspec_trend_conc_ltt == 0 ~ NA_real_,
    TRUE ~ round(100 * (`Long-term species (no.)` / nspec_trend_conc_ltt), 1)
  ),
  `Current species conclusive (perc.)` = case_when(
    `Trend Status` %in% cats_uncertain ~ NA_real_,
    nspec_trend_conc_cat == 0 ~ NA_real_,
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
  mutate(across(everything(), ~ replace_na(., 0))) %>%
  mutate(`Species (perc.)` = round(100 * (`Species (no.)` / sum(`Species (no.)`)), 1)) %>%
  mutate(across(everything(), ~ replace(., is.nan(.), NA_real_)))

status_priority <- main %>%
  filter(!is.na(SOIBv2.Priority.Status)) %>% # count() counts NA also
  mutate(SOIBv2.Priority.Status = factor(SOIBv2.Priority.Status,
                                         levels = c("High", "Moderate", "Low"))) %>%
  count(SOIBv2.Priority.Status) %>%
  complete(SOIBv2.Priority.Status, fill = list(n = 0)) %>%
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
