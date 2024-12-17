library(tidyverse)
library(glue)
library(tictoc)
library(writexl)


# setup -------------------------------------------------------------------

# preparing data for specific mask (this is the only part that changes, but automatically)
cur_metadata <- get_metadata(cur_mask)

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
  mutate(across(c(Long.Term.Analysis, Current.Analysis, Selected.SoIB),
                ~ as.character(.))) %>%
  mutate(across(c(Long.Term.Analysis, Current.Analysis, Selected.SoIB),
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
  tojoin <- get_metadata("none")$SOIBMAIN.WOCATS.PATH %>%
    distinct(eBird.English.Name.2023, rangelci, rangemean, rangerci)
  
  # joining to main object
  main <- main %>% left_join(tojoin)
  
  # checkpoint-object "main"
  main5_postoccu <- main
  
  write.csv(main, file = mainwocats_path, row.names = F)
  
}

# classification: assign SoIB Status categories ------------------------------

# any vagrant reported recently
spec_vagrants <- d %>%
  filter(year > (soib_year_info("latest_year") - 5)) %>%
  distinct(COMMON.NAME) %>%
  pull(COMMON.NAME)

# classifying into SoIB Status for long-term and current trends and range

# taking upper limit of CI for declines, and lower limit for increases

main = main %>%
  mutate(
    
    SoIB.Latest.Long.Term.Status = case_when(
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
    
    SoIB.Latest.Current.Status = case_when(
      is.na(currentslopemean) ~ "Insufficient Data",
      (currentsloperci-currentslopelci) > 6 ~ "Trend Inconclusive", # arbitrary
      # decline and increase threshold values
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
    
    SoIB.Latest.Range.Status = case_when(
      is.na(rangemean) ~ NA_character_,
      rangemean == 0 & !(eBird.English.Name.2023 %in% spec_vagrants) ~ "Historical",
      # above is to prevent species that are not historical but classified as vagrants
      # from being classified as Historical (instead, Very Restricted)
      rangerci < 625 ~ "Very Restricted",
      # larger threshold for species that are not island endemics
      (is.na(Restricted.Islands) & rangerci < 7500) ~ "Very Restricted",
      rangerci < 42500 ~ "Restricted",
      rangelci > 1000000 ~ "Very Large",
      rangelci > 250000 ~ "Large",
      TRUE ~ "Moderate"
    )
    
  )


# for states, due to obviously smaller ranges within states, Range Status categories
# lose meaning. Only Historical is meaningful, and we want to capture this information.
# so, for states we retain NA and Historical classifications, but others are reverted to
# full-country Range Status categories

###
# we have decided to abandon this entirely: non-country levels are not going to
# have Range Status assigned. So, this part needs to be removed later.
###

if (cur_metadata$MASK.TYPE == "state") {
  
  main_tokeep <- main %>% filter(is.na(SoIB.Latest.Range.Status) | SoIB.Latest.Range.Status == "Historical")
  
  main_toupdate <- anti_join(main, main_tokeep) %>% dplyr::select(-SoIB.Latest.Range.Status)
  
  main_nat <- get_metadata("none") %>%
    pull(SOIBMAIN.PATH) %>%
    read.csv() %>%
    distinct(eBird.English.Name.2023, SoIB.Latest.Range.Status)
  
  main_update <- left_join(main_toupdate, main_nat)
  
  main_order = main %>% select(eBird.English.Name.2023)
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
      dplyr::select(eBird.English.Name.2023, SoIB.Latest.Long.Term.Status) %>%
      # the modtrendsN files only have species for which we have run LTT
      filter(!is.na(SoIB.Latest.Long.Term.Status),
             SoIB.Latest.Long.Term.Status != "Insufficient Data") %>%
      rename(COMMON.NAME = eBird.English.Name.2023) %>%
      bind_rows(modtrends1, modtrends2, modtrends3, modtrends4, modtrends5) %>%
      group_by(COMMON.NAME) %>%
      # how many different status categories have been assigned?
      reframe(NO.STATUS = n_distinct(SoIB.Latest.Long.Term.Status),
              
              CONSERVATIVE.STATUS = case_when(
                
                NO.STATUS > 2 ~ "Trend Inconclusive",
                
                "Trend Inconclusive" %in% SoIB.Latest.Long.Term.Status ~ "Trend Inconclusive",
                
                NO.STATUS == 2 &
                  # if all Status assignments are either of the two increases
                  ("Rapid Increase" %in% SoIB.Latest.Long.Term.Status &
                     "Increase" %in% SoIB.Latest.Long.Term.Status) ~ "Increase",
                NO.STATUS == 2 &
                  # if all Status assignments are either of the two decreases
                  ("Rapid Decline" %in% SoIB.Latest.Long.Term.Status &
                     "Decline" %in% SoIB.Latest.Long.Term.Status) ~ "Decline",
                
                NO.STATUS == 2 ~ "Trend Inconclusive",
                TRUE ~ min(SoIB.Latest.Long.Term.Status)
                
              )) %>%
      mutate(ROBUST = if_else(NO.STATUS == 1, 1, 0)) %>%
      dplyr::select(-NO.STATUS)
    
    
    main <- main %>%
      left_join(sens_ltt, by = c("eBird.English.Name.2023" = "COMMON.NAME")) %>%
      # if Status assignment is not robust, take the most conservative one
      mutate(SoIB.Latest.Long.Term.Status = case_when(ROBUST == 0 ~ CONSERVATIVE.STATUS,
                                                 TRUE ~ SoIB.Latest.Long.Term.Status)) %>%
      dplyr::select(-CONSERVATIVE.STATUS, -ROBUST)
    
  } else {
    print(glue("Skipping sensitivity-check-based adjustments to Trend Status (LTT) for {cur_mask} (LTT)"))
  }
  
  
  if (run_res_trends_CAT == TRUE) {
    
    # sensitivity check for current trends ###
  
  # changes classifications based on sensitivity analyses
  
  sens <- read.csv(cursens_path)
  
  # classifying the sens values to SoIB categories
  sens_cat <- imap(soib_year_info("cat_years"), ~ {
    
    sens %>%
      # selecting the corresponding column each time
      mutate(currentslopelci = .[[2 + (.y - 1) * 3]],
             currentslopemean = .[[3 + (.y - 1) * 3]],
             currentsloperci = .[[4 + (.y - 1) * 3]]) %>%
      mutate(SoIB.Latest.Current.Status.Sens = case_when(
        
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
      dplyr::select(eBird.English.Name.2023, SoIB.Latest.Current.Status.Sens) %>%
      magrittr::set_colnames(c("eBird.English.Name.2023", glue("s{.x}")))
    
  }) %>%
    reduce(full_join)
  
  sens_cat <- main %>%
    dplyr::select(eBird.English.Name.2023, SoIB.Latest.Current.Status) %>%
    left_join(sens_cat) %>%
    filter(!SoIB.Latest.Current.Status %in% c("Insufficient Data", "Trend Inconclusive"))
  
  
  # creating empty vectors that will be filled with indices of species that fall
  # under 7 criteria
  
  ind1 = numeric(0)
  ind2 = numeric(0)
  ind3 = numeric(0)
  ind4 = numeric(0)
  ind5 = numeric(0)
  ind6 = numeric(0)
  ind7 = numeric(0)
  
  for (i in 1:length(sens_cat$eBird.English.Name.2023)) {
    
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
  
  spec_ind.rem <- sens_cat$eBird.English.Name.2023[ind.rem]
  spec_ind6 <- sens_cat$eBird.English.Name.2023[ind6]
  spec_ind7 <- sens_cat$eBird.English.Name.2023[ind7]
  
  
  
  
  main <- main %>%
    # changing classification where needed
    mutate(SoIB.Latest.Current.Status = case_when(
      
      eBird.English.Name.2023 %in% spec_ind.rem ~ "Trend Inconclusive",
      eBird.English.Name.2023 %in% spec_ind6 ~ "Decline",
      eBird.English.Name.2023 %in% spec_ind7 ~ "Increase",
      TRUE ~ SoIB.Latest.Current.Status
      
    ))
  
  } else {
    print(glue("Skipping sensitivity-check-based adjustments to Trend Status (CAT) for {cur_mask}"))
  }
  
} else {
  
  print(glue("Skipping sensitivity-check-based adjustments to Trend Status for {cur_mask}"))
  
}


# classification: converting all non-selected to NA -----------------------

main <- main %>%
  mutate(across(c(SoIB.Latest.Long.Term.Status, SoIB.Latest.Current.Status, SoIB.Past.Range.Status),
                ~ if_else(Selected.SoIB != "X", NA_character_, .)))

# classification: assign SoIB Priority status (based on trends and occupancy) -----

cats_trend = c("Rapid Decline", "Decline", "Insufficient Data",
               "Trend Inconclusive", "Stable", "Increase", "Rapid Increase")
cats_range = c("Historical", "Very Restricted", "Restricted",
               "Moderate", "Large", "Very Large")

cats_decline = c("Decline", "Rapid Decline")
cats_uncertain = c("Insufficient Data", "Trend Inconclusive")
cats_restricted = c("Historical", "Very Restricted", "Restricted")


# old categories, has to change when past categories align with latest
cats_trend_soib1 = c("Strong Decline", "Moderate Decline", "Data Deficient",
                     "Uncertain", "Stable", "Moderate Increase", "Strong Increase")

cats_decline_soib1 = c("Moderate Decline", "Strong Decline")
cats_uncertain_soib1 <- c("Data Deficient", "Uncertain")


main = main %>%
  left_join(priorityrules) %>%
  mutate(SoIB.Latest.Priority.Status = as.character(SoIB.Latest.Priority.Status)) %>%
  # changing priority rules based on IUCN category (which isn't considered in rules)
  mutate(SoIB.Latest.Priority.Status = case_when(
    
    IUCN.Category %in% c("Extinct in the Wild", "Extinct") ~ "Low",
    
    SoIB.Latest.Long.Term.Status %in% cats_uncertain &
      SoIB.Latest.Current.Status %in% cats_uncertain &
      IUCN.Category %in% c("Endangered", "Critically Endangered") ~ "High",
    
    SoIB.Latest.Long.Term.Status %in% cats_decline &
      SoIB.Latest.Current.Status %in% cats_decline &
      IUCN.Category %in% c("Endangered", "Critically Endangered") ~ "High",
    
    SoIB.Latest.Long.Term.Status %in% cats_uncertain &
      SoIB.Latest.Current.Status %in% cats_uncertain &
      SoIB.Latest.Range.Status %in% cats_restricted &
      IUCN.Category %in% c("Vulnerable") ~ "High",
    
    SoIB.Latest.Long.Term.Status %in% cats_uncertain &
      SoIB.Latest.Current.Status %in% cats_uncertain &
      IUCN.Category %in% c("Near Threatened", "Vulnerable") &
      SoIB.Latest.Priority.Status == "Low" ~ "Moderate",
    
    SoIB.Latest.Long.Term.Status == "Insufficient Data" &
      SoIB.Latest.Current.Status == "Insufficient Data" &
      Endemic.Region != "Non-endemic" &
      SoIB.Latest.Priority.Status == "Low" ~ "Moderate",
    
    TRUE ~ SoIB.Latest.Priority.Status
    
  )) %>%
  # converting percentage-of-year1 to percentage-change
  mutate(across(c(longtermlci, longtermmean, longtermrci),
                ~ . - 100)) %>%
  # ensuring correct order of columns
  relocate(eBird.English.Name.2023, eBird.Scientific.Name.2023, eBird.Code, Order, Family,
           SoIB.Past.Priority.Status, SoIB.Past.Long.Term.Status, SoIB.Past.Current.Status, SoIB.Past.Range.Status,
           Breeding.Activity.Period, Non.Breeding.Activity.Period,
           Diet.Guild, India.Endemic, Subcontinent.Endemic, Himalayas.Endemic, Endemic.Region,
           Habitat.Specialization, Migratory.Status.Within.India, Essential, Discard,
           Restricted.Islands,
           India.Checklist.Common.Name, India.Checklist.Scientific.Name,
           BLI.Common.Name, BLI.Scientific.Name, IUCN.Category, WPA.Schedule,
           CITES.Appendix, CMS.Appendix, Onepercent.Estimates,
           Long.Term.Analysis, Current.Analysis, Selected.SoIB,
           totalrange25km, proprange25km2000, proprange25km.current, proprange25km.latestyear,
           mean5km, ci5km,
           get_iucn_proj_cols(),
           longtermlci, longtermmean, longtermrci,
           currentslopelci, currentslopemean, currentsloperci,
           rangelci, rangemean, rangerci,
           SoIB.Latest.Long.Term.Status, SoIB.Latest.Current.Status, SoIB.Latest.Range.Status,
           SoIB.Latest.Priority.Status)

main = main %>%
  mutate(
    Long.Term.Analysis = case_when(
      SoIB.Latest.Long.Term.Status == "Insufficient Data" ~ "",
      TRUE ~ Long.Term.Analysis),
    Current.Analysis = case_when(
      SoIB.Latest.Current.Status == "Insufficient Data" ~ "",
      TRUE ~ Current.Analysis)
  )


# removing/changing Status assignments where not applicable

# get mapping of national-level species with their Priority Status categories 
# from resolved & classified file of full-country
nat_priority <- get_metadata("none")$SOIBMAIN.PATH %>% 
  read.csv() %>% 
  # contains() because if running national for first time, column will have previous year, 
  # else current year
  dplyr::select(contains("eBird.English.Name.20"), SoIB.Latest.Priority.Status)

main <- main %>% 
  # SoIB past statuses not available for habs/states
  # SoIB range status not available for habs/states
  mutate(across(c("SoIB.Past.Priority.Status", "SoIB.Past.Long.Term.Status","SoIB.Past.Current.Status","SoIB.Past.Range.Status",
                  "SoIB.Latest.Range.Status"),
                ~ case_when(cur_mask != "none" ~ NA, TRUE ~ .))) %>% 
  # Priority Status for subnational levels should take the national-level values
  # but only if running latest year subnational AFTER latest year national run
  {if (as.numeric(str_remove(names(nat_priority)[1], "eBird.English.Name.")) == 
       soib_year_info("latest_year")) {
    dplyr::select(., -SoIB.Latest.Priority.Status) %>% 
      left_join(nat_priority, by = "eBird.English.Name.2023") 
  } else {
    .
  }} 


# retain Status categories from major update separately (only for interannual updates)

if (interannual_update == TRUE){

  status_majupd_file = "SoIB_main_status_majupd.csv" # file to move past data to
  # (if a new major update being run, it will be a different repo altogether
  # so the if conditional will work anyway)
  status_majupd_path <- glue("{get_metadata(cur_mask)$RESULTS}{status_majupd_file}")
  
  main_past = read.csv(main_path)
  main_past_spec_col <- names(main_past)[1]
  
  if (!file.exists(status_majupd_path)) { # for the 1st interannual update in one major update cycle

    # filter for the 4 major update status columns and save as csv
    
    # update to latest taxonomy and select category columns
    status_maj_upd = main_past %>%
      dplyr::select(main_past_spec_col, SOIBv2.Long.Term.Status, SOIBv2.Current.Status,
                    SOIBv2.Range.Status, SOIBv2.Priority.Status) %>%
      left_join(ebird_tax_mapping(), by = main_past_spec_col) %>%
      # SOIB.v2 to be updated to SoIB.Latest in the next annual update
      # Need to raise an issue here as the main files can't be edited
      dplyr::select(eBird.English.Name.2023,
                    SoIB.Major.Update.Long.Term.Status = SOIBv2.Long.Term.Status,
                    SoIB.Major.Update.Current.Status = SOIBv2.Current.Status,
                    SoIB.Major.Update.Range.Status = SOIBv2.Range.Status,
                    SoIB.Major.Update.Priority.Status = SOIBv2.Priority.Status)
    
    # write latest major update file
    write.csv(status_maj_upd, file = status_majupd_path, row.names = FALSE)
    
  } else { 
    
    # this would be for reruns during the same update cycle, or
    # for later interannual updates during the same major update cycle
    
    # read csv of maj upd statuses
    status_maj_upd = read.csv(status_majupd_path)
    
    if (file.exists(status_majupd_path) & (names(main)[1] != main_past_spec_col)) {
      # to ensure that it's brought to the correct taxonomy in the following years
      status_maj_upd <- status_maj_upd %>%
        left_join(ebird_tax_mapping(), by = main_past_spec_col) %>% 
        dplyr::select(names(main)[1], starts_with("SoIB.Major.Update"))
    }

  }
    
    # add to current main data, order with the major update columns at the end
    main = main %>%
      left_join(status_maj_upd, by = "eBird.English.Name.2023") %>%
      # ensuring correct order of columns
      relocate(SoIB.Major.Update.Long.Term.Status, SoIB.Major.Update.Current.Status,
               SoIB.Major.Update.Range.Status, SoIB.Major.Update.Priority.Status,
               .after = last_col()) 
  
}


# saving main sheet after classification
write.csv(main, file = main_path, row.names = FALSE)


# summaries -------------------------------------------------------------------

# species qualifications
species_qual0 <- main %>%
  mutate(Range.Analysis = if_else(is.na(SoIB.Latest.Range.Status), "", "X")) %>%
  summarise(across(c(Selected.SoIB, Long.Term.Analysis, Current.Analysis, Range.Analysis),
                   # adds up cases where condition is true
                   ~ sum(. == "X"))) %>% 
  # we don't analyse range for any habitat/CA mask
  mutate(Range.Analysis  = case_when(cur_metadata$MASK.TYPE %in% c("habitat", "conservation_area") ~ 0,
                                     TRUE ~ Range.Analysis))

# number of species with conclusive trends
nspec_trend_inconc <- data.frame(Category = cats_trend) %>%
  magrittr::set_colnames("Trend Status") %>%
  left_join(main %>%
              count(SoIB.Latest.Long.Term.Status) %>%
              magrittr::set_colnames(c("Trend Status", "Long-term species (no.)"))) %>%
  left_join(main %>%
              count(SoIB.Latest.Current.Status) %>%
              magrittr::set_colnames(c("Trend Status", "Current species (no.)"))) %>%
  filter(`Trend Status` == "Trend Inconclusive") %>%
  mutate(across(everything(), ~ replace_na(., 0)))

nspec_trend_conc_ltt = species_qual0$Long.Term.Analysis - nspec_trend_inconc$`Long-term species (no.)`
nspec_trend_conc_cat = species_qual0$Current.Analysis - nspec_trend_inconc$`Current species (no.)`

species_qual <- species_qual0 %>%
  magrittr::set_colnames(c("SoIB Assessment", "Long-term Analysis",
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
              count(SoIB.Latest.Long.Term.Status) %>%
              magrittr::set_colnames(c("Trend Status", "Long-term species (no.)"))) %>%
  left_join(main %>%
              count(SoIB.Latest.Current.Status) %>%
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
              count(SoIB.Latest.Range.Status) %>%
              magrittr::set_colnames(c("Range Status", "Species (no.)"))) %>%
  mutate(across(everything(), ~ replace_na(., 0))) %>%
  mutate(`Species (perc.)` = round(100 * (`Species (no.)` / sum(`Species (no.)`)), 1)) %>%
  mutate(across(everything(), ~ replace(., is.nan(.), NA_real_)))

status_priority <- main %>%
  filter(!is.na(SoIB.Latest.Priority.Status)) %>% # count() counts NA also
  mutate(SoIB.Latest.Priority.Status = factor(SoIB.Latest.Priority.Status,
                                         levels = c("High", "Moderate", "Low"))) %>%
  count(SoIB.Latest.Priority.Status) %>%
  complete(SoIB.Latest.Priority.Status, fill = list(n = 0)) %>%
  magrittr::set_colnames(c("Priority Status", "No. of species"))


# From the last major update, break-up of how High Priority species which
# were not High in the past ("new") were attained - this is basically a comparison of 
# major updates

high_priority_new_breakup_major_update = main %>% 
  {if (interannual_update == TRUE) {
    mutate(.,
           SoIB.Latest.Priority.Status = SoIB.Major.Update.Priority.Status,
           SoIB.Latest.Long.Term.Status = SoIB.Major.Update.Long.Term.Status,
           SoIB.Latest.Current.Status = SoIB.Major.Update.Current.Status,
           SoIB.Latest.Range.Status = SoIB.Major.Update.Range.Status)
  } else {
    .
  }} %>% 
  filter(SoIB.Latest.Priority.Status == "High",
         SoIB.Past.Priority.Status != "High" | is.na(SoIB.Past.Priority.Status)) %>%
  ### below needs to be functionised because the rules stay the same ###
  transmute(Breakup = case_when(
    
    # we have conclusive trend this time, but last time was inconclusive or NA
    (!SoIB.Latest.Long.Term.Status %in% cats_uncertain | !SoIB.Latest.Current.Status %in% cats_uncertain) &
      (SoIB.Past.Long.Term.Status %in% cats_uncertain_soib1 | is.na(SoIB.Past.Long.Term.Status) |
         SoIB.Past.Current.Status %in% cats_uncertain_soib1 | is.na(SoIB.Past.Current.Status)) ~ "Trend New",
    
    # had conclusive trends both times, but this time trend different
    (!SoIB.Latest.Long.Term.Status %in% cats_uncertain | !SoIB.Latest.Current.Status %in% cats_uncertain) ~
      "Trend Different",
    
    # if trends not different or new, assigned high priority based on range
    SoIB.Latest.Range.Status == "Very Restricted" ~ "Range",
    
    # if not even by range, then assigned high priority based on IUCN category
    TRUE ~ "IUCN"
    
  )) %>%
  mutate(Breakup = factor(Breakup,
                          levels = c("Trend New", "Trend Different", "Range", "IUCN"))) %>%
  count(Breakup) %>%
  mutate(Perc = round(100 * (n / sum(n)), 1)) %>%
  magrittr::set_colnames(c("Break-up", "New High Species (no.)", "New High Species (perc.)"))


# changes in the major update - summary of high priority species, not just new changes
high_priority_breakup_major_update = main %>% 
  {if (interannual_update == TRUE) {
    mutate(.,
           SoIB.Latest.Priority.Status = SoIB.Major.Update.Priority.Status,
           SoIB.Latest.Long.Term.Status = SoIB.Major.Update.Long.Term.Status,
           SoIB.Latest.Current.Status = SoIB.Major.Update.Current.Status,
           SoIB.Latest.Range.Status = SoIB.Major.Update.Range.Status)
  } else {
    .
  }} %>% 
  filter(SoIB.Latest.Priority.Status == "High") %>%
  transmute(Breakup = case_when(
    
    # we have conclusive trend this time, but last time was inconclusive or NA
    (!SoIB.Latest.Long.Term.Status %in% cats_uncertain | !SoIB.Latest.Current.Status %in% cats_uncertain) &
      (SoIB.Past.Long.Term.Status %in% cats_uncertain_soib1 | is.na(SoIB.Past.Long.Term.Status) |
         SoIB.Past.Current.Status %in% cats_uncertain_soib1 | is.na(SoIB.Past.Current.Status)) ~ "Trend New",
    
    # had conclusive trends both times, but this time trend different
    (!SoIB.Latest.Long.Term.Status %in% cats_uncertain | !SoIB.Latest.Current.Status %in% cats_uncertain) ~
      "Trend Different",
    
    # if trends not different or new, assigned high priority based on range
    SoIB.Latest.Range.Status == "Very Restricted" ~ "Range",
    
    # if not even by range, then assigned high priority based on IUCN category
    TRUE ~ "IUCN"
    
  )) %>%
  mutate(Breakup = factor(Breakup,
                          levels = c("Trend New", "Trend Different", "Range", "IUCN"))) %>%
  count(Breakup) %>%
  mutate(Perc = round(100 * (n / sum(n)), 1)) %>%
  magrittr::set_colnames(c("Break-up", "High Species (no.)", "High Species (perc.)")) %>%
  left_join(high_priority_new_breakup_major_update)


# break-up of how latest High Priority species which were not High in the last
# major update were attained - this a comparison of the latest interannual update with major
# only applicable for an interannual update

if (interannual_update == TRUE) {
  
  high_priority_new_breakup_interannual_update = main %>%
    filter(SoIB.Latest.Priority.Status == "High",
           SoIB.Major.Update.Priority.Status != "High" | is.na(SoIB.Major.Update.Priority.Status)) %>%
    transmute(Breakup = case_when(
      
      # we have conclusive trend this time, but last time was inconclusive or NA
      (!SoIB.Latest.Long.Term.Status %in% cats_uncertain | !SoIB.Latest.Current.Status %in% cats_uncertain) &
        (SoIB.Major.Update.Long.Term.Status %in% cats_uncertain | is.na(SoIB.Major.Update.Long.Term.Status) |
           SoIB.Major.Update.Current.Status %in% cats_uncertain | is.na(SoIB.Major.Update.Current.Status)) ~ "Trend New",
      
      # had conclusive trends both times, but this time trend different
      (!SoIB.Latest.Long.Term.Status %in% cats_uncertain | !SoIB.Latest.Current.Status %in% cats_uncertain) ~
        "Trend Different",
      
      # if trends not different or new, assigned high priority based on range
      SoIB.Latest.Range.Status == "Very Restricted" ~ "Range",
      
      # if not even by range, then assigned high priority based on IUCN category
      TRUE ~ "IUCN"
      
    )) %>%
    mutate(Breakup = factor(Breakup,
                            levels = c("Trend New", "Trend Different", "Range", "IUCN"))) %>%
    count(Breakup) %>%
    mutate(Perc = round(100 * (n / sum(n)), 1)) %>%
    magrittr::set_colnames(c("Break-up", "New High Species (no.)", "New High Species (perc.)"))
  
  
  # changes in the interannual update - summary of high priority species, not just new changes
  
  high_priority_breakup_interannual_update = main %>%
    filter(SoIB.Latest.Priority.Status == "High") %>%
    transmute(Breakup = case_when(
      
      # we have conclusive trend this time, but last time was inconclusive or NA
      (!SoIB.Latest.Long.Term.Status %in% cats_uncertain | !SoIB.Latest.Current.Status %in% cats_uncertain) &
        (SoIB.Major.Update.Long.Term.Status %in% cats_uncertain | is.na(SoIB.Major.Update.Long.Term.Status) |
           SoIB.Major.Update.Current.Status %in% cats_uncertain | is.na(SoIB.Major.Update.Current.Status)) ~ "Trend New",
      
      # had conclusive trends both times, but this time trend different
      (!SoIB.Latest.Long.Term.Status %in% cats_uncertain | !SoIB.Latest.Current.Status %in% cats_uncertain) ~
        "Trend Different",
      
      # if trends not different or new, assigned high priority based on range
      SoIB.Latest.Range.Status == "Very Restricted" ~ "Range",
      
      # if not even by range, then assigned high priority based on IUCN category
      TRUE ~ "IUCN"
      
    )) %>%
    mutate(Breakup = factor(Breakup,
                            levels = c("Trend New", "Trend Different", "Range", "IUCN"))) %>%
    count(Breakup) %>%
    mutate(Perc = round(100 * (n / sum(n)), 1)) %>%
    magrittr::set_colnames(c("Break-up", "High Species (no.)", "High Species (perc.)")) %>%
    left_join(high_priority_new_breakup_interannual_update)
  
}


if (cur_metadata$MASK.TYPE == "country") {
  
  # comparing SoIB major updates
  
  SoIB1_SoIB2_major_update = main
  SoIB2_SoIB1_major_update = main
  
  if (interannual_update == TRUE) {
    SoIB1_SoIB2_major_update = SoIB1_SoIB2_major_update %>%
      mutate(SoIB.Latest.Priority.Status = SoIB.Major.Update.Priority.Status)
    SoIB2_SoIB1_major_update = SoIB2_SoIB1_major_update %>%
      mutate(SoIB.Latest.Priority.Status = SoIB.Major.Update.Priority.Status)
  }
    
  SoIB1_SoIB2_major_update = SoIB1_SoIB2_major_update %>%
    filter(!is.na(SoIB.Past.Priority.Status) & SoIB.Past.Priority.Status != "") %>%
    group_by(SoIB.Past.Priority.Status) %>%
    mutate(n = n()) %>%
    group_by(SoIB.Past.Priority.Status, SoIB.Latest.Priority.Status) %>%
    reframe(NO.SPEC = n(),
            PERC.SPEC = round(100 * (NO.SPEC / max(n)), 1)) %>%
    magrittr::set_colnames(c("SoIB Past Priority Status", "SoIB Major Update Priority Status",
                             "Species (no.)", "Species (perc.)"))
  
  SoIB2_SoIB1_major_update = SoIB2_SoIB1_major_update %>%
    filter(!is.na(SoIB.Latest.Priority.Status) & SoIB.Latest.Priority.Status != "") %>%
    group_by(SoIB.Latest.Priority.Status) %>%
    mutate(n = n()) %>%
    group_by(SoIB.Latest.Priority.Status, SoIB.Past.Priority.Status) %>%
    reframe(NO.SPEC = n(),
            PERC.SPEC = round(100 * (NO.SPEC / max(n)), 1)) %>%
    magrittr::set_colnames(c("SoIB Major Update Priority Status", "SoIB Past Priority Status",
                             "Species (no.)", "Species (perc.)"))
  
  # comparing an SoIB interannual update
  
  if (interannual_update == TRUE) {
    SoIB1_SoIB2_interannual_update = main %>%
      filter(!is.na(SoIB.Major.Update.Priority.Status) & SoIB.Major.Update.Priority.Status != "") %>%
      group_by(SoIB.Major.Update.Priority.Status) %>%
      mutate(n = n()) %>%
      group_by(SoIB.Major.Update.Priority.Status, SoIB.Latest.Priority.Status) %>%
      reframe(NO.SPEC = n(),
              PERC.SPEC = round(100 * (NO.SPEC / max(n)), 1)) %>%
      magrittr::set_colnames(c("SoIB Major Update Priority Status", "SoIB Latest Priority Status",
                               "Species (no.)", "Species (perc.)"))
    
    SoIB2_SoIB1_interannual_update = main %>%
      filter(!is.na(SoIB.Latest.Priority.Status) & SoIB.Latest.Priority.Status != "") %>%
      group_by(SoIB.Latest.Priority.Status) %>%
      mutate(n = n()) %>%
      group_by(SoIB.Latest.Priority.Status, SoIB.Major.Update.Priority.Status) %>%
      reframe(NO.SPEC = n(),
              PERC.SPEC = round(100 * (NO.SPEC / max(n)), 1)) %>%
      magrittr::set_colnames(c("SoIB Latest Priority Status", "SoIB Major Update Priority Status",
                               "Species (no.)", "Species (perc.)"))
  }
  
  
  
  # cross-tab of SoIB and IUCN assessments - major update
  
  SoIB_vs_IUCN_0 = main %>% 
    {if (interannual_update == TRUE) {
      mutate(., SoIB.Latest.Priority.Status = SoIB.Major.Update.Priority.Status)
    } else {
      .
    }} %>% 
    filter(Selected.SoIB == "X") %>%
    mutate(SoIB.Latest.Priority.Status = factor(SoIB.Latest.Priority.Status,
                                           levels = c("High", "Moderate", "Low")),
           IUCN.Category = factor(IUCN.Category, levels = c(
             "Critically Endangered", "Endangered", "Vulnerable", "Near Threatened",
             "Least Concern", "Not Recognised"
           ))) %>%
    group_by(SoIB.Latest.Priority.Status, IUCN.Category) %>%
    tally() %>%
    pivot_wider(names_from = SoIB.Latest.Priority.Status, values_from = n) %>%
    replace(is.na(.), 0) %>%
    magrittr::set_colnames(c(" ", "High", "Moderate", "Low"))
  
  temp <- SoIB_vs_IUCN_0 %>%
    reframe(across(c("High", "Moderate", "Low"), sum)) %>%
    mutate(new = "Sum") %>%
    relocate(new, High, Moderate, Low) %>%
    magrittr::set_colnames(c(" ", "High", "Moderate", "Low"))
  
  SoIB_vs_IUCN_major_update <- SoIB_vs_IUCN_0 %>%
    bind_rows(temp) %>%
    mutate(Sum = High + Low + Moderate)
  
  SoIB_vs_IUCN_percIUCN_major_update = SoIB_vs_IUCN_0 %>%
    mutate(Sum = High + Low + Moderate) %>%
    mutate(across(c("High", "Moderate", "Low"),
                  ~ round(100 * (. / Sum), 1))) %>%
    mutate(Sum = NULL)
  
  SoIB_vs_IUCN_percSoIB_major_update = SoIB_vs_IUCN_0 %>%
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
  
  
  
  if (interannual_update == TRUE) {
    
    # cross-tab of SoIB and IUCN assessments - interannual update
    SoIB_vs_IUCN_0 <- main %>%
      filter(Selected.SoIB == "X") %>%
      mutate(SoIB.Latest.Priority.Status = factor(SoIB.Latest.Priority.Status,
                                                  levels = c("High", "Moderate", "Low")),
             IUCN.Category = factor(IUCN.Category, levels = c(
               "Critically Endangered", "Endangered", "Vulnerable", "Near Threatened",
               "Least Concern", "Not Recognised"
             ))) %>%
      group_by(SoIB.Latest.Priority.Status, IUCN.Category) %>%
      tally() %>%
      pivot_wider(names_from = SoIB.Latest.Priority.Status, values_from = n) %>%
      replace(is.na(.), 0) %>%
      magrittr::set_colnames(c(" ", "High", "Moderate", "Low"))
    
    temp <- SoIB_vs_IUCN_0 %>%
      reframe(across(c("High", "Moderate", "Low"), sum)) %>%
      mutate(new = "Sum") %>%
      relocate(new, High, Moderate, Low) %>%
      magrittr::set_colnames(c(" ", "High", "Moderate", "Low"))
    
    SoIB_vs_IUCN_interannual_update <- SoIB_vs_IUCN_0 %>%
      bind_rows(temp) %>%
      mutate(Sum = High + Low + Moderate)
    
    SoIB_vs_IUCN_percIUCN_interannual_update = SoIB_vs_IUCN_0 %>%
      mutate(Sum = High + Low + Moderate) %>%
      mutate(across(c("High", "Moderate", "Low"),
                    ~ round(100 * (. / Sum), 1))) %>%
      mutate(Sum = NULL)
    
    SoIB_vs_IUCN_percSoIB_interannual_update = SoIB_vs_IUCN_0 %>%
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
    
  }
  
  
  # reasons for uplisting or downlisting - major update
  
  reason_uplist_high_major_update = main
  reason_downlist_high_major_update = main
  
  if (interannual_update == TRUE) {
    reason_uplist_high_major_update = reason_uplist_high_major_update %>%
      mutate(SoIB.Latest.Priority.Status = SoIB.Major.Update.Priority.Status,
             SoIB.Latest.Long.Term.Status = SoIB.Major.Update.Long.Term.Status,
             SoIB.Latest.Current.Status = SoIB.Major.Update.Current.Status,
             SoIB.Latest.Range.Status = SoIB.Major.Update.Range.Status)
    
    reason_downlist_high_major_update = reason_downlist_high_major_update %>%
      mutate(SoIB.Latest.Priority.Status = SoIB.Major.Update.Priority.Status,
             SoIB.Latest.Long.Term.Status = SoIB.Major.Update.Long.Term.Status,
             SoIB.Latest.Current.Status = SoIB.Major.Update.Current.Status,
             SoIB.Latest.Range.Status = SoIB.Major.Update.Range.Status)
  }
  
  reason_uplist_high_major_update = reason_uplist_high_major_update %>%
    filter(SoIB.Past.Priority.Status %in% c("Low", "Moderate"),
           SoIB.Latest.Priority.Status == "High") %>%
    mutate(Breakup = case_when(
      
      # LTT and CAT were uncertain in the past but we have some trend now
      ((!SoIB.Latest.Long.Term.Status %in% cats_uncertain | !SoIB.Latest.Current.Status %in% cats_uncertain) &
         SoIB.Past.Long.Term.Status %in% cats_uncertain_soib1 &
         SoIB.Past.Current.Status %in% cats_uncertain_soib1)  ~ "First-time trend",
      
      # stronger decline this time (LTT)
      (SoIB.Latest.Long.Term.Status == "Rapid Decline" &
         SoIB.Past.Long.Term.Status %in% cats_trend_soib1[!cats_trend_soib1 == "Strong Decline" &
                                                       !cats_trend_soib1 %in% cats_uncertain_soib1]) |
        (SoIB.Latest.Long.Term.Status %in% cats_decline &
           SoIB.Past.Long.Term.Status %in% cats_trend_soib1[!cats_trend_soib1 %in% cats_decline_soib1 &
                                                         !cats_trend_soib1 %in% cats_uncertain_soib1])
      ~ "More decline in LTT",
      
      # stronger decline this time (CAT)
      (SoIB.Latest.Current.Status == "Rapid Decline" &
         SoIB.Past.Current.Status %in% cats_trend_soib1[!cats_trend_soib1 == "Strong Decline" &
                                                     !cats_trend_soib1 %in% cats_uncertain_soib1]) |
        (SoIB.Latest.Current.Status %in% cats_decline &
           SoIB.Past.Current.Status %in% cats_trend_soib1[!cats_trend_soib1 %in% cats_decline_soib1 &
                                                       !cats_trend_soib1 %in% cats_uncertain_soib1])
      ~ "More decline in CAT",
      
      # first-time LTT & first-time CAT
      (SoIB.Past.Long.Term.Status %in% cats_uncertain_soib1 &
         !SoIB.Latest.Long.Term.Status %in% cats_uncertain) ~ "First-time LTT",
      (SoIB.Past.Current.Status %in% cats_uncertain_soib1 &
         !SoIB.Latest.Current.Status %in% cats_uncertain) ~ "First-time CAT",
      
      # both times, had at least one of two trends
      (!SoIB.Past.Long.Term.Status %in% cats_uncertain_soib1 |
         !SoIB.Past.Current.Status %in% cats_uncertain_soib1) &
        (!SoIB.Latest.Long.Term.Status %in% cats_uncertain |
           !SoIB.Latest.Current.Status %in% cats_uncertain)  ~ "Other changes in trends",
      
      # earlier had at least one trend, but this time both trends uncertain
      (!SoIB.Past.Long.Term.Status %in% cats_uncertain_soib1 |
         !SoIB.Past.Current.Status %in% cats_uncertain_soib1) &
        (SoIB.Latest.Long.Term.Status %in% cats_uncertain &
           SoIB.Latest.Current.Status %in% cats_uncertain) ~ "Loss of trends",
      
      (SoIB.Past.Range.Status == "Restricted" & SoIB.Latest.Range.Status == "Very Restricted") |
        (SoIB.Past.Range.Status == "Moderate" & SoIB.Latest.Range.Status == "Restricted") |
        (SoIB.Past.Range.Status == "Large" & SoIB.Latest.Range.Status == "Moderate") ~ "Reducing range",
      
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
  
  
  reason_downlist_high_major_update = reason_downlist_high_major_update %>%
    filter(SoIB.Latest.Priority.Status %in% c("Low", "Moderate"),
           SoIB.Past.Priority.Status == "High") %>%
    mutate(Breakup = case_when(
      
      # LTT and CAT were uncertain in the past but we have some trend now
      ((!SoIB.Latest.Long.Term.Status %in% cats_uncertain | !SoIB.Latest.Current.Status %in% cats_uncertain) &
         SoIB.Past.Long.Term.Status %in% cats_uncertain_soib1 &
         SoIB.Past.Current.Status %in% cats_uncertain_soib1)  ~ "First-time trend",
      
      # weaker decline this time (LTT)
      (SoIB.Past.Long.Term.Status == "Strong Decline" &
         SoIB.Latest.Long.Term.Status %in% cats_trend[!cats_trend == "Rapid Decline" &
                                                   !cats_trend %in% cats_uncertain]) |
        (SoIB.Past.Long.Term.Status %in% cats_decline_soib1 &
           SoIB.Latest.Long.Term.Status %in% cats_trend[!cats_trend %in% cats_decline &
                                                     !cats_trend %in% cats_uncertain]) |
        (SoIB.Past.Long.Term.Status %in% c(cats_decline_soib1, "Stable") &
           SoIB.Latest.Long.Term.Status %in% c("Increase", "Rapid Increase")) |
        (SoIB.Past.Long.Term.Status %in% c(cats_decline_soib1, "Stable", "Moderate Increase") &
           SoIB.Latest.Long.Term.Status == "Rapid Increase")
      ~ "Less decline in LTT",
      
      # stronger decline this time (CAT)
      (SoIB.Past.Current.Status == "Strong Decline" &
         SoIB.Latest.Current.Status %in% cats_trend[!cats_trend == "Rapid Decline" &
                                                 !cats_trend %in% cats_uncertain]) |
        (SoIB.Past.Current.Status %in% cats_decline_soib1 &
           SoIB.Latest.Current.Status %in% cats_trend[!cats_trend %in% cats_decline &
                                                   !cats_trend %in% cats_uncertain]) |
        (SoIB.Past.Current.Status %in% c(cats_decline_soib1, "Stable") &
           SoIB.Latest.Current.Status %in% c("Increase", "Rapid Increase")) |
        (SoIB.Past.Current.Status %in% c(cats_decline_soib1, "Stable", "Moderate Increase") &
           SoIB.Latest.Current.Status == "Rapid Increase")
      ~ "Less decline in CAT",
      
      # first-time LTT & first-time CAT
      (SoIB.Past.Long.Term.Status %in% cats_uncertain_soib1 &
         !SoIB.Latest.Long.Term.Status %in% cats_uncertain) ~ "First-time LTT",
      (SoIB.Past.Current.Status %in% cats_uncertain_soib1 &
         !SoIB.Latest.Current.Status %in% cats_uncertain) ~ "First-time CAT",
      
      # both times, had at least one of two trends
      (!SoIB.Past.Long.Term.Status %in% cats_uncertain_soib1 |
         !SoIB.Past.Current.Status %in% cats_uncertain_soib1) &
        (!SoIB.Latest.Long.Term.Status %in% cats_uncertain |
           !SoIB.Latest.Current.Status %in% cats_uncertain)  ~ "Other changes in trends",
      
      # earlier had at least one trend, but this time both trends uncertain
      (!SoIB.Past.Long.Term.Status %in% cats_uncertain_soib1 |
         !SoIB.Past.Current.Status %in% cats_uncertain_soib1) &
        (SoIB.Latest.Long.Term.Status %in% cats_uncertain &
           SoIB.Latest.Current.Status %in% cats_uncertain) ~ "Loss of trends",
      
      (SoIB.Latest.Range.Status == "Restricted" & SoIB.Past.Range.Status == "Very Restricted") |
        (SoIB.Latest.Range.Status == "Moderate" & SoIB.Past.Range.Status == "Restricted") |
        (SoIB.Latest.Range.Status == "Large" & SoIB.Past.Range.Status == "Moderate") |
        (SoIB.Latest.Range.Status == "Very Large" & SoIB.Past.Range.Status == "Large") ~ "Increasing range",
      
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
  
  
  if (interannual_update == TRUE) {
    
    reason_uplist_high_interannual_update = main %>%
      filter(SoIB.Major.Update.Priority.Status %in% c("Low", "Moderate"),
             SoIB.Latest.Priority.Status == "High") %>%
      mutate(Breakup = case_when(
        
        # LTT and CAT were uncertain in the major update but we have some trend now
        ((!SoIB.Latest.Long.Term.Status %in% cats_uncertain | !SoIB.Latest.Current.Status %in% cats_uncertain) &
           SoIB.Major.Update.Long.Term.Status %in% cats_uncertain &
           SoIB.Major.Update.Current.Status %in% cats_uncertain)  ~ "First-time trend",
        
        # stronger decline this time (LTT)
        (SoIB.Latest.Long.Term.Status == "Rapid Decline" &
           SoIB.Major.Update.Long.Term.Status %in% cats_trend[!cats_trend == "Rapid Decline" &
                                                              !cats_trend %in% cats_uncertain]) |
          (SoIB.Latest.Long.Term.Status %in% cats_decline &
             SoIB.Major.Update.Long.Term.Status %in% cats_trend[!cats_trend %in% cats_decline &
                                                                !cats_trend %in% cats_uncertain])
        ~ "More decline in LTT",
        
        # stronger decline this time (CAT)
        (SoIB.Latest.Current.Status == "Rapid Decline" &
           SoIB.Major.Update.Current.Status %in% cats_trend[!cats_trend == "Rapid Decline" &
                                                            !cats_trend %in% cats_uncertain]) |
          (SoIB.Latest.Current.Status %in% cats_decline &
             SoIB.Major.Update.Current.Status %in% cats_trend[!cats_trend %in% cats_decline &
                                                              !cats_trend %in% cats_uncertain])
        ~ "More decline in CAT",
        
        # first-time LTT & first-time CAT
        (SoIB.Major.Update.Long.Term.Status %in% cats_uncertain &
           !SoIB.Latest.Long.Term.Status %in% cats_uncertain) ~ "First-time LTT",
        (SoIB.Major.Update.Current.Status %in% cats_uncertain &
           !SoIB.Latest.Current.Status %in% cats_uncertain) ~ "First-time CAT",
        
        # both times, had at least one of two trends
        (!SoIB.Major.Update.Long.Term.Status %in% cats_uncertain |
           !SoIB.Major.Update.Current.Status %in% cats_uncertain) &
          (!SoIB.Latest.Long.Term.Status %in% cats_uncertain |
             !SoIB.Latest.Current.Status %in% cats_uncertain)  ~ "Other changes in trends",
        
        # earlier had at least one trend, but this time both trends uncertain
        (!SoIB.Major.Update.Long.Term.Status %in% cats_uncertain |
           !SoIB.Major.Update.Current.Status %in% cats_uncertain) &
          (SoIB.Latest.Long.Term.Status %in% cats_uncertain &
             SoIB.Latest.Current.Status %in% cats_uncertain) ~ "Loss of trends",
        
        (SoIB.Major.Update.Range.Status == "Restricted" & SoIB.Latest.Range.Status == "Very Restricted") |
          (SoIB.Major.Update.Range.Status == "Moderate" & SoIB.Latest.Range.Status == "Restricted") |
          (SoIB.Major.Update.Range.Status == "Large" & SoIB.Latest.Range.Status == "Moderate") ~ "Reducing range",
        
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
    
    
    reason_downlist_high_interannual_update = main %>%
      filter(SoIB.Latest.Priority.Status %in% c("Low", "Moderate"),
             SoIB.Major.Update.Priority.Status == "High") %>%
      mutate(Breakup = case_when(
        
        # LTT and CAT were uncertain in the major update but we have some trend now
        ((!SoIB.Latest.Long.Term.Status %in% cats_uncertain | !SoIB.Latest.Current.Status %in% cats_uncertain) &
           SoIB.Major.Update.Long.Term.Status %in% cats_uncertain &
           SoIB.Major.Update.Current.Status %in% cats_uncertain)  ~ "First-time trend",
        
        # weaker decline this time (LTT)
        (SoIB.Major.Update.Long.Term.Status == "Rapid Decline" &
           SoIB.Latest.Long.Term.Status %in% cats_trend[!cats_trend == "Rapid Decline" &
                                                          !cats_trend %in% cats_uncertain]) |
          (SoIB.Major.Update.Long.Term.Status %in% cats_decline &
             SoIB.Latest.Long.Term.Status %in% cats_trend[!cats_trend %in% cats_decline &
                                                            !cats_trend %in% cats_uncertain]) |
          (SoIB.Major.Update.Long.Term.Status %in% c(cats_decline, "Stable") &
             SoIB.Latest.Long.Term.Status %in% c("Increase", "Rapid Increase")) |
          (SoIB.Major.Update.Long.Term.Status %in% c(cats_decline, "Stable", "Increase") &
             SoIB.Latest.Long.Term.Status == "Rapid Increase")
        ~ "Less decline in LTT",
        
        # stronger decline this time (CAT)
        (SoIB.Major.Update.Current.Status == "Rapid Decline" &
           SoIB.Latest.Current.Status %in% cats_trend[!cats_trend == "Rapid Decline" &
                                                        !cats_trend %in% cats_uncertain]) |
          (SoIB.Major.Update.Current.Status %in% cats_decline &
             SoIB.Latest.Current.Status %in% cats_trend[!cats_trend %in% cats_decline &
                                                          !cats_trend %in% cats_uncertain]) |
          (SoIB.Major.Update.Current.Status %in% c(cats_decline, "Stable") &
             SoIB.Latest.Current.Status %in% c("Increase", "Rapid Increase")) |
          (SoIB.Major.Update.Current.Status %in% c(cats_decline, "Stable", "Increase") &
             SoIB.Latest.Current.Status == "Rapid Increase")
        ~ "Less decline in CAT",
        
        # first-time LTT & first-time CAT
        (SoIB.Major.Update.Long.Term.Status %in% cats_uncertain &
           !SoIB.Latest.Long.Term.Status %in% cats_uncertain) ~ "First-time LTT",
        (SoIB.Major.Update.Current.Status %in% cats_uncertain &
           !SoIB.Latest.Current.Status %in% cats_uncertain) ~ "First-time CAT",
        
        # both times, had at least one of two trends
        (!SoIB.Major.Update.Long.Term.Status %in% cats_uncertain |
           !SoIB.Major.Update.Current.Status %in% cats_uncertain) &
          (!SoIB.Latest.Long.Term.Status %in% cats_uncertain |
             !SoIB.Latest.Current.Status %in% cats_uncertain)  ~ "Other changes in trends",
        
        # earlier had at least one trend, but this time both trends uncertain
        (!SoIB.Major.Update.Long.Term.Status %in% cats_uncertain |
           !SoIB.Major.Update.Current.Status %in% cats_uncertain) &
          (SoIB.Latest.Long.Term.Status %in% cats_uncertain &
             SoIB.Latest.Current.Status %in% cats_uncertain) ~ "Loss of trends",
        
        (SoIB.Latest.Range.Status == "Restricted" & SoIB.Major.Update.Range.Status == "Very Restricted") |
          (SoIB.Latest.Range.Status == "Moderate" & SoIB.Major.Update.Range.Status == "Restricted") |
          (SoIB.Latest.Range.Status == "Large" & SoIB.Major.Update.Range.Status == "Moderate") |
          (SoIB.Latest.Range.Status == "Very Large" & SoIB.Major.Update.Range.Status == "Large") ~ "Increasing range",
        
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
  
}



# put all summaries together and write an excel

if (cur_metadata$MASK.TYPE == "country") {
  
  if (interannual_update == TRUE) {
    summaries_data = list("Trends Status" = status_trends,
                          "Range Status" = status_range,
                          "Priority Status" = status_priority,
                          "Species qualification" = species_qual,
                          "Interannual update: High Priority break-up" = high_priority_breakup_interannual_update,
                          "SoIB major vs interannual update" = SoIB1_SoIB2_interannual_update,
                          "SoIB interannual vs major update" = SoIB2_SoIB1_interannual_update,
                          "SoIB interannual update vs IUCN (no.)" = SoIB_vs_IUCN_interannual_update,
                          "SoIB interannual update vs IUCN (IUCN %)" = SoIB_vs_IUCN_percIUCN_interannual_update,
                          "SoIB interannual update vs IUCN (SoIB %)" = SoIB_vs_IUCN_percSoIB_interannual_update,
                          "Interannual update: Reason for uplisting" = reason_uplist_high_interannual_update,
                          "Interannual update: Reason for downlisting" = reason_downlist_high_interannual_update,
                          "Major update: High Priority break-up" = high_priority_breakup_major_update,
                          "SoIB past vs major update" = SoIB1_SoIB2_major_update,
                          "SoIB major update vs past" = SoIB2_SoIB1_major_update,
                          "SoIB major update vs IUCN (no.)" = SoIB_vs_IUCN_major_update,
                          "SoIB major update vs IUCN (IUCN %)" = SoIB_vs_IUCN_percIUCN_major_update,
                          "SoIB major update vs IUCN (SoIB %)" = SoIB_vs_IUCN_percSoIB_major_update,
                          "Major update: Reason for uplisting" = reason_uplist_high_major_update,
                          "Major update: Reason for downlisting" = reason_downlist_high_major_update)
  } else {
    
    summaries_data = list("Trends Status" = status_trends,
                          "Range Status" = status_range,
                          "Priority Status" = status_priority,
                          "Species qualification" = species_qual,
                          "Major update: High Priority break-up" = high_priority_breakup_major_update,
                          "SoIB past vs major update" = SoIB1_SoIB2_major_update,
                          "SoIB major update vs past" = SoIB2_SoIB1_major_update,
                          "SoIB major update vs IUCN (no.)" = SoIB_vs_IUCN_major_update,
                          "SoIB major update vs IUCN (IUCN %)" = SoIB_vs_IUCN_percIUCN_major_update,
                          "SoIB major update vs IUCN (SoIB %)" = SoIB_vs_IUCN_percSoIB_major_update,
                          "Major update: Reason for uplisting" = reason_uplist_high_major_update,
                          "Major update: Reason for downlisting" = reason_downlist_high_major_update)
    
  }
  
  
  
} else {
  
  if (interannual_update == TRUE) {
    
    summaries_data = list("Trends Status" = status_trends,
                          "Range Status" = status_range,
                          "Priority Status" = status_priority,
                          "Species qualification" = species_qual,
                          "Interannual update: High Priority break-up" = high_priority_breakup_interannual_update,
                          "Major update: High Priority break-up" = high_priority_breakup_major_update)
    
  } else {
    
    summaries_data = list("Trends Status" = status_trends,
                          "Range Status" = status_range,
                          "Priority Status" = status_priority,
                          "Species qualification" = species_qual,
                          "Major update: High Priority break-up" = high_priority_breakup_major_update)
    
  }
  
}

write_xlsx(path = summaries_path, summaries_data)
