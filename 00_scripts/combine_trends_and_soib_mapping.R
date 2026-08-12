# This script combines trends information with the SoIB mapping file in the first
# step to the creation of the SoIB main file.

library(tidyverse)
library(glue)
library(tictoc)
library(sf)

source('00_scripts/00_functions.R')


# setup -------------------------------------------------------------------

interannual_update <- TRUE
cur_metadata <- get_metadata(cur_mask)
mapping_path <- "00_data/SoIB_mapping_2024.csv"

# read paths
base_path <- cur_metadata$FULLSPECLIST.PATH
speclist_path <- cur_metadata$SPECLISTDATA.PATH
timeseries_pathonly <- cur_metadata$TIMESERIES.FOLDER
ltt_pathonly <- cur_metadata$LTT.FOLDER
cat_pathonly <- cur_metadata$CAT.FOLDER

# write paths
trends_outpath <- cur_metadata$TRENDS.OUTPATH
mainwocats_path <- cur_metadata$SOIBMAIN.WOCATS.PATH

# Occupancy information will be later added to this file to create the
# SoIB_main_wocats.csv 


load("00_data/spec_misid.RData") # to remove from LTT and CAT "selection" later
load(speclist_path)


### for conditionals ###

# don't run resolving if no species selected
run_res_trends <- ((1 %in% specieslist$ht) | (1 %in% specieslist$rt) |
                     (1 %in% restrictedspecieslist$ht) | (1 %in% restrictedspecieslist$rt)) &
  # edge cases where species selected, but trends could not be generated
  (length(list.files(timeseries_pathonly)) != 0)

run_res_trends_LTT <- ((1 %in% specieslist$ht) | (1 %in% restrictedspecieslist$ht)) &
  (length(list.files(timeseries_pathonly)) != 0)

run_res_trends_CAT <- ((1 %in% specieslist$rt) | (1 %in% restrictedspecieslist$rt)) &
  (length(list.files(timeseries_pathonly)) != 0)


# calculations: resolve trends ----------------------------------------------

base = read.csv(base_path) %>%
  # if full column has no X at all, gets read as NAs
  mutate(across(c(Long.Term.Analysis, Current.Analysis, Selected.SoIB),
                ~ as.character(.))) %>%
  mutate(across(c(Long.Term.Analysis, Current.Analysis, Selected.SoIB),
                ~ replace_na(., ""))) %>%
  dplyr::select(-SCIENTIFIC.NAME)

main = read.csv(mapping_path)

# the taxonomy-year species-name column, e.g. "eBird.English.Name.2024",
# read off the mapping file rather than assumed
soib_name_col <- get_soib_name_col(main)

main <- main %>%
  left_join(base, by = setNames("COMMON.NAME", soib_name_col))

if (run_res_trends == FALSE) {

  # list of columns that need to be created since we have skipped steps
  na_columns <- c("longtermlci", "longtermmean", "longtermrci",
                  "currentslopelci", "currentslopemean", "currentsloperci",
                  get_iucn_proj_cols())

  # creating NA columns to match structure of "normal" main data
  main[, na_columns] <- NA_real_

  print(glue("Skipping resolving species trends for {cur_mask}"))

} else {

  # data processing and prep ------------------------------------------------

  # combining the per-species timeseries files
  timeseries_files <- list.files(path = timeseries_pathonly, full.names = TRUE)
  timeseries_files <- timeseries_files[!file.info(timeseries_files)$isdir]

  trends <- timeseries_files %>%
    map_df(read.csv)
  
  trends = trends %>%
    mutate(COMMON.NAME = factor(COMMON.NAME, levels = base$COMMON.NAME)) %>%
    arrange(COMMON.NAME, timegroups) %>%
    # truncating combined (recent + projected) LCI at 0, as in the old methodology
    mutate(lci_std_comb = case_when(lci_std_comb < 0 ~ 0,
                                    TRUE ~ lci_std_comb)) %>%
    relocate(timegroups, COMMON.NAME, timegroupsf)


  # data filtering: extra metrics -------------------------------------------
  # (unchanged from the old methodology: based on sampling-coverage metrics
  # from the mapping/full-specieslist files, independent of how trends
  # themselves are calculated)

  # remove species based on 2 extra metrics (only 1 for PAs and states)

  # 1. number of sampled 5kmx5km cells within (not for PAs or states)

  if (cur_mask %in% c("none", "woodland", "cropland", "ONEland")) {

    specsc1 = main %>%
      # identifying species with less than 8/25 5 km cells with data within 25 km grids
      filter(!is.na(mean5km) & mean5km < 8 &
               (Long.Term.Analysis == "X" | Current.Analysis == "X") &
               is.na(Restricted.Islands)) %>%
      pull(all_of(soib_name_col))

    specsc2 = main %>%
      # identifying species where the variation of within 25 km cell sampling is too high
      filter(!is.na(ci5km) & (ci5km/mean5km) > 0.25 &
               (Long.Term.Analysis == "X" | Current.Analysis == "X") &
               is.na(Restricted.Islands)) %>%
      pull(all_of(soib_name_col))

    specsc = union(specsc1, specsc2)


    trends = trends %>%
      filter(!COMMON.NAME %in% specsc)

    main <- main %>%
      mutate(Long.Term.Analysis = if_else(.data[[soib_name_col]] %in% specsc,
                                          "", Long.Term.Analysis),
             Current.Analysis = if_else(.data[[soib_name_col]] %in% specsc,
                                        "", Current.Analysis))

  }


  # 2. proportion of 25kmx25km cells sampled

  specsd3 = main %>%
    # identifying cells where on an average, less than 60 % of the 2022 coverage
    # was achieved during the last N CAT years
    filter(!is.na(proprange25km.current) &
             (proprange25km.current/proprange25km.latestyear) < 0.6 &
             (Current.Analysis == "X")) %>%
    pull(all_of(soib_name_col))

  trends = trends %>%
    filter(!COMMON.NAME %in% specsd3)

  main <- main %>%
    # remove these species from both analyses, since unlikely that it qualifies for long-term
    # but not for current
    mutate(Long.Term.Analysis = if_else(.data[[soib_name_col]] %in% specsd3,
                                        "", Long.Term.Analysis),
           Current.Analysis = if_else(.data[[soib_name_col]] %in% specsd3,
                                      "", Current.Analysis))


  # rewriting selected species for LTT and CAT, post extra-metrics filtering.
  # species files on disk (LTT/CAT/sensitivity) may still exist for species
  # excluded above -- produce_trends.R has no knowledge of these mapping-based
  # exclusions -- so every combine step below explicitly filters down to
  # spec_lt/spec_ct to keep it consistent with main's analysis flags.
  spec_lt = main %>%
    filter(Long.Term.Analysis == "X") %>%
    pull(all_of(soib_name_col))

  spec_ct = main %>%
    filter(Current.Analysis == "X") %>%
    pull(all_of(soib_name_col))


  # checkpoint-object "main"
  main1_postfilt <- main


  # Trends (long-term) ----------------------------------------

  if (run_res_trends_LTT == FALSE) {

    na_columns <- c("longtermlci", "longtermmean", "longtermrci")
    main[, na_columns] <- NA_real_

    print(glue("Skipping resolving species LTT for {cur_mask}"))

  } else {

    temp <- list.files(path = ltt_pathonly, full.names = TRUE) %>%
      map_df(read.csv) %>%
      filter(COMMON.NAME %in% spec_lt) %>%
      dplyr::select(COMMON.NAME, longtermlci, longtermmean, longtermrci)

    main <- main %>%
      left_join(temp, by = setNames("COMMON.NAME", soib_name_col))

  }

  # checkpoint-object "main"
  main2_postLTT <- main


  # Trends (current) -------------------------------------------

  if (run_res_trends_CAT == FALSE) {

    na_columns <- c("currentslopelci", "currentslopemean", "currentsloperci")
    main[, na_columns] <- NA_real_

    print(glue("Skipping resolving species CAT for {cur_mask}"))

  } else {

    temp <- list.files(path = cat_pathonly, full.names = TRUE) %>%
      map_df(read.csv) %>%
      filter(COMMON.NAME %in% spec_ct) %>%
      dplyr::select(COMMON.NAME, currentslopelci, currentslopemean, currentsloperci)

    main <- main %>%
      left_join(temp, by = setNames("COMMON.NAME", soib_name_col))

  }

  # checkpoint-object "main"
  main3_postCAT <- main


  # calculations: combining and saving full trends timeseries ---------------


  # in an interannual update, we want to rename the old trends.csv
  # to archive it for future/downstream use while latest will be saved as trends.csv

  if (interannual_update == TRUE & file.exists(trends_outpath)) {

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
  write.csv(trends, file = trends_outpath, row.names = FALSE)


  # joining future projected trends to main dataframe ###

  extra.years = soib_year_info("iucn_projection")

  tojoin <- map(extra.years, ~ trends %>%
                  filter(timegroups == .x) %>%
                  dplyr::select(COMMON.NAME, lci_std_comb, mean_std_comb, rci_std_comb) %>%
                  magrittr::set_colnames(c(soib_name_col,
                                           glue("proj{.x}.lci"),
                                           glue("proj{.x}.mean"),
                                           glue("proj{.x}.rci")))) %>%
    reduce(full_join, by = soib_name_col)

  main <- main %>%
    left_join(tojoin, by = soib_name_col) %>%
    # removing misIDd species "selection" for long-term and current analyses
    mutate(Long.Term.Analysis = ifelse(.data[[soib_name_col]] %in% spec_misid,
                                       "", Long.Term.Analysis),
           Current.Analysis = ifelse(.data[[soib_name_col]] %in% spec_misid,
                                     "", Current.Analysis))


  # checkpoint-object "main"
  main4_postCATcomb <- main


}

# saving info for trends columns
write.csv(main, file = mainwocats_path, row.names = FALSE)


