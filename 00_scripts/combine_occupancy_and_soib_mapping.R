# This script combines occupancy information with the SoIB mapping file 
# in the second step to the creation of the SoIB main file.
# This needs to run after combining trends

library(tidyverse)
library(glue)
library(tictoc)
library(sf)

source('00_scripts/00_functions.R')

interannual_update <- TRUE
update_occupancy <- FALSE

cur_metadata <- get_metadata(cur_mask)

# read paths
occu_pres_pathonly <- cur_metadata$OCCU.PRES.PATHONLY
occu_mod_pathonly <- cur_metadata$OCCU.MOD.PATHONLY
speclist_path <- cur_metadata$SPECLISTDATA.PATH
mainwocats_path <- file.path(
  dirname(cur_metadata$SOIBMAIN.WOCATS.PATH),
  "SoIB_main_wocats_trends.csv"
) 

# write paths
occu_outpath <- cur_metadata$OCCU.OUTPATH
mainwocats_write_path <- cur_metadata$SOIBMAIN.WOCATS.PATH

# in interannual updates, we need to delete all past-year output files
# because species names change every year with taxonomy updates.
# hence, although most species' files will simply get overwritten, for many
# species we will end up with multiple files, one for each taxonomy update
# (if not interannual update, everything will be in a new repo so no need for this.)
if (update_occupancy == TRUE) {
  
  files_to_del <- list.files(occu_outpath, full.names = TRUE)
  
  if (length(files_to_del) != 0) {
    file.remove(files_to_del)
  }
  
}


load("00_data/spec_misid.RData") # to remove from LTT and CAT "selection" later
# for occupancy
load("00_data/maps_sf.RData")
load(speclist_path)

main = read.csv(mainwocats_path)

# the taxonomy-year species-name column, e.g. "eBird.English.Name.2024",
# read off the file written by add_trends_soib_main.R rather than assumed
soib_name_col <- get_soib_name_col(main)

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
  
  # species names from the mapping-derived main object
  main_species <- main[[soib_name_col]]
  
  est = array(data = NA,
              dim = c(length(main_species), 2),
              dimnames = list(main_species, c("occ", "occ.ci")))
  
  
  # determining which range size value to use for each species based on the range size
  # estimated for each region
  
  for (i in main_species)
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
  
  
  tojoin = tibble(!!soib_name_col := rownames(est)) %>%
    mutate(rangemean = round(as.numeric(est[, 1]), 0),
           rangeci = round(as.numeric(est[, 2]), 0)) %>%
    mutate(rangelci = rangemean - rangeci,
           rangerci = rangemean + rangeci,
           rangeci = NULL) %>%
    mutate(rangemean = case_when(is.na(rangemean) &
                                   .data[[soib_name_col]] %in% specieslist$COMMON.NAME ~ 0,
                                 TRUE ~ rangemean)) %>%
    mutate(across(c("rangelci", "rangerci"), ~ case_when(rangemean == 0 ~ 0,
                                                         TRUE ~ .)))
  
  
  # joining to main object
  main <- main %>%
    left_join(tojoin, by = soib_name_col)
  
  # checkpoint-object "main"
  main5_postoccu <- main
  
  write.csv(main, file = mainwocats_write_path, row.names = F)

}

toc()
