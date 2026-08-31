## Requires cur_mask and speciesfortrends (a list of species common names)
## for this to work
## All of this should come from the config file

par_cores = 12
sims_main = 1000 # for the bootMer here, no subsampling!
sims_boot = 100 # for sensitivity

###### start of actual code

library(tidyverse)
library(lme4)
library(VGAM)
library(parallel)

source('00_scripts/00_functions.R')

cur_metadata <- get_metadata(cur_mask)

lttsens_folder <- cur_metadata$LTTSENS.FOLDER
cursens_folder <- cur_metadata$CURSENS.FOLDER 
trends_folder <- cur_metadata$TIMESERIES.FOLDER
ltt_folder <- cur_metadata$LTT.FOLDER
cat_folder <- cur_metadata$CAT.FOLDER

load(cur_metadata$SPECLISTDATA.PATH)
load(cur_metadata$DATA.PATH)

## wetland grid selection file
## details in "00_scripts/wetland_selection_check"

wetland_filter = read.csv("00_data/grid_wetland_classification_intersection_all_states_seasonal_p20.csv")

wetland_filter = wetland_filter %>%
  arrange(desc(gridg0),desc(grid_label)) %>%
  dplyr::distinct(gridg0,.keep_all = TRUE) %>%
  dplyr::distinct(gridg0,grid_label) %>%
  mutate(gridg0 = as.character(gridg0))
  
wetland_species = read.csv("00_data/soib_mapping_2025.csv") %>%
  filter(Habitat.Specialization == "Wetland") %>%
  pull(eBird.English.Name.2025)

# Create a single dataframe out of specieslist and restrictedspecieslist
# so that all information about model type is avalable in a
# single object

specieslist_fullmodel = specieslist %>%
  filter(!ht == 0 | !rt == 0) %>%
  mutate(model = "full")

specieslist_partmodel = restrictedspecieslist %>%
  filter(!ht == 0 | !rt == 0) %>%
  filter(mixed == 1) %>% dplyr::select(-mixed) %>%
  mutate(model = "part")

specieslist_glm = restrictedspecieslist %>%
  filter(!ht == 0 | !rt == 0) %>%
  filter(mixed == 0) %>% dplyr::select(-mixed) %>%
  mutate(model = "glm")

totalspecieslist = specieslist_fullmodel %>%
  bind_rows(specieslist_partmodel) %>%
  bind_rows(specieslist_glm) %>%
  mutate(order = factor(COMMON.NAME, levels = specieslist$COMMON.NAME)) %>%
  arrange(order) %>% dplyr::select(-order)

#Order according to specieslist
totalspecieslist_ordered = specieslist %>%
  dplyr::select(COMMON.NAME) %>%
  filter(COMMON.NAME %in% totalspecieslist$COMMON.NAME) %>%
  left_join(totalspecieslist)

# Entire set of time periods to use when needed
tm_full = data %>% distinct(timegroups)
# Years to project for IUCN comparison
extra.years = soib_year_info("iucn_projection")

# Create a skeleton with all years/timegroups for the final trends file
ltemp_full = databins %>% distinct(timegroups, year) %>%
  rename(timegroupsf = timegroups,
         timegroups = year) %>% 
  mutate(timegroupsf = factor(timegroupsf, 
                              levels = soib_year_info("timegroup_lab", "FALSE"))) %>%
  dplyr::select(timegroups,timegroupsf)

to_add = data.frame(timegroups = extra.years,
                    timegroupsf = as.character(extra.years))
  
ltemp_full = ltemp_full %>%
  bind_rows(to_add)

# filtering according to selected species

# tictoc::tic("across species")
#totalspecieslist_ordered = totalspecieslist_ordered %>%
#  filter(COMMON.NAME %in% speciesfortrends)

for (species in totalspecieslist_ordered$COMMON.NAME)
{
  print(paste(species,"in",cur_mask))
  
  ltemp_base = ltemp_full %>%
    mutate(COMMON.NAME = species, .before = 1)
  
  datas = data
  first_year = databins$timegroups[1]
  first_year_recent = soib_year_info("cat_years")[1]
  databins_use = databins
  
  # A flag to check whether the species needs ltt or cat
  ht = totalspecieslist_ordered[totalspecieslist_ordered$COMMON.NAME == species,]$ht
  
  if(is.na(ht))
  {
    datas = datas %>%
      filter(timegroups %in% soib_year_info("cat_years"))
    databins_use = databins %>%
      filter(timegroups %in% soib_year_info("cat_years"))
    first_year = soib_year_info("cat_years")[1]
  }
    
  # if not ht, this will be a reduced timegroups
  tm = datas %>% distinct(timegroups)
  
  # constraining by range
  datas = datas %>%
    filter(COMMON.NAME == species) %>%
    distinct(gridg3, month) %>% 
    left_join(datas) %>%
    suppressMessages()
  
  # constraining by habitat
  if (species %in% wetland_species)
  {
    datas = datas %>%
      left_join(wetland_filter) %>%
      filter(grid_label == "wetland") %>%
      dplyr::select(-grid_label)
  }

  data_freq = datas %>%
    filter(COMMON.NAME == species, ALL.SPECIES.REPORTED == 1) %>%
    group_by(timegroups,season,gridg0) %>%
    reframe(s_lists = n_distinct(group.id))
  
  # get sampling info and list length per grid
  data_samp = datas %>%
    filter(ALL.SPECIES.REPORTED == 1) %>%
    group_by(timegroups,season,gridg0,gridg1,gridg3) %>%
    reframe(n_lists = n_distinct(group.id),
            no.sp = median(no.sp))
  
  # list length for prediction
  datay = data_samp %>%
    group_by(gridg3, gridg1) %>% 
    reframe(medianlla = median(no.sp)) %>%
    group_by(gridg3) %>% 
    reframe(medianlla = mean(medianlla)) %>%
    reframe(medianlla = round(mean(medianlla)))
  
  medianlla = datay$medianlla
  gg1 = data_samp$gridg1[1]
  gg3 = data_samp$gridg3[1]
  
  # model type
  model = totalspecieslist_ordered[totalspecieslist_ordered$COMMON.NAME == species,]$model
  
  # dataframe to predict based on the model type
  ltemp = data_samp %>%
    group_by(season) %>%
    reframe(timegroups = unique(tm$timegroups)) %>%
    {
      if (model == "glm") {
        mutate(., 
               no.sp = medianlla)
      } else if (model == "part") {
        mutate(., 
               no.sp = medianlla,
               gridg1 = gg1)
      } else {
        mutate(., 
               no.sp = medianlla,
               gridg1 = gg1,
               gridg3 = gg3)
      }
    }
  
  if (!is.na(ht))
  {
    ltemp = ltemp %>%
      mutate(timegroups = factor(timegroups, 
                                  levels = soib_year_info("timegroup_lab", "FALSE"))) %>%
      arrange(season,timegroups)
  } else {
    ltemp = ltemp %>%
      mutate(timegroups = factor(timegroups, 
                                 levels = soib_year_info("cat_years"))) %>%
      arrange(season,timegroups)
  }
  
  # calculate actual frequencies that go into the response
  data_tot = data_samp %>% left_join(data_freq)  %>%
    mutate(s_lists = case_when(is.na(s_lists) ~ 0,
                               TRUE ~ as.numeric(s_lists))) %>%
    mutate(freq = s_lists/n_lists)
  
  # calculate trends
  ltemp_pred_comb = trend_calculation(my_seed = 0, sims = sims_main)
  if (is.null(ltemp_pred_comb)) next
  
  # calculate standardised long-term trends
  f1_rats_long = standarized_trends_ltt(ltemp_pred_comb)
  
  # calculate ltt
  modtrends = f1_rats_long %>%
    filter(timegroups == soib_year_info("latest_year")) %>%
    dplyr::select(lci_std, mean_std, rci_std) %>%
    rename(longtermlci = lci_std,
           longtermmean = mean_std,
           longtermrci = rci_std) %>%
    mutate(COMMON.NAME = species, .before = 1)
  
  # calculate standardised current trends
  f1_rats_recent = standarized_trends_cat()
  
  # project trends
  f1_proj = trends_projections()

  # time consuming step, five repeats of the 
  # model and bootMer for ltt sensitivity
  if(!is.na(ht)) {
    modtrends1 = ltt_sensitivity_sim(1) %>%
      mutate(COMMON.NAME = species, .before = 1)
    modtrends2 = ltt_sensitivity_sim(2) %>%
      mutate(COMMON.NAME = species, .before = 1)
    modtrends3 = ltt_sensitivity_sim(3) %>%
      mutate(COMMON.NAME = species, .before = 1)
    modtrends4 = ltt_sensitivity_sim(4) %>%
      mutate(COMMON.NAME = species, .before = 1)
    modtrends5 = ltt_sensitivity_sim(5) %>%
      mutate(COMMON.NAME = species, .before = 1)
  }
  
  # adding projected trends to recent trends
  f1_proj_tocomb = f1_proj %>%
    rename(lci_std_comb = lci_ext_std,
           mean_std_comb = mean_ext_std,
           rci_std_comb = rci_ext_std)
  
  f1_comb = f1_rats_recent %>%
    rename(lci_std_comb = lci_std_recent,
           mean_std_comb = mean_std_recent,
           rci_std_comb = rci_std_recent) %>%
    bind_rows(f1_proj_tocomb)
    
  # calculating CIs for unstandardised trends
  f1_freqs = ltemp_pred_comb %>%
    mutate(COMMON.NAME = species) %>%
    group_by(COMMON.NAME,timegroups,timegroupsf) %>%
    reframe(lci = quantile(pred, 0.025), 
            mean = median(pred), 
            rci = quantile(pred, 0.975)) %>%
    left_join(f1_rats_long) %>%
    left_join(f1_rats_recent) %>%
    suppressMessages()
  
  # tictoc::toc()
  
  f1 = ltemp_base %>%
    left_join(f1_freqs) %>%
    left_join(f1_proj) %>%
    left_join(f1_comb)
  
  # calculate cat and cat sensitivity (will produce files)
  cattrends = current_annual_trend_calculation()
  cattrends_sens = cat_sensitivity_sim()
  
  trends_path <- paste0(trends_folder,species,".csv")
  
  ltt_path <- paste0(ltt_folder,species,".csv")
  cat_path <- paste0(cat_folder,species,".csv")
  
  lttsens_species <- paste0(lttsens_folder,species,"/")
  if (!dir.exists(lttsens_species)) {dir.create(lttsens_species)}
  
  lttsens_path1 <- paste0(lttsens_species,"sim1.csv")
  lttsens_path2 <- paste0(lttsens_species,"sim2.csv")
  lttsens_path3 <- paste0(lttsens_species,"sim3.csv")
  lttsens_path4 <- paste0(lttsens_species,"sim4.csv")
  lttsens_path5 <- paste0(lttsens_species,"sim5.csv")
  
  cursens_path <- paste0(cursens_folder,species,".csv")
  
  write.csv(f1,trends_path,row.names=F)
  write.csv(modtrends,ltt_path,row.names=F)
  write.csv(cattrends,cat_path,row.names=F)
  
  if(!is.na(ht)) {
    write.csv(modtrends1,lttsens_path1,row.names=F)
    write.csv(modtrends2,lttsens_path2,row.names=F)
    write.csv(modtrends3,lttsens_path3,row.names=F)
    write.csv(modtrends4,lttsens_path4,row.names=F)
    write.csv(modtrends5,lttsens_path5,row.names=F)
  }
  
  write.csv(cattrends_sens,cursens_path,row.names=F)

}
