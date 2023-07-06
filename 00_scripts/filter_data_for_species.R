
require(glue) 
require(tidyverse)
require(tictoc)

source('00_scripts/00_functions.R')


# 0. preparing data ----------------------------------------------------------

# mapping of SoIB-species-of-interest to a range of variables/classifications
# (manually created)
fullmap = read.csv("00_data/SoIB_mapping_2022.csv")


# <annotation_pending_AV> why these species? why not
# do this step along with the step combining e.g. Green/Greenish?
# species frequently misidentified and therefore ignored in analyses ###
spec_misid <- c("Besra","Horsfield's Bushlark","Common Flameback",
                "Eastern Orphean Warbler","Richard's Pipit",
                "Asian Palm Swift")


# species info for different slices ###
spec_resident = fullmap %>%
  filter(Migratory.Status.Within.India %in% c("Resident",
                                              "Resident & Altitudinal Migrant",
                                              "Resident & Winter Migrant",
                                              "Resident & Summer Migrant",
                                              "Resident & Local Migrant",
                                              "Resident & Localized Summer Migrant",
                                              "Resident & Within-India Migrant",
                                              "Resident (Extirpated)")) %>%
  dplyr::select(eBird.English.Name.2022) %>% 
  as.vector() %>% 
  list_c()

# species filtered for certain habitat masks
spec_woodland = fullmap %>%
  filter(Habitat.Specialization %in% c("Forest",
                                       "Forest & Plantation")) %>%
  dplyr::select(eBird.English.Name.2022) %>% 
  as.vector() %>% 
  list_c()

# we are considering cropland and ONE habitats together to classify "openland species"
spec_openland = fullmap %>%
  filter(Habitat.Specialization %in% c("Alpine & Cold Desert",
                                       "Grassland",
                                       "Grassland & Scrub",
                                       "Open Habitat")) %>%
  dplyr::select(eBird.English.Name.2022) %>% 
  as.vector() %>% 
  list_c()


# 0. main data filtering -----------------------------------------------------

load("00_data/data.RData")

# for stats/summary of data filtering and properties at each step
stats1 = paste(nrow(data),"filter 0 observations")
stats2 = paste(length(unique(data$group.id)),"filter 0 unique checklists")

data = data %>%
  # not considering travelling lists covering > 50km at all
  filter(is.na(EFFORT.DISTANCE.KM) | EFFORT.DISTANCE.KM <= 50) %>%
  # data quality
  filter(REVIEWED == 0 | APPROVED == 1) %>%
  # removing exotic species observations
  filter(!EXOTIC.CODE %in% c("X"))

data = data %>%
  mutate(timegroups = case_when(year <= 1999 ~ "before 2000",
                                year > 1999 & year <= 2006 ~ "2000-2006",
                                year > 2006 & year <= 2010 ~ "2007-2010",
                                year > 2010 & year <= 2012 ~ "2011-2012",
                                year >= 2013 ~ as.character(year))) %>% 
  mutate(timegroups1 = case_when(year <= 2006 ~ "before 2006",
                                 year > 2006 & year <= 2014 ~ "2007-2014",
                                 year > 2014 & year <= 2019 ~ "2015-2019",
                                 year > 2019 ~ "2020-2022"))


# removing vagrants
data = removevagrants(data)

stats3 = paste(nrow(data),"filter 1 observations")
stats4 = paste(length(unique(data$group.id)),"filter 1 unique checklists")
stats5 = paste(nrow(data[data$ALL.SPECIES.REPORTED == 1,]),
               "filter 1 usable observations")
stats6 = paste(length(unique(data[data$ALL.SPECIES.REPORTED == 1,]$group.id)),
               "filter 1 unique complete checklists")

# removing false complete lists
data = completelistcheck(data)


data_base = data


data0 = data_base %>% dplyr::select(-REVIEWED,-APPROVED,-cyear)
# this file is for uses outside of main eBird trends analyses, has extra columns
save(data0, file = "00_data/dataforanalyses_extra.RData")


# 1. processing: full country -----------------------------------------------

tictoc::tic("dataspeciesfilter for full country")
dataspeciesfilter(cur_mask = "none")
tictoc::toc() # 423 sec


# 2. processing: woodland mask ----------------------------------------------

tictoc::tic("dataspeciesfilter for woodland mask")
dataspeciesfilter(cur_mask = "woodland")
tictoc::toc() # 211 sec


# 3. processing: cropland mask ----------------------------------------------

tictoc::tic("dataspeciesfilter for cropland mask")
dataspeciesfilter(cur_mask = "cropland")
tictoc::toc() # 60 sec


# 4. processing: ONEland mask -----------------------------------------------

tictoc::tic("dataspeciesfilter for ONEland mask")
dataspeciesfilter(cur_mask = "ONEland")
tictoc::toc() # 60 sec


# 5. processing: PA mask ----------------------------------------------------

tictoc::tic("dataspeciesfilter for PA mask")
dataspeciesfilter(cur_mask = "PA")
tictoc::toc() # 80 sec


# 6. processing: Kerala state ---------------------------------------------

tictoc::tic("dataspeciesfilter for Kerala state")
dataspeciesfilter(cur_mask = "Kerala")
tictoc::toc() # 140 sec
