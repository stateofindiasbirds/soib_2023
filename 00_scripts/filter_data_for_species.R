
require(glue) 
require(tidyverse)
require(tictoc)

source('00_scripts/00_functions.R')


# 0. preparing data ----------------------------------------------------------

# mapping of SoIB-species-of-interest to a range of variables/classifications
# (manually created)
fullmap = read.csv("00_data/SoIB_mapping_2023.csv")


# species frequently misidentified and therefore ignored in analyses ###
spec_misid <- c("Besra","Singing Bushlark","Common Flameback",
                "Eastern Orphean Warbler","Richard's Pipit",
                "Asian Palm Swift")
# saving to read in resolve step
save(spec_misid, file = "00_data/spec_misid.RData")


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
  pull(eBird.English.Name.2023)

# species filtered for certain habitat masks
spec_woodland = fullmap %>%
  filter(Habitat.Specialization %in% c("Forest",
                                       "Forest & Plantation")) %>%
  pull(eBird.English.Name.2023)

# we are considering cropland and ONE habitats together to classify "openland species"
spec_openland = fullmap %>%
  filter(Habitat.Specialization %in% c("Alpine & Cold Desert",
                                       "Grassland",
                                       "Grassland & Scrub",
                                       "Open Habitat")) %>%
  pull(eBird.English.Name.2023)


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
                                year >= 2013 ~ as.character(year))) 


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

tic("dataspeciesfilter for full country")
dataspeciesfilter(cur_mask = "none")
toc() # 495 sec


# 2. processing: woodland mask ----------------------------------------------

tic("dataspeciesfilter for woodland mask")
dataspeciesfilter(cur_mask = "woodland")
toc() # 240 sec


# 3. processing: cropland mask ----------------------------------------------

tic("dataspeciesfilter for cropland mask")
dataspeciesfilter(cur_mask = "cropland")
toc() # 60 sec


# 4. processing: ONEland mask -----------------------------------------------

tic("dataspeciesfilter for ONEland mask")
dataspeciesfilter(cur_mask = "ONEland")
toc() # 60 sec


# 5. processing: PA mask ----------------------------------------------------

tic("dataspeciesfilter for PA mask")
dataspeciesfilter(cur_mask = "PA")
toc() # 80 sec


# 6. processing: states ---------------------------------------------

tic.clearlog()
tic("dataspeciesfilter for all states")

analyses_metadata %>% 
  filter(MASK.TYPE == "state") %>% 
  distinct(MASK) %>% 
  pull(MASK) %>% 
  # walking dataspeciesfilter() over each state
  walk(~ {
    
    tic(glue("dataspeciesfilter for {.x} state"))
    dataspeciesfilter(cur_mask = .x)
    toc(log = TRUE, quiet = TRUE) 
    
  })

toc(log = TRUE, quiet = TRUE) 
tic.log()
