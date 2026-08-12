require(glue) 
require(tidyverse)
require(tictoc)

source('00_scripts/00_functions.R')


# 0. preparing data ----------------------------------------------------------

# mapping of SoIB-species-of-interest to a range of variables/classifications
# (manually created)
fullmap = read.csv("00_data/SoIB_mapping_2025.csv")


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
  pull(eBird.English.Name.2025)

# species filtered for certain habitat masks
spec_woodland = fullmap %>%
  filter(Habitat.Specialization %in% c("Forest",
                                       "Forest & Plantation")) %>%
  pull(eBird.English.Name.2025)

# we are considering cropland and ONE habitats together to classify "openland species"
spec_openland = fullmap %>%
  filter(Habitat.Specialization %in% c("Alpine & Cold Desert",
                                       "Grassland",
                                       "Grassland & Scrub",
                                       "Open Habitat")) %>%
  pull(eBird.English.Name.2025)


# 0. main data filtering -----------------------------------------------------

load("00_data/data_obs.RData")
load("00_data/data_chk.RData")

# for stats/summary of data filtering and properties at each step
stats1 = paste(nrow(data_obs),"filter 0 observations")
stats2 = paste(length(unique(data_obs$group.id)),"filter 0 unique checklists")

data_obs = data_obs %>%
  left_join(data_chk %>% dplyr::distinct(SAMPLING.EVENT.IDENTIFIER,EFFORT.DISTANCE.KM)) %>%
  # not considering travelling lists covering > 50km at all
  filter(is.na(EFFORT.DISTANCE.KM) | EFFORT.DISTANCE.KM <= 50) %>%
  dplyr::select(-EFFORT.DISTANCE.KM)

data_chk = data_chk %>%
  mutate(timegroups = case_when(year <= 1999 ~ soib_year_info("timegroup_lab")[1],
                                year > 1999 & year <= 2006 ~ soib_year_info("timegroup_lab")[2],
                                year > 2006 & year <= 2010 ~ soib_year_info("timegroup_lab")[3],
                                year > 2010 & year <= 2012 ~ soib_year_info("timegroup_lab")[4],
                                year >= 2013 ~ as.character(year))) 


# removing vagrants
data_obs = removevagrants(data_obs,data_chk)

stats3 = paste(nrow(data_obs),"filter 1 observations")
stats4 = paste(length(unique(data_obs$group.id)),"filter 1 unique checklists")
stats5 = paste(nrow(data_obs %>% 
                      left_join(data_chk %>% dplyr::distinct(SAMPLING.EVENT.IDENTIFIER,ALL.SPECIES.REPORTED)) %>%
                      filter(ALL.SPECIES.REPORTED == 1)),
               "filter 1 usable observations")
stats6 = paste(length(unique(data_chk[data_chk$ALL.SPECIES.REPORTED == 1 &
                                        data_chk$group.id %in% unique(data_obs$group.id),]$group.id)),
               "filter 1 unique complete checklists")

# removing false complete lists
data_chk = completelistcheck(data_chk)

load("00_data/vagrantdata.RData")

vagrants = d %>%
  mutate(vagrant = 1)

data_total = data_obs %>%
  mutate(vagrant = 0) %>%
  bind_rows(vagrants)

checklist_grids = data_chk %>%
  dplyr::distinct(SAMPLING.EVENT.IDENTIFIER,gridg0)

# this file is for uses outside of main eBird trends analyses, has extra columns
save(data_total, file = "00_data/dataforanalyses_extra_obs.RData")
save(data_chk, file = "00_data/dataforanalyses_extra_chk.RData")
write.csv(checklist_grids, file = "00_data/checklist_grids.csv")

data_base = data_obs %>%
  dplyr::distinct(group.id, COMMON.NAME, CATEGORY)

# creating a group.id map
data_chk = data_chk %>%
  dplyr::select(group.id,
                ALL.SPECIES.REPORTED,
                year,
                timegroups,
                season,
                LOCALITY.ID,
                gridg0,
                gridg1,
                gridg2,
                gridg3,
                gridg4,
                no.sp,
                month,
                maskWdl,
                maskCrp,
                maskOne,
                pa.name,
                ST_NM) %>%
  arrange(desc(group.id), desc(ALL.SPECIES.REPORTED)) %>%
  distinct(group.id, .keep_all = TRUE)

# 1. processing: full country -----------------------------------------------

tic("dataspeciesfilter for full country")
dataspeciesfilter(cur_mask = "none")
toc() 
# 495 sec (2023)
# 185 sec (2024)
# 104 sec (2025)
# 127 sec (2026)


# 2. processing: woodland mask ----------------------------------------------

tic("dataspeciesfilter for woodland mask")
dataspeciesfilter(cur_mask = "woodland")
toc() 
# 240 sec (2023)
# 86 sec (2024)
# 46 sec (2025)
# 55 sec (2026)


# 3. processing: cropland mask ----------------------------------------------

tic("dataspeciesfilter for cropland mask")
dataspeciesfilter(cur_mask = "cropland")
toc() 
# 60 sec (2023)
# 42 sec (2024)
# 22 sec (2025)
# 27 sec (2026)


# 4. processing: ONEland mask -----------------------------------------------

tic("dataspeciesfilter for ONEland mask")
dataspeciesfilter(cur_mask = "ONEland")
toc() 
# 60 sec (2023)
# 20 sec (2024)
# 10 sec (2025)
# 14 sec (2026)


# 5. processing: PA mask ----------------------------------------------------

tic("dataspeciesfilter for PA mask")
dataspeciesfilter(cur_mask = "PA")
toc() 
# 80 sec (2023)
# 24 sec (2024)
# 13 sec (2025)
# 15 sec (2026)


# 6. processing: states ---------------------------------------------

tic.clearlog()
tic("dataspeciesfilter for all states")

get_metadata() %>% 
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
# 253 sec (2024)
# 121 sec (2025)
# 137 sec (2026)