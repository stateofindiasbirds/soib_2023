# preparing data for specific mask (this is the only part that changes, but automatically)
cur_metadata <- analyses_metadata %>% filter(MASK == cur_mask)
speclist_path <- cur_metadata$SPECLISTDATA.PATH
data_path <- cur_metadata$DATA.PATH

###

require(tidyverse)

source('00_scripts/00_functions.R')

load(data_path)
load(speclist_path)
fullmap = read.csv("00_data/SoIB_mapping_2023.csv")


# classifying migratory status --------------------------------------------

doublespecies0 = fullmap %>%
  filter(Migratory.Status.Within.India %in% c(
    "Resident",
    "Resident & Altitudinal Migrant",
    "Altitudinal Migrant",
    "Resident (Extirpated)",
    "Uncertain"
  )) %>%
  pull(eBird.English.Name.2023)

doublespecies1 = fullmap %>% 
  filter(Migratory.Status.Within.India %in% c(
    "Local Migrant",
    "Local Migrant & Summer Migrant",
    "Local Migrant & Winter Migrant",
    "Localized Summer Migrant & Localized Winter Migrant",
    "Resident & Local Migrant",
    "Resident & Localized Summer Migrant",
    "Resident & Summer Migrant",
    "Resident & Winter Migrant",
    "Resident & Within-India Migrant",
    "Summer Migrant & Localized Winter Migrant",
    "Summer Migrant & Winter Migrant",
    "Winter Migrant & Localized Summer Migrant",
    "Within-India Migrant",
    "Within-India Migrant & Winter Migrant"
  )) %>%
  pull(eBird.English.Name.2023)

doublespecies2 = fullmap %>% 
  filter(Migratory.Status.Within.India %in% c(
    "Passage Migrant & Localized Summer Migrant",
    "Summer Migrant & Passage Migrant"
  )) %>%
  pull(eBird.English.Name.2023)

doublespecies3 = fullmap %>% 
  filter(Migratory.Status.Within.India %in% c(
    "Passage Migrant & Localized Winter Migrant"
  )) %>%
  pull(eBird.English.Name.2023)

fullmap = fullmap %>%
  mutate(status = case_when(
    eBird.English.Name.2023 %in% doublespecies0 ~ "R",
    eBird.English.Name.2023 %in% doublespecies1 ~ "MS",
    eBird.English.Name.2023 %in% doublespecies2 ~ "MP",
    eBird.English.Name.2023 %in% doublespecies3 ~ "MP",
    TRUE ~ "M"
  ))

fullmap1 = fullmap %>% 
  filter(eBird.English.Name.2023 %in% doublespecies1) %>%
  mutate(status = "MW")
fullmap2 = fullmap %>% 
  filter(eBird.English.Name.2023 %in% doublespecies2) %>%
  mutate(status = "MS")
fullmap3 = fullmap %>% 
  filter(eBird.English.Name.2023 %in% doublespecies3) %>%
  mutate(status = "MW")

fullmap <- fullmap %>% bind_rows(fullmap1, fullmap2, fullmap3)


speciesforocc = fullmap %>%
  dplyr::select(eBird.English.Name.2023, status) %>%
  filter(eBird.English.Name.2023 %in% specieslist$COMMON.NAME)
noofspecies = length(speciesforocc$eBird.English.Name.2023)

data = data %>% filter(year > 2017)

vec = 1:noofspecies
