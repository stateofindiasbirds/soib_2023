# preparing data for specific mask (this is the only part that changes, but automatically)
cur_metadata <- analyses_metadata %>% filter(MASK == cur_mask)
speclist_path <- cur_metadata$SPECLISTDATA.PATH
data_path <- cur_metadata$DATA.PATH


###

require(tidyverse)

source('00_scripts/00_functions.R')

load(data_path)
load(speclist_path)
specieslist = specieslist$COMMON.NAME

fullmap = read.csv("00_data/SoIB_mapping_2022.csv")


# creating new directory if it doesn't already exist
if (!dir.exists(cur_metadata$OCCU.PRES.PATHONLY)) {
  dir.create(cur_metadata$OCCU.PRES.PATHONLY, 
             recursive = T)
}


# classifying migratory status --------------------------------------------

doublespecies0 = fullmap %>%
  filter(Migratory.Status.Within.India %in% c(
    "Resident",
    "Resident & Altitudinal Migrant",
    "Altitudinal Migrant",
    "Resident (Extirpated)",
    "Uncertain"
  )) %>%
  dplyr::select(eBird.English.Name.2022) %>% 
  as.vector() %>% 
  list_c()

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
  dplyr::select(eBird.English.Name.2022) %>% 
  as.vector() %>% 
  list_c()

doublespecies2 = fullmap %>% 
  filter(Migratory.Status.Within.India %in% c(
    "Passage Migrant & Localized Summer Migrant",
    "Summer Migrant & Passage Migrant"
    )) %>%
  dplyr::select(eBird.English.Name.2022) %>% 
  as.vector() %>% 
  list_c()

doublespecies3 = fullmap %>% 
  filter(Migratory.Status.Within.India %in% c(
    "Passage Migrant & Localized Winter Migrant"
    )) %>%
  dplyr::select(eBird.English.Name.2022) %>% 
  as.vector() %>% 
  list_c()

fullmap = fullmap %>%
  mutate(status = case_when(
    eBird.English.Name.2022 %in% doublespecies0 ~ "R",
    eBird.English.Name.2022 %in% doublespecies1 ~ "MS",
    eBird.English.Name.2022 %in% doublespecies2 ~ "MP",
    eBird.English.Name.2022 %in% doublespecies3 ~ "MP",
    TRUE ~ "M"
    ))

fullmap1 = fullmap %>% 
  filter(eBird.English.Name.2022 %in% doublespecies1) %>%
  mutate(status = "MW")
fullmap2 = fullmap %>% 
  filter(eBird.English.Name.2022 %in% doublespecies2) %>%
  mutate(status = "MS")
fullmap3 = fullmap %>% 
  filter(eBird.English.Name.2022 %in% doublespecies3) %>%
  mutate(status = "MW")

fullmap = rbind(fullmap,fullmap1)
fullmap = rbind(fullmap,fullmap2)
fullmap = rbind(fullmap,fullmap3)


# calculation -------------------------------------------------------------

speciesforocc = fullmap %>%
  dplyr::select(eBird.English.Name.2022,status) %>%
  filter(eBird.English.Name.2022 %in% specieslist)
noofspecies = length(speciesforocc$eBird.English.Name.2022)

data = data %>%
  filter(year > 2017)

vec = 1:noofspecies

for (i in vec)
{
  start = Sys.time()
  species = speciesforocc$eBird.English.Name.2022[i]
  status = speciesforocc$status[i]
  
  # file names for individual files
  write_path <- cur_metadata %>% 
    dplyr::summarise(OCCU.PRES.PATH = glue("{OCCU.PRES.PATHONLY}{species}_{status}.csv")) %>% 
    as.character()
  
  if (status == "R")
  {
    datac = data
  }
  if (status == "M")
  {
    temp1 = data %>%
      filter(COMMON.NAME == species) %>%
      distinct(month)
    
    datac = temp1 %>% left_join(data)
  }
  if (status == "MP")
  {
    temp1 = data %>%
      filter(month %in% c(9:11,3:5)) %>%
      filter(COMMON.NAME == species) %>%
      distinct(month)
    
    datac = temp1 %>% left_join(data)
  }
  if (status == "MS")
  {
    temp1 = data %>%
      filter(month %in% c(5:8)) %>%
      filter(COMMON.NAME == species) %>%
      distinct(month)
    
    datac = temp1 %>% left_join(data)
  }
  if (status == "MW")
  {
    temp1 = data %>%
      filter(month %in% c(11:12,1:2)) %>%
      filter(COMMON.NAME == species) %>%
      distinct(month)
    
    datac = temp1 %>% left_join(data)
  }
  
  datac = datac %>%
    filter(COMMON.NAME == species)
  
  f2 = datac
  f2 = f2 %>% distinct(gridg1)
  f2$presence = 1
  f2$status = status
  f2$COMMON.NAME = species
  
  end = Sys.time()
  print(end-start)

  print(i)
  if (length(f2$COMMON.NAME) > 0)
  {
    write.csv(f2, file = write_path, row.names = F)
  }
  
  gc()
}
