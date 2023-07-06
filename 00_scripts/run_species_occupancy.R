## prepare data for occupancy analyses

require(tidyverse)
library(parallel)
require(foreach)
require(doParallel)

source('00_scripts/00_functions.R')

load("01_analyses_full/dataforanalyses.RData")
load("01_analyses_full/specieslists.RData")
load("00_data/neighbours.RData")

fullmap = read.csv("00_data/SoIB_mapping_2022.csv")

doublespecies0 = fullmap %>%
  filter(Migratory.Status.Within.India %in% c("Resident",
                                              "Resident & Altitudinal Migrant",
                                              "Altitudinal Migrant",
                                              "Resident (Extirpated)",
                                              "Uncertain")) %>%
  dplyr::select(eBird.English.Name.2022) %>% 
  as.vector() %>% 
  list_c()

doublespecies1 = fullmap %>% 
  filter(Migratory.Status.Within.India %in% c("Local Migrant",
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
                                              "Within-India Migrant & Winter Migrant")) %>%
  dplyr::select(eBird.English.Name.2022) %>% 
  as.vector() %>% 
  list_c()

doublespecies2 = fullmap %>% 
  filter(Migratory.Status.Within.India %in% c("Passage Migrant & Localized Summer Migrant",
                                              "Summer Migrant & Passage Migrant")) %>%
  dplyr::select(eBird.English.Name.2022) %>% 
  as.vector() %>% 
  list_c()

doublespecies3 = fullmap %>% 
  filter(Migratory.Status.Within.India %in% 
           c("Passage Migrant & Localized Winter Migrant")) %>%
  dplyr::select(eBird.English.Name.2022) %>% 
  as.vector() %>% 
  list_c()

fullmap = fullmap %>%
  mutate(status = case_when(
    eBird.English.Name.2022 %in% doublespecies0 ~ "R",
    eBird.English.Name.2022 %in% doublespecies1 ~ "MS",
    eBird.English.Name.2022 %in% doublespecies2 ~ "MP",
    eBird.English.Name.2022 %in% doublespecies3 ~ "MP",
    TRUE ~ "M"))

fullmap1 = fullmap %>% filter(eBird.English.Name.2022 %in% doublespecies1) %>%
  mutate(status = "MW")
fullmap2 = fullmap %>% filter(eBird.English.Name.2022 %in% doublespecies2) %>%
  mutate(status = "MS")
fullmap3 = fullmap %>% filter(eBird.English.Name.2022 %in% doublespecies3) %>%
  mutate(status = "MW")

fullmap = rbind(fullmap,fullmap1)
fullmap = rbind(fullmap,fullmap2)
fullmap = rbind(fullmap,fullmap3)

speciesforocc = fullmap %>%
  dplyr::select(eBird.English.Name.2022,status) %>%
  filter(eBird.English.Name.2022 %in% specieslist$COMMON.NAME)
noofspecies = length(speciesforocc$eBird.English.Name.2022)

data = data %>%
  filter(year > 2017)

data = data %>%
  mutate(OBSERVATION.COUNT = replace(OBSERVATION.COUNT, !is.na(OBSERVATION.COUNT), "1"))

data$OBSERVATION.COUNT = as.numeric(data$OBSERVATION.COUNT)


vec = 1:noofspecies
chunks = split(vec, cut(seq_along(vec),80,labels = FALSE))

for (k in 1:80)
{
  # start parallel
  n.cores = parallel::detectCores()/2 - 2
  # create the cluster
  my.cluster = parallel::makeCluster(
    n.cores, 
    type = "PSOCK"
  )
  # register it to be used by %dopar%
  doParallel::registerDoParallel(cl = my.cluster)
  
  # # check if it is registered (optional)
  # foreach::getDoParRegistered()
  # # how many workers are available? (optional)
  # foreach::getDoParWorkers()
  
  start = Sys.time()
  occu0 = foreach(i = chunks[[k]], .combine = 'rbind', .errorhandling = 'remove') %dopar%
    occupancyrun(data = data, 
                 i = i,
                 speciesforocc = speciesforocc,
                 nb8g1 = nb8g1)
  end = Sys.time()
  print(end-start)
  
  parallel::stopCluster(cl = my.cluster)
  
  print(k)
  write.csv(occu0, paste("01_analyses_full/occupancy/chunk_",k,".csv",sep=""), row.names = F)
  
  gc()
}












