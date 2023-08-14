require(tidyverse)
require(raster)
require(ggsci)
library(sf)

load("00_data/vagrantdata.RData")
load("01_analyses_full/specieslists.RData")
load("00_data/dataforanalyses_extra.RData")

load("00_data/grids_sf_nb.RData")
our_neighbours <- g1_nb_q
rm(g1_nb_r, g2_nb_q, g2_nb_r, g3_nb_q, g3_nb_r, g4_nb_q, g4_nb_r)

main = read.csv("01_analyses_full/results/SoIB_main.csv")
mig = main %>% dplyr::select(eBird.English.Name.2022,Migratory.Status.Within.India)

list.mig = mig %>%
  filter(Migratory.Status.Within.India != "Resident", 
         eBird.English.Name.2022 %in% specieslist$COMMON.NAME) %>%
  distinct(eBird.English.Name.2022) %>% pull(eBird.English.Name.2022)

data0 = data0 %>% filter(COMMON.NAME %in% list.mig, year > 2017) %>% 
  # subset for state when required
  dplyr::select(COMMON.NAME,day,gridg1)
datas = data0 %>% filter(day > 145 & day <= 215) %>% distinct(COMMON.NAME,gridg1) %>% mutate(status = "S")
dataw = data0 %>% filter(day <= 60 | day > 325) %>% distinct(COMMON.NAME,gridg1) %>% mutate(status = "W")
datap = data0 %>% filter((day > 60 & day <= 145) | (day > 215 & day <= 325)) %>% 
  distinct(COMMON.NAME,gridg1) %>% mutate(status = "P")
data_presence = datas %>% bind_rows(dataw) %>% bind_rows(datap) %>%
  dplyr::select(COMMON.NAME,status,gridg1) %>%
  mutate(prop_nb = NA, occupancy = 1) %>% mutate(gridg1 = as.numeric(gridg1))
data_presence$status = factor(data_presence$status, levels = c("S","W","P"))


d = d %>% filter(COMMON.NAME %in% list.mig, year > 2017) %>% 
  # subset for state when required
  dplyr::select(COMMON.NAME,day,LATITUDE,LONGITUDE)
ds = d %>% filter(day > 145 & day <= 215) %>% 
  distinct(COMMON.NAME,LATITUDE,LONGITUDE) %>% mutate(status = "S")
dw = d %>% filter(day <= 60 | day > 325) %>% 
  distinct(COMMON.NAME,LATITUDE,LONGITUDE) %>% mutate(status = "W")
dp = d %>% filter((day > 60 & day <= 145) | (day > 215 & day <= 325)) %>% 
  distinct(COMMON.NAME,LATITUDE,LONGITUDE) %>% mutate(status = "P")
vagrant_presence = ds %>% bind_rows(dw) %>% bind_rows(dp)



# occ.model files
occ.model <- list.files(path = "01_analyses_full/occupancy-model", 
                        # Generate the full file paths
                        full.names = T) %>% 
  # Read each CSV file and combine them into a single data frame
  map_df(read.csv)
occ.model.resident = occ.model %>%
  filter(prop_nb != 0, presence == 0, !COMMON.NAME %in% list.mig) %>%
  dplyr::select(COMMON.NAME,status,gridg1,prop_nb,occupancy)
occ.model.migrant = occ.model %>%
  filter(prop_nb != 0, presence == 0, COMMON.NAME %in% list.mig) %>%
  dplyr::select(COMMON.NAME,status,gridg1,prop_nb,occupancy) %>%
  mutate(status = NA)

# occ.presence files
occ.presence <- list.files(path = "01_analyses_full/occupancy-presence", 
                           # Generate the full file paths
                           full.names = T) %>% 
  # Read each CSV file and combine them into a single data frame
  map_df(read.csv)
listM = occ.presence %>%
  filter(status == "M") %>% distinct(COMMON.NAME) %>%
  pull(COMMON.NAME)
occ.presence.resident = occ.presence %>%
  filter(!COMMON.NAME %in% list.mig) %>%
  dplyr::select(COMMON.NAME,status,gridg1) %>%
  mutate(prop_nb = NA, occupancy = 1)


occ.resident = occ.model.resident %>%
  bind_rows(occ.presence.resident)

for (i in list.mig)
{
  temp = occ.model.migrant %>%
    filter(COMMON.NAME == i)
  temp.dat = data_presence %>%
    filter(COMMON.NAME == i)
  
  if (length(temp$COMMON.NAME) > 0)
  {
    for (j in unique(temp$gridg1))
    {
      nbs = our_neighbours[[j]]
      stats = temp.dat %>%
        filter(gridg1 %in% nbs) %>%
        count(status, sort = TRUE) %>%
        slice_max(n, with_ties = FALSE) %>%
        pull(status)
      occ.model.migrant$status[occ.model.migrant$COMMON.NAME == i & 
                                 occ.model.migrant$gridg1 == j] = as.character(stats)
    }
  }
  print(i)
}

occ.migrant = occ.model.migrant %>%
  bind_rows(data_presence)


occ.full = occ.resident %>%
  bind_rows(occ.migrant) %>%
  group_by(COMMON.NAME,status,gridg1) %>%
  #remove duplicates that come in from incidentals
  arrange(desc(occupancy), .by_group = T) %>% slice(1) %>% ungroup() %>%
  arrange(COMMON.NAME,status,desc(occupancy))


# Change everything to the four statuses of interest
occ.final = occ.full %>%
  group_by(COMMON.NAME,gridg1) %>%
  reframe(status_f = case_when(any(status == "R") ~ "YR",
                               any(status == "S") & any(status == "W") & !COMMON.NAME %in% listM ~ "YR",
                               any(status == "S") ~ "S",
                               any(status == "W") ~ "W",
                               TRUE ~ "P"),
          occupancy_f = max(occupancy)) %>%
  rename(status = status_f, occupancy = occupancy_f)

write.csv(occ.final, "01_analyses_full/data_to_plot_distribution_maps.csv", row.names = FALSE)
write.csv(vagrant_presence, "01_analyses_full/data_vagrant_for_distribution_maps.csv", row.names = FALSE)

