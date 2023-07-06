library(tidyverse)
library(sf)
source('00_scripts/00_functions.R')
load("01_analyses_full/specieslists.RData")
load("00_data/maps_sf.RData")
fullmap = read.csv("00_data/SoIB_mapping_2022.csv")

to.add = g1_in_sf %>% data.frame() %>% dplyr::select(GRID.G1,AREA.G1)
names(to.add) = c("gridg1","area")


# occupancy files
occ <- list.files(path = "01_analyses_full/occupancy/",  # to change into proper format
                     # Generate the full file paths
                     full.names = T) %>% 
  # Read each CSV file and combine them into a single data frame
  map_df(read.csv) %>%
  dplyr::select(-area) # remove this later because these area values are incorrect
names(occ)[names(occ) == "gridg"] = "gridg1"

# occupancy actual files
occ.act <- list.files(path = "01_analyses_full/occupancy.actual/",  # to change into proper format
                  # Generate the full file paths
                  full.names = T) %>% 
  # Read each CSV file and combine them into a single data frame
  map_df(read.csv) %>%
  dplyr::select(-area) # remove this later

occ.full1 = occ %>% left_join(occ.act) %>% filter(is.na(actual))
occ.full2 = occ.act %>% left_join(occ) %>% dplyr::select(names(occ.full1))
occ.full = rbind(occ.full1,occ.full2)
occ.full$gridg1 = as.character(occ.full$gridg1)
occ.full = occ.full %>% left_join(to.add)
occ.full$occupancy[occ.full$actual == 1] = 1
occ.full$se[occ.full$actual == 1] = 0
occ.full = occ.full %>% filter(!is.na(occupancy), !is.na(se), !is.na(gridg1))
occ.full$occupancy[occ.full$nb == 0 & occ.full$occupancy != 1] = 0
occ.full$se[occ.full$nb == 0 & occ.full$occupancy != 1] = 0


occ.summary = occ.full %>%
  group_by(COMMON.NAME,status) %>% 
  reframe(occ = sum(occupancy*area),
          occ.ci = round((erroradd(se*area))*1.96))

species = as.character(fullmap$eBird.English.Name.2022)
est = array(data=NA,dim=c(length(species),2),
            dimnames=list(species,c("occ","occ.ci")))



for (i in species)
{
  temp = occ.full %>% filter(COMMON.NAME == i)
  if (length(temp$COMMON.NAME) == 0)
    next
  
  ## files to be used for creating maps later
  write.csv(temp, paste("01_analyses_full/results/occupancy/",i,".csv",sep = ""), row.names = F)
  
  temp.sum = occ.summary %>% filter(COMMON.NAME == i)
  l = length(temp.sum$status)
  
  for (j in 1:l)
  {
    flag = 0
    
    if (temp.sum$status[j] %in% c("MP") & is.na(est[i,"occ"]))
    {
      est[i,"occ"] = temp.sum$occ[j]
      est[i,"occ.ci"] = temp.sum$occ.ci[j]
      flag = 1
    }
    
    if (temp.sum$status[j] %in% c("R","MS"))
    {
      est[i,"occ"] = temp.sum$occ[j]
      est[i,"occ.ci"] = temp.sum$occ.ci[j]
    }
    
    if (temp.sum$status[j] %in% c("M","MW") & (is.na(est[i,"occ"]) | flag == 1))
    {
      est[i,"occ"] = temp.sum$occ[j]
      est[i,"occ.ci"] = temp.sum$occ.ci[j]
    }
  }
}


estdf = data.frame(rep(rownames(est)))
names(estdf) = "eBird.English.Name.2022"
estdf$rangelci = round(as.numeric(est[,1])/10000,3) - round(as.numeric(est[,2])/10000,3)
estdf$rangemean = round(as.numeric(est[,1])/10000,3)
estdf$rangerci = round(as.numeric(est[,1])/10000,3) + round(as.numeric(est[,2])/10000,3)
estdf$rangemean[(estdf$eBird.English.Name.2022 %in% specieslist$COMMON.NAME) & is.na(estdf$rangemean)] = 0
estdf$rangelci[estdf$rangemean == 0] = 0
estdf$rangerci[estdf$rangemean == 0] = 0

# Use estdf to add rangelci, rangemean and rangerci to the main file



