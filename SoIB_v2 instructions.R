###################                       PART 1                     ###################################


## initialize and create spatialpolygon and spatialgrid databases from shapefiles
## has to be run only once unless any of these change
## requires R packages tidyverse, rgdal, sp
## district, state and country shapefiles (2011 currently) MUST BE in the working directory
## creates grids at 25, 50, 100, 200 resolutions and lists of 4 and 8 nearest neighbours
## writes 'maps.RData' and 'neighbours.RData' to the home folder (called in other functions)

source('SoIB_v2 functions.R')
createmaps()

### Sequence of steps to clean data starting from .txt file

## clean the eBird EBD, add some important columns, select only few
## has to be run after every new EBD download
## requires R packages lubridate and tidyverse
## txt data file MUST BE in the working directory
## writes 'indiaspecieslist.csv' (common and scientific names of all species)
## writes 'rawdata.RData' to the home folder

source('SoIB_v2 functions.R')
readcleanrawdata("ebd_IN_relJul-2022.txt","ebd_sensitive_relMay-2022_IN.txt") 

## add map and grid variables to the dataset (dataframe)
## has to be run after the previous step
## requires R packages tidyverse, data.table, sp and rgeos
## data.RData and maps.RData files MUST BE in the working directory
## writes 'data.RData' to the home folder

source('SoIB_v2 functions.R')
addmapvars()

## clean up and filter data for analyses
## has to be run after previous step
## requires tidyverse, lubridate
## "indiaspecieslist.csv", "Activity - Activity.csv", "Migratory Status - Migratory Status.csv",
## "Endemicity - Endemicity.csv", "Select Species from List - Select Species from List.csv"
## and data.RData files MUST BE in the home folder
## writes 'dataforanalyses.RData' to the home folder, workspace contains info about
## amount of data in each temporal bin, full species list (with all attribute columns) 
## and selected species list, data

source('SoIB_v2 functions.R')
dataspeciesfilter(locationlimit = 15,gridlimit = 4)



  ###################                       PART 2                     ###################################
## provide "dataforanalyses.RData", "neighbours.RData", "indiaspecieslist.csv" and 
## "Migratory Status - Migratory Status.csv"

## ALL SUBSEQUENT ANALYSES REQUIRE DATA THAT HAS BEEN THROUGH THE PREVIOUS STEPS
## if "dataforanalyses.RData" can be loaded and "indiaspecieslist.csv" is available, part 1 not required


## provides occupancy estimates for one or many species
## select one or more Indian bird species
## requires tidyverse, reshape2, data.table, unmarked
## "neighbours.RData", "indiaspecieslist", "Migratory Status - Migratory Status.csv" 
## MUST BE in the home folder
## returns a dataframe with occupancy values


source('SoIB_v2 functions.R')
occ = SoIBoccupancy(data,species,areag=areag1)

## for the final run, species = specieslist$COMMON.NAME (or this incrementally)



## THE MAIN RATE-LIMITING STEP
## provides trends estimates for only ONE species at a time
## requires tidyverse, lme4 and VGAM
## the dataframe specieslist MUST BE present in the environment
## returns a dataframe with trend values
## with error = F, errors are not computed, function runs faster

source('SoIB_v2 functions.R')
load("dataforanalyses.RData")
species = "House Sparrow"
start = Sys.time()
trends = freqtrendssparrow(data,species,specieslist,error=T,nsim=300)
end = Sys.time()
print(end-start)

save(trends,file = "SparrowTrends.RData")

tre = freqtrends(data,species,specieslist,error=T,nsim=2)

tre = freqtrendsrestricted(data,species,restrictedspecieslist)

## this has to be run for all species in specieslist

#c = 0
#for (species in specieslist$COMMON.NAME)
#{
#  c = c + 1
#  tre = freqtrends(data,species,specieslist,error=T,nsim=1000)
#  if (c == 1)
#  {
#    trends = tre
#  }
#  if (c > 1)
#  {
#    trends = rbind(trends,tre)
#  }
#}

source('SoIB_v2 functions.R')
load("dataforanalyses.RData")
#load("finaloccupancy.RData")

#occ = SoIBoccupancy(data,species = specieslist$COMMON.NAME[1],areag = areag1)

occ = read.csv("occ.csv")
speciesleft = setdiff(specieslist$COMMON.NAME,occ$species)

#for (i in 2:length(specieslist$COMMON.NAME))
for (i in 1:length(speciesleft))
{
  #occ1 = SoIBoccupancy(data,species = specieslist$COMMON.NAME[i],areag = areag1)
  occ1 = SoIBoccupancy(data,species = speciesleft[i],areag = areag1)
  occ = rbind(occ,occ1)
  write.csv(occ,"occ.csv",row.names=FALSE)
}

for (i in 1:length(restrictedspecieslist$COMMON.NAME))
{
  species = restrictedspecieslist$COMMON.NAME[i]
  start = Sys.time()
  tre <- freqtrendsrestricted(data,species,specieslist = restrictedspecieslist,nsim = 1000)
  #tre <- freqtrends(data,species,specieslist = restrictedspecieslist)
  name = paste(species,".RData",sep="")
  save(tre,file = name)
  end = Sys.time()
  print(end-start)
}





###################                       PART 3                     ###################################

## calculates composite values
## use a dataframe called 'trends' created from multiple 'tre's
## reuires tidyverse
## no data files called in the function
## returns a single composite trend

source('SoIB_v2 functions.R')

## all information to be added as columns to trend file
## create separate composite data frames and merge








###################                       PART 4                     ###################################

## only single species for calculatetrendslope
## plots trends, provide trend data for up to 8 species, or 8 composites
## requires tidyverse
## no data files called in either function but environment MUST HAVE specieslist
## MUST HAVE a dataframe called trends which has trends for all species in 'specieslist'
## ensure that selectspecies has a maximum of 8 species

library(tidyverse)
#map = map %>%
#  select(eBird.English.Name.2018,eBird.English.Name.2019,eBird.Scientific.Name.2019,IUCN,Schedule)

init = "./All Trends"
nms = list.files(path = init)

load(paste(init,"/",nms[1], sep = ""))
trends = tre

for (i in 2:length(nms))
{
  load(paste(init,"/",nms[i], sep = ""))
  trends = rbind(trends,tre)
}

rm(list=setdiff(ls(envir = .GlobalEnv), c("trends")), pos = ".GlobalEnv")
save.image("AllTrends.RData")
#rm(list = ls(all.names = TRUE))

source('SoIB_v2 functions.R')
load("dataforanalyses.RData")
load("AllTrends.RData")

check1 = restrictedspecieslist$COMMON.NAME[!is.na(restrictedspecieslist$ht)]
check2 = restrictedspecieslist$COMMON.NAME[!is.na(restrictedspecieslist$rt)]

specieslist$rt[specieslist$COMMON.NAME %in% check2] = 1
specieslist$ht[specieslist$COMMON.NAME %in% check1] = 1
trends = trends %>% filter(species %in% specieslist$COMMON.NAME)

specs = unique(trends$species)
temp = calculatetrendslope(trends,specs[1],specieslist)

for(i in 2:length(specs))
{
  print(specs[i])
  temp1 = calculatetrendslope(trends,specs[i],specieslist)
  temp = rbind(temp,temp1)
}

glmr = temp

write.csv(glmr,"glmr.csv",row.names = F)

## this has to be run for all species in specieslist

#c = 0
#for (species in specieslist$COMMON.NAME)
#{
#  c = c + 1
#  trendslopetmp = calculatetrendslope(trends, species, specieslist, composite = F)
#  if (c == 1)
#  {
#    trendslope = trendslopetmp
#  }
#  if (c > 1)
#  {
#    trendslope = rbind(trendslope,trendslopetmp)
#  }
#}


## to plot trends for up to 8 species
load("AllTrends.RData")
source('SoIB_v2 functions.R')
library(tidyverse)

plottrends(trends, selectspecies = c("Ashy Prinia","House Sparrow","Red-necked Falcon"))


