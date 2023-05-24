###################                       PART 1                     ###################################


## initialize and create spatialpolygon and spatialgrid databases from shapefiles
## has to be run only once unless any of these change
## requires R packages tidyverse, rgdal, sp
## district, state and country shapefiles (2011 currently) MUST BE in the working directory
## creates grids at 25, 50, 100, 200 resolutions and lists of 4 and 8 nearest neighbours
## writes 'maps.RData' and 'neighbours.RData' to the home folder (called in other functions)

## check the India Maps repo

### Sequence of steps to clean data starting from .txt file

## clean the eBird EBD, add some important columns, select only few
## has to be run after every new EBD download
## requires R packages lubridate and tidyverse
## txt data file MUST BE in the working directory
## writes 'indiaspecieslist.csv' (common and scientific names of all species)
## writes 'rawdata.RData' to the home folder

source('00_scripts/SoIBv2_functions.R')
readcleanrawdata("00_data/ebd_IN_relFeb-2023.txt","00_data/ebd_sensitive_relFeb-2023_IN.txt") 

## add map and grid variables to the dataset (dataframe)
## has to be run after the previous step
## requires R packages tidyverse, data.table, sp and rgeos
## data.RData and maps.RData files MUST BE in the working directory
## writes '00_data/data.RData' to the home folder

source('00_scripts/SoIBv2_functions.R')
addmapvars()

## clean up and filter data for analyses
## has to be run after previous step
## requires tidyverse, lubridate
## "indiaspecieslist.csv", "Activity - Activity.csv", "Migratory Status - Migratory Status.csv",
## "Endemicity - Endemicity.csv", "Select Species from List - Select Species from List.csv"
## and data.RData files MUST BE in the home folder
## writes '00_data/dataforanalyses.RData' to the home folder, workspace contains info about
## amount of data in each temporal bin, full species list (with all attribute columns) 
## and selected species list, data

source('00_scripts/SoIBv2_functions.R')
dataspeciesfilter(locationlimit = 15,gridlimit = 4,listlimit = 50)


## create random group IDs
## print data files
## run trend analyses




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


source('00_scripts/SoIBv2_functions.R')
occ = SoIBoccupancy(data,species,areag=areag1)

## for the final run, species = specieslist$COMMON.NAME (or this incrementally)

source('00_scripts/SoIBv2_functions.R')
load("00_data/dataforanalyses.RData")
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





