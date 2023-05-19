### readcleanrawdata ########################################

## read and clean raw data and add important columns like group id, seaonality variables
## place raw txt file (India download) in working directory 

readcleanrawdata = function(rawpath = "ebd_IN_relFeb-2023.txt", 
                            sensitivepath = "ebd_sensitive_relFeb-2023_IN.txt")
{
  require(lubridate)
  require(tidyverse)
  
  # select only necessary columns
  preimp = c("CATEGORY","COMMON.NAME","SCIENTIFIC.NAME","OBSERVATION.COUNT",
             "LOCALITY.ID","LOCALITY.TYPE","REVIEWED","APPROVED","STATE","COUNTY",
             "LATITUDE","LONGITUDE","OBSERVATION.DATE","TIME.OBSERVATIONS.STARTED","OBSERVER.ID",
             "PROTOCOL.TYPE","DURATION.MINUTES","EFFORT.DISTANCE.KM","EXOTIC.CODE",
             "NUMBER.OBSERVERS","ALL.SPECIES.REPORTED","GROUP.IDENTIFIER","SAMPLING.EVENT.IDENTIFIER")
  
  # CATEGORY - species, subspecies, hybrid, etc.; COMMON.NAME - common name of species;
  # SCIENTIFIC NAME - scientific name; OBSERVATION.COUNT - count of each species observed in a list;
  # LOCALITY.ID - unique location ID; LOCALITY.TYPE - hotspot, etc.;
  # LATITUDE and LONGITUDE - coordinates; OBSERVATION.DATE - checklist date; 
  # TIME.OBSERVATIONS.STARTED - checklist start time; OBSERVER ID - unique observer ID;
  # PROTOCOL TYPE - stationary, traveling, historical, etc.; DURATION.MINUTES - checklist duration;
  # EFFORT.DISTANCE.KM - distance traveled; NUMBER.OBSERVERS - no. of birders;
  # ALL.SPECIES.REPORTED - indicates whether a checklist is complete or not;
  # GROUP.IDENTIFIER - unique ID for every set of shared checklists (NA when not shared);
  # SAMPLING.EVENT.IDENTIFIER - unique checlist ID
  
  nms = read.delim(rawpath, nrows = 1, sep = "\t", header = T, quote = "", stringsAsFactors = F, 
                   na.strings = c(""," ",NA))
  nms = names(nms)
  nms[!(nms %in% preimp)] = "NULL"
  nms[nms %in% preimp] = NA
  
  # read data from certain columns only
  data = read.delim(rawpath, colClasses = nms, sep = "\t", header = T, quote = "", 
                    stringsAsFactors = F, na.strings = c(""," ",NA))

  # read sensitive species data
  nms1 = read.delim(sensitivepath, nrows = 1, sep = "\t", header = T, quote = "", stringsAsFactors = F, 
                    na.strings = c(""," ",NA))
  nms1 = names(nms1)
  nms1[!(nms1 %in% preimp)] = "NULL"
  nms1[nms1 %in% preimp] = NA
  

  sesp = read.delim(sensitivepath, colClasses = nms1, sep = "\t", header = T, quote = "", 
                    stringsAsFactors = F, na.strings = c(""," ",NA))

  # read sensitive species data
  
  
  # merge both data frames
  data = rbind(data,sesp)
  
  # create and write a file with common names and scientific names of all Indian species
  # useful for mapping
  temp = data %>%
    filter(REVIEWED == 0 | APPROVED == 1) %>%
    filter(!EXOTIC.CODE %in% c("X")) %>%
    filter(CATEGORY == "species" | CATEGORY == "issf") %>%
    distinct(COMMON.NAME,SCIENTIFIC.NAME)
  
  write.csv(temp,"indiaspecieslist.csv", row.names=FALSE)
  
  ## create location file for LULC
  
  locdat = data %>% distinct(LOCALITY.ID,LATITUDE,LONGITUDE)
  write.csv(locdat,"eBird_location_data.csv", row.names=FALSE)
  
  
  ## choosing important columns required for further analyses
  
  imp = c("CATEGORY","COMMON.NAME","OBSERVATION.COUNT",
          "LOCALITY.ID", "REVIEWED","APPROVED","EXOTIC.CODE",
          "LOCALITY.TYPE","STATE","COUNTY",
          "LATITUDE","LONGITUDE","OBSERVATION.DATE","TIME.OBSERVATIONS.STARTED",
          "OBSERVER.ID","PROTOCOL.TYPE",
          "DURATION.MINUTES","EFFORT.DISTANCE.KM",
          "ALL.SPECIES.REPORTED","group.id")
  

  # no of days in every month, and cumulative number
  days = c(31,28,31,30,31,30,31,31,30,31,30,31)
  cdays = c(0,31,59,90,120,151,181,212,243,273,304,334)
  
  # create a column "group.id" which can help remove duplicate checklists
  data = data %>%
    mutate(group.id = ifelse(is.na(GROUP.IDENTIFIER), SAMPLING.EVENT.IDENTIFIER, GROUP.IDENTIFIER))
  
  data = data %>%
    # need to combine several closely related species and slashes/spuhs
    # so, first changing their category to species since they will be combined next
    mutate(CATEGORY = case_when(COMMON.NAME %in% c("Green/Greenish Warbler",
                                                   "Siberian/Amur Stonechat",
                                                   "Red-necked/Little Stint",
                                                   "Western/Eastern Yellow Wagtail",
                                                   "Common/Himalayan Buzzard",
                                                   "Eurasian/Eastern Marsh-Harrier",
                                                   "Lesser/Greater Sand-Plover",
                                                   "Baikal/Spotted Bush Warbler",
                                                   "Lemon-rumped/Sichuan Leaf Warbler",
                                                   "Red-rumped/Striated Swallow",
                                                   "Bank Swallow/Pale Sand Martin",
                                                   "Riparia sp.",
                                                   "Greater/Mongolian Short-toed Lark",
                                                   "Taiga/Red-breasted Flycatcher",
                                                   "Tricolored x Chestnut Munia (hybrid)",
                                                   "Little/House Swift",
                                                   "Pin-tailed/Swinhoe's Snipe",
                                                   "Booted/Sykes's Warbler",
                                                   "Iduna sp.") ~ "species",
                                TRUE ~ CATEGORY)) %>%
    # combining species, slashes and spuhs
    mutate(COMMON.NAME = case_when(
      (COMMON.NAME == "Green Warbler" | COMMON.NAME == "Green/Greenish Warbler") ~ 
        "Greenish Warbler",
      (COMMON.NAME == "Amur Stonechat" | COMMON.NAME == "Siberian/Amur Stonechat") ~ 
        "Siberian Stonechat",
      (COMMON.NAME == "Red-necked Stint" | COMMON.NAME == "Red-necked/Little Stint") ~ 
        "Little Stint",
      (COMMON.NAME == "Eastern Yellow Wagtail" | COMMON.NAME == "Western/Eastern Yellow Wagtail") ~ 
        "Western Yellow Wagtail",
      (COMMON.NAME == "Himalayan Buzzard" | COMMON.NAME == "Common/Himalayan Buzzard") ~ 
        "Common Buzzard",
      (COMMON.NAME == "Eastern Marsh-Harrier" | COMMON.NAME == "Eurasian/Eastern Marsh-Harrier") ~ 
        "Eurasian Marsh-Harrier",
      (COMMON.NAME == "Greater Sand-Plover" | COMMON.NAME == "Lesser/Greater Sand-Plover") ~ 
        "Lesser Sand-Plover",
      (COMMON.NAME == "Baikal Bush Warbler" | COMMON.NAME == "Baikal/Spotted Bush Warbler") ~ 
        "Spotted Bush Warbler",
      (COMMON.NAME == "Sichuan Leaf Warbler" | COMMON.NAME == "Lemon-rumped/Sichuan Leaf Warbler") ~ 
        "Lemon-rumped Warbler",
      (COMMON.NAME == "Striated Swallow" | COMMON.NAME == "Red-rumped/Striated Swallow") ~ 
        "Red-rumped Swallow",
      (COMMON.NAME == "Pale Sand Martin" | COMMON.NAME == "Bank Swallow/Pale Sand Martin" | COMMON.NAME == "Riparia sp.") ~ 
        "Gray-throated Martin",
      (COMMON.NAME == "Mongolian Short-toed Lark" | COMMON.NAME == "Greater/Mongolian Short-toed Lark") ~ 
        "Greater Short-toed Lark",
      (COMMON.NAME == "Taiga Flycatcher" | COMMON.NAME == "Taiga/Red-breasted Flycatcher") ~ 
        "Red-breasted Flycatcher",
      (COMMON.NAME == "Chestnut Munia" | COMMON.NAME == "Tricolored x Chestnut Munia (hybrid)") ~ 
        "Tricolored Munia",
      (COMMON.NAME == "House Swift" | COMMON.NAME == "Little/House Swift") ~ 
        "Little Swift",
      (COMMON.NAME == "Swinhoe's Snipe" | COMMON.NAME == "Pin-tailed/Swinhoe's Snipe") ~ 
        "Pin-tailed Snipe",
      (COMMON.NAME == "Sykes's Warbler" | COMMON.NAME == "Booted/Sykes's Warbler" | COMMON.NAME == "Iduna sp.") ~ 
        "Booted Warbler",
      TRUE ~ COMMON.NAME
    ))
  
  ## setup eBird data ##
  
  ## filter species, slice by single group ID, remove repetitions
  ## remove repeats by retaining only a single group.id + species combination
  ## set date, add month, year and day columns using package LUBRIDATE
  ## add number of species/list length column (no.sp), for list length analyses (lla)
  
  
  data = data %>%
    group_by(group.id,COMMON.NAME) %>% slice(1) %>% ungroup %>%
    dplyr::select(all_of(imp)) %>%
    mutate(OBSERVATION.DATE = as.Date(OBSERVATION.DATE), 
           month = month(OBSERVATION.DATE),
           day = day(OBSERVATION.DATE) + cdays[month], 
           #week = week(OBSERVATION.DATE),
           #fort = ceiling(day/14),
           cyear = year(OBSERVATION.DATE)) %>%
    dplyr::select(-c("OBSERVATION.DATE")) %>%
    mutate(year = ifelse(month > 5, cyear, cyear-1)) %>% # from June to May
    group_by(group.id) %>% mutate(no.sp = n_distinct(COMMON.NAME)) %>%
    ungroup
  
  data = data %>% filter(year < 2023)
  names(data)[names(data) == "STATE"] = "ST_NM"
  names(data)[names(data) == "COUNTY"] = "DISTRICT"
  
  ## remove probable mistakes
  
  assign("data",data,.GlobalEnv)
  
  # save workspace
  save(data, file="rawdata.RData")
  rm(data, pos = ".GlobalEnv")
}

### createmaps ########################################


## Refer to the India Maps repository


### addmapvars ########################################


## prepare data for analyses, add map variables, grids
## place the 'maps_sf' and "grids_g0_sf" workspaces in the working directory

addmapvars = function(datapath = "rawdata.RData", 
                      mappath1 = "grids_sf_full.RData", 
                      mappath2 = "grids_g0_sf.RData",
                      mappath3 = "maps_sf.RData",
                      papath = "maps_pa_sf.RData",
                      maskspath = "habmasks_sf.RData")
{
  require(tidyverse)
  require(sf)
  
  load(datapath)
  
  # map details to add to eBird data
  load(mappath1)
  load(mappath2)
  load(mappath3)
  load(papath)
  load(maskspath)
  names(habmasks_sf)[1] = "gridg1"
  india_buff_sf$X = "X"

  sf_use_s2(FALSE)
  
  temp = data %>%
    distinct(group.id, LONGITUDE, LATITUDE) %>% 
    group_by(group.id) %>% 
    slice(1) %>% 
    ungroup() %>% 
    # joining map vars to EBD
    st_as_sf(coords = c("LONGITUDE", "LATITUDE"), remove = F) %>% 
    st_set_crs(st_crs(india_sf)) %>%
    # PAs
    st_join(pa_sf %>% dplyr::select(NAME)) %>%
    ###
    # grid cells
    st_join(g0_sf %>% dplyr::select(GRID.G0)) %>% 
    st_join(g1_sf %>% dplyr::select(GRID.G1)) %>% 
    st_join(g2_sf %>% dplyr::select(GRID.G2)) %>% 
    st_join(g3_sf %>% dplyr::select(GRID.G3)) %>% 
    st_join(g4_sf %>% dplyr::select(GRID.G4)) %>% 
    st_join(india_buff_sf %>% dplyr::select(X)) %>% 
    st_drop_geometry()
  
  temp = temp %>% 
    distinct(NAME,GRID.G0,GRID.G1,GRID.G2,GRID.G3,GRID.G4,group.id,X) %>% 
    group_by(group.id) %>% 
    slice(1) %>% 
    ungroup()
  names(temp) = c("pa.name","gridg0","gridg1","gridg2","gridg3","gridg4","group.id","X")
  
  data = data %>%
    left_join(temp)
  
  data = data %>%
    filter(!is.na(X)) %>%
    select(-X)
  
  data = left_join(data,habmasks_sf)
  
  ### 
  
  assign("data",data,.GlobalEnv)

  save(data, file="data.RData")
  rm(data, pos = ".GlobalEnv")
}


### completelistcheck ########################################

## remove all probable errors
## type can be "trends" or "range"
## to use in dataspeciesfilter()

completelistcheck = function(data)
{
  require(tidyverse)
  require(lubridate)
  
  # create 2 columns from the "TIME.OBSERVATIONS.STARTED' column
  temp = data.frame(data$TIME.OBSERVATIONS.STARTED)
  temp = temp %>%
    separate(data.TIME.OBSERVATIONS.STARTED, c("hr","min"))
  data = cbind(data,temp)
  
  # calculate speed and species/unit time (sut)
  data = data %>%
    mutate(speed = EFFORT.DISTANCE.KM*60/DURATION.MINUTES,
           sut = no.sp*60/DURATION.MINUTES) %>%
    mutate(hr = as.numeric(hr), min = as.numeric(min)) %>%
    mutate(end = floor((hr*60+min+DURATION.MINUTES)/60)) # caluclate time checklist ended
  
  temp = data %>%
    filter(ALL.SPECIES.REPORTED == 1, PROTOCOL.TYPE != "Incidental") %>%
    group_by(group.id) %>% slice(1)
  
  # exclude any list that may in fact be incomplete
  # set threshholds for speed and sut
  
  vel = 20
  time = 2
  
  # choose checklists without info on duration with 3 or fewers species
  grp = temp %>%
    filter(no.sp <= 3, is.na(DURATION.MINUTES)) %>%
    distinct(group.id)
  grp = grp$group.id
  
  # exclude records based on verious criteria 
  data = data %>%
    mutate(ALL.SPECIES.REPORTED = 
             case_when(ALL.SPECIES.REPORTED == 1 & (EFFORT.DISTANCE.KM > 10 | group.id %in% grp | 
                                                      speed > vel | (sut < time & no.sp <= 3) | 
                                                      PROTOCOL.TYPE == "Incidental" | 
                                                      (!is.na(hr) & ((hr <= 4 & end <= 4) | 
                                                                       (hr >= 20 & end <= 28)))) ~ 0, 
                       ALL.SPECIES.REPORTED == 0 ~ 0,
                       TRUE ~ 1))
  
  data = data %>%
    select(-speed,-sut,-hr,-min,-end)
}

### removevagrants ########################################

## remove vagrants
## to use in dataspeciesfilter()

removevagrants = function(data)
{
  fullmap = read.csv("SoIB_mapping_2022.csv")
  migspecies = fullmap %>%
    filter(!Migratory.Status.Within.India %in% c("Resident",
                                                 "Resident & Altitudinal Migrant",
                                                 "Resident & Winter Migrant",
                                                 "Resident & Summer Migrant",
                                                 "Resident & Local Migrant",
                                                 "Resident & Localized Summer Migrant",
                                                 "Resident & Within-India Migrant",
                                                 "Resident (Extirpated)")) %>%
    select(eBird.English.Name.2022)
  migspecies = as.vector(migspecies$eBird.English.Name.2022)
  
  d = data %>%
    filter(COMMON.NAME %in% migspecies) %>%
    group_by(gridg4,month,COMMON.NAME) %>% reframe(nyear = n_distinct(year)) %>%
    filter(nyear <= 3) %>% select(gridg4,month,COMMON.NAME)
  
  d = left_join(d,data)
  d = d %>%
    filter(year > 2014)
  
  save(d,file = "vagrantdata.RData")
  
  data = anti_join(data,d)
  return(data)
}



### dataspeciesfilter ########################################

## select species for State of India's Birds, and species for historical and recent trends
## includes all diurnal endemics (endemicity) and essential species (SelectSpecies)

dataspeciesfilter = function(datapath = "data.RData",
                             locationlimit = 15,gridlimit = 4,listlimit = 50)
{
  require(tidyverse)
  require(DataCombine)

  
  fullmap = read.csv("SoIB_mapping_2022.csv")
  resspecies = fullmap %>%
    filter(Migratory.Status.Within.India %in% c("Resident",
                                                "Resident & Altitudinal Migrant",
                                                "Resident & Winter Migrant",
                                                "Resident & Summer Migrant",
                                                "Resident & Local Migrant",
                                                "Resident & Localized Summer Migrant",
                                                "Resident & Within-India Migrant",
                                                "Resident (Extirpated)")) %>%
    select(eBird.English.Name.2022)
  resident = as.vector(resspecies$eBird.English.Name.2022)
  
  wdlspecies = fullmap %>%
    filter(Habitat.Specialization %in% c("Forest",
                                         "Forest & Plantation")) %>%
    select(eBird.English.Name.2022)
  woodland = as.vector(wdlspecies$eBird.English.Name.2022)
  
  opnspecies = fullmap %>%
    filter(Habitat.Specialization %in% c("Alpine & Cold Desert",
                                         "Grassland",
                                         "Grassland & Scrub",
                                         "Open Habitat")) %>%
    select(eBird.English.Name.2022)
  openland = as.vector(opnspecies$eBird.English.Name.2022)
  
  
  load(datapath)
  
  x1 = paste(nrow(data),"filter 0 observations")
  x2 = paste(length(unique(data$group.id)),"filter 0 unique checklists")
  
  data$gridg0 = as.character(data$gridg0)
  data$gridg1 = as.character(data$gridg1)
  data$gridg2 = as.character(data$gridg2)
  data$gridg3 = as.character(data$gridg3)
  data$gridg4 = as.character(data$gridg4)

  data = data %>%
    filter(is.na(EFFORT.DISTANCE.KM) | EFFORT.DISTANCE.KM <= 50) %>%
    filter(REVIEWED == 0 | APPROVED == 1) %>%
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
  
  data = removevagrants(data)

  x3 = paste(nrow(data),"filter 1 observations")
  x4 = paste(length(unique(data$group.id)),"filter 1 unique checklists")
  x5 = paste(nrow(data[data$ALL.SPECIES.REPORTED == 1,]),
             "filter 1 usable observations")
  x6 = paste(length(unique(data[data$ALL.SPECIES.REPORTED == 1,]$group.id)),
             "filter 1 unique complete checklists")
  
  data = completelistcheck(data)
  
  data_base = data
  
  data0 = data_base %>% select(-REVIEWED,-APPROVED,-cyear)
  save(data0,file = "dataforanalyses_extra.RData")
  
  
  #### full country
  
  data0 = data_base
  data = data0 %>% select(-CATEGORY,-LOCALITY.ID,-REVIEWED,-APPROVED,-ST_NM,-DISTRICT,
                         -LOCALITY.TYPE,-pa.name,-maskWdl,-maskCrp,-maskOne,
                         -LATITUDE,-LONGITUDE,-TIME.OBSERVATIONS.STARTED,-PROTOCOL.TYPE,
                         -DURATION.MINUTES,-EFFORT.DISTANCE.KM,-day,-cyear,-EXOTIC.CODE)

  x7 = paste(nrow(data[data$ALL.SPECIES.REPORTED == 1,]),
             "filter 1 usable observations")
  x8 = paste(length(unique(data[data$ALL.SPECIES.REPORTED == 1,]$group.id)),
             "filter 2 unique complete checklists")
  x9 = paste(length(unique(data[data$timegroups == "before 2000" &
                                  data$ALL.SPECIES.REPORTED == 1,]$group.id)),
             "pre-2000 checklists")
  
  databins = data %>%
    filter(ALL.SPECIES.REPORTED == 1) %>%
    group_by(timegroups) %>% reframe(lists = n_distinct(group.id), year = round(median(year))) %>%
    arrange(year)

  datah = data0 %>%
    filter(ALL.SPECIES.REPORTED == 1, CATEGORY == "species" | CATEGORY == "issf") %>%
    group_by(COMMON.NAME,timegroups) %>% reframe(locs = n_distinct(LOCALITY.ID), 
                                                   cells = n_distinct(gridg4)) %>%
    group_by(COMMON.NAME,timegroups) %>%
    filter(locs > locationlimit, cells > gridlimit) %>%
    group_by(COMMON.NAME) %>% reframe(years = n()) %>%
    group_by(COMMON.NAME) %>%
    filter(years == 14) %>%
    mutate(ht = 1) %>% select (COMMON.NAME,ht)
  
  datar = data0 %>%
    filter(ALL.SPECIES.REPORTED == 1, CATEGORY == "species" | CATEGORY == "issf", year > 2014) %>%
    group_by(COMMON.NAME,year) %>% reframe(locs = n_distinct(LOCALITY.ID), 
                                             cells = n_distinct(gridg4)) %>%
    group_by(COMMON.NAME,year) %>%
    filter(locs > locationlimit, cells > gridlimit) %>%
    group_by(COMMON.NAME) %>% reframe(years = n()) %>%
    group_by(COMMON.NAME) %>%
    filter(years == 8) %>%
    mutate(rt = 1) %>% select(COMMON.NAME,rt)
  
  dataresth1 = data0 %>%
    filter(ALL.SPECIES.REPORTED == 1, CATEGORY == "species" | CATEGORY == "issf") %>%
    group_by(COMMON.NAME,timegroups) %>% reframe(cells = n_distinct(gridg4)) %>%
    group_by(COMMON.NAME,timegroups) %>%
    filter(cells <= gridlimit) %>%
    group_by(COMMON.NAME) %>% reframe(years = n()) %>%
    group_by(COMMON.NAME) %>%
    filter(years == 14) %>%
    select (COMMON.NAME)
  
  speciesresth = data.frame(species = intersect(unique(dataresth1$COMMON.NAME),resident))
  speciesresth$validh = NA
  
  for (i in 1:length(speciesresth$species))
  {
    tempresth1 = data0 %>%
      filter(COMMON.NAME == speciesresth$species[i]) %>%
      distinct(gridg1)
    tempresth1 = tempresth1 %>% left_join(data0)
    tempresth1 = tempresth1 %>%
      group_by(timegroups) %>% reframe(n = n_distinct(group.id)) %>%
      group_by(timegroups) %>%
      filter(n > listlimit)
    if (length(tempresth1$timegroups) == 14)
      speciesresth$validh[speciesresth$species == speciesresth$species[i]] = 1
  }
  
  datarestr1 = data0 %>%
    filter(ALL.SPECIES.REPORTED == 1, CATEGORY == "species" | CATEGORY == "issf", year > 2014) %>%
    group_by(COMMON.NAME,timegroups) %>% reframe(cells = n_distinct(gridg4)) %>%
    group_by(COMMON.NAME,timegroups) %>%
    filter(cells <= gridlimit) %>%
    group_by(COMMON.NAME) %>% reframe(years = n()) %>%
    group_by(COMMON.NAME) %>%
    filter(years == 8) %>%
    select (COMMON.NAME)
  
  speciesrestr = data.frame(species = intersect(unique(datarestr1$COMMON.NAME),resident))
  speciesrestr$validr = NA
  
  for (i in 1:length(unique(datarestr1$COMMON.NAME)))
  {
    temprestr1 = data0 %>%
      filter(COMMON.NAME == speciesrestr$species[i]) %>%
      distinct(gridg1)
    temprestr1 = temprestr1 %>% left_join(data0)
    temprestr1 = temprestr1 %>%
      filter(year > 2014) %>%
      group_by(timegroups) %>% reframe(n = n_distinct(group.id)) %>%
      group_by(timegroups) %>%
      filter(n > listlimit)
    if (length(temprestr1$timegroups) == 8)
      speciesrestr$validr[speciesrestr$species == speciesrestr$species[i]] = 1
  }
  
  dataf = fullmap
  names(dataf)[1:2] = c("COMMON.NAME","SCIENTIFIC.NAME")
  
  dataf = left_join(dataf,datah,by = c("COMMON.NAME"))
  dataf = left_join(dataf,datar,by = c("COMMON.NAME"))

  specieslist = dataf %>%
    filter((Essential == 1 | Endemic.Region != "None" | 
              ht == 1 | rt == 1) & 
             (Breeding.Activity.Period != "Nocturnal" | 
              Non.Breeding.Activity.Period != "Nocturnal" | 
              COMMON.NAME == "Jerdon's Courser") & 
             (is.na(Discard))) %>%
    select(COMMON.NAME,ht,rt)
  dataf$ht[dataf$Breeding.Activity.Period == "Nocturnal" &
            dataf$Non.Breeding.Activity.Period == "Nocturnal"] = NA
  dataf$rt[dataf$Breeding.Activity.Period == "Nocturnal" &
             dataf$Non.Breeding.Activity.Period == "Nocturnal"] = NA
  
  specieslist$ht[specieslist$COMMON.NAME %in% c("Besra","Horsfield's Bushlark","Common Flameback",
                                                "Eastern Orphean Warbler","Richard's Pipit")] = NA
  specieslist$rt[specieslist$COMMON.NAME %in% c("Besra","Horsfield's Bushlark","Common Flameback",
                                                "Eastern Orphean Warbler","Richard's Pipit")] = NA
  
  restrictedspecieslist  = data.frame(species = specieslist$COMMON.NAME)
  restrictedspecieslist = left_join(restrictedspecieslist,speciesresth)
  restrictedspecieslist = left_join(restrictedspecieslist,speciesrestr)
  
  restrictedspecieslist = restrictedspecieslist %>%
    filter(!is.na(validh) | !is.na(validr))

  names(restrictedspecieslist) = c("COMMON.NAME","ht","rt")
  
  restrictedspecieslist$ht[restrictedspecieslist$COMMON.NAME %in% 
                             c("Besra","Horsfield's Bushlark","Common Flameback",
                               "Eastern Orphean Warbler","Richard's Pipit")] = NA
  restrictedspecieslist$rt[restrictedspecieslist$COMMON.NAME %in% 
                             c("Besra","Horsfield's Bushlark","Common Flameback",
                               "Eastern Orphean Warbler","Richard's Pipit")] = NA
  
  check1 = restrictedspecieslist$COMMON.NAME[!is.na(restrictedspecieslist$ht)]
  check2 = restrictedspecieslist$COMMON.NAME[!is.na(restrictedspecieslist$rt)]
  
  randomcheck_a = data0 %>% filter(ALL.SPECIES.REPORTED == 1, 
                                COMMON.NAME %in% restrictedspecieslist$COMMON.NAME) %>%
    group_by(COMMON.NAME) %>% reframe(n = n_distinct(gridg1)) %>%
    group_by(COMMON.NAME) %>% filter(n > 7)
  
  randomcheck_b = data0 %>% filter(ALL.SPECIES.REPORTED == 1, 
                                   COMMON.NAME %in% restrictedspecieslist$COMMON.NAME) %>%
    group_by(COMMON.NAME) %>% reframe(n = n_distinct(gridg1)) %>%
    group_by(COMMON.NAME) %>% filter(n <= 7)
  
  restrictedspecieslist_a = restrictedspecieslist %>% filter(COMMON.NAME %in% randomcheck_a$COMMON.NAME)
  restrictedspecieslist_a$mixed = 1
  restrictedspecieslist_b = restrictedspecieslist %>% filter(COMMON.NAME %in% randomcheck_b$COMMON.NAME)
  restrictedspecieslist_b$mixed = 0
  
  restrictedspecieslist = rbind(restrictedspecieslist_a,restrictedspecieslist_b)
  
  t1 = dataf %>%
    filter((ht == 1 | rt == 1) & (Breeding.Activity.Period != "Nocturnal" |
                                  Non.Breeding.Activity.Period != "Nocturnal"))
  x10 = paste(length(t1$COMMON.NAME),"filter 1 number of species")
  t2 = dataf %>%
    filter((Endemic.Region != "None" | 
              ht == 1 | rt == 1) & (Breeding.Activity.Period != "Nocturnal" |
                                      Non.Breeding.Activity.Period != "Nocturnal"))
  x11 = paste(length(t2$COMMON.NAME),"filter 2 number of species")
  t3 = dataf %>%
    filter((Essential == 1 | Endemic.Region != "None" | 
              ht == 1 | rt == 1) & (Breeding.Activity.Period != "Nocturnal" |
                                      Non.Breeding.Activity.Period != "Nocturnal"))
  x12 = paste(length(t3$COMMON.NAME),"filter 3 number of species")
  
  specieslist1 = specieslist
  specieslist1$selected = 1
  specieslist1 = specieslist1 %>% select(COMMON.NAME,selected)
  
  dataf = dataf %>%
    select(COMMON.NAME,SCIENTIFIC.NAME,ht,rt)
  
  dataf = left_join(dataf,specieslist1)
  names(dataf) = c("COMMON.NAME","SCIENTIFIC.NAME","Long.Term.Analysis","Current.Analysis",
                   "Selected.SOIB")
  
  dataf[is.na(dataf)] = ""
  dataf[dataf == 1] = "X"
  
  dataf$Long.Term.Analysis[dataf$COMMON.NAME %in% check1] = "X"
  dataf$Current.Analysis[dataf$COMMON.NAME %in% check2] = "X"
  
  dataf = dataf %>% select("COMMON.NAME","SCIENTIFIC.NAME","Long.Term.Analysis","Current.Analysis",
                           "Selected.SOIB")
  
  sampledcells = c(length(unique(data0$gridg0)),length(unique(data0$gridg1)),
                   length(unique(data0$gridg2)),length(unique(data0$gridg3)),
                   length(unique(data0$gridg4)))
  
  stats = c(x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12)
  
  #### additional filtering safeguards - proportion of range sampled during every timegroup
  
  totalrange = data %>%
    filter(COMMON.NAME %in% dataf$COMMON.NAME, ALL.SPECIES.REPORTED == 1) %>%
    group_by(COMMON.NAME) %>% reframe(totalrange25km = n_distinct(gridg1))
  
  proprange2000 = data %>%
    filter(COMMON.NAME %in% dataf$COMMON.NAME, ALL.SPECIES.REPORTED == 1) %>%
    filter(timegroups == "before 2000") %>%
    group_by(COMMON.NAME) %>% reframe(proprange25km2000 = n_distinct(gridg1))
  
  proprange2022 = data %>%
    filter(COMMON.NAME %in% dataf$COMMON.NAME, ALL.SPECIES.REPORTED == 1) %>%
    filter(timegroups == "2022") %>%
    group_by(COMMON.NAME) %>% reframe(proprange25km2022 = n_distinct(gridg1))
  
  proprange.current = data %>%
    filter(COMMON.NAME %in% dataf$COMMON.NAME, ALL.SPECIES.REPORTED == 1) %>%
    filter(timegroups %in% c("2015","2016","2017","2018","2019","2020","2021","2022")) %>%
    group_by(COMMON.NAME, timegroups) %>% reframe(proprange25km.current = n_distinct(gridg1)) %>%
    group_by(COMMON.NAME) %>% reframe(proprange25km.current = mean(proprange25km.current))
  
  range25km = left_join(totalrange,proprange2000)
  range25km = left_join(range25km,proprange.current)
  range25km = left_join(range25km,proprange2022)
  range25km = range25km %>%
    mutate(proprange25km2000 = proprange25km2000/totalrange25km,
           proprange25km.current = proprange25km.current/totalrange25km,
           proprange25km2022 = proprange25km2022/totalrange25km)

  
  #### additional filtering safeguards - proportional sampling within each 25km grid cell
  
  samp5km = data %>%
    filter(ALL.SPECIES.REPORTED == 1) %>%
    group_by(gridg1) %>% reframe(n = n_distinct(gridg0))
  
  spec25km = data %>%
    filter(ALL.SPECIES.REPORTED == 1) %>%
    filter(COMMON.NAME %in% dataf$COMMON.NAME) %>%
    distinct(COMMON.NAME,gridg1)
  
  samp25km5km = spec25km %>% left_join(samp5km) %>%
    group_by(COMMON.NAME) %>% reframe(mean5km = mean(n), ci5km = 1.96*sd(n)/sqrt(n()))
  
  dataf = dataf %>% left_join(range25km)
  dataf = dataf %>% left_join(samp25km5km)
  

  write.csv(dataf,"fullspecieslist.csv",row.names = F)
  
  locs_write = data0 %>% 
    filter(ALL.SPECIES.REPORTED == 1) %>%
    distinct(LOCALITY.ID,group.id,month,timegroups)
  write.csv(locs_write,"sub_samp_locs.csv",row.names = F)
  
  save(specieslist,restrictedspecieslist,databins,file = "specieslists.RData")
  save(data,sampledcells,databins,stats,file = "dataforanalyses.RData")
  
  
  
  
  
  
  
  #### woodland mask
  
  data0 = data_base %>% filter(maskWdl == 1)
  data = data0 %>% select(-CATEGORY,-LOCALITY.ID,-REVIEWED,-APPROVED,-ST_NM,-DISTRICT,
                           -LOCALITY.TYPE,-pa.name,-maskWdl,-maskCrp,-maskOne,
                           -LATITUDE,-LONGITUDE,-TIME.OBSERVATIONS.STARTED,-PROTOCOL.TYPE,
                           -DURATION.MINUTES,-EFFORT.DISTANCE.KM,-day,-cyear,-EXOTIC.CODE)
  
  x7 = paste(nrow(data[data$ALL.SPECIES.REPORTED == 1,]),
             "filter 1 usable observations")
  x8 = paste(length(unique(data[data$ALL.SPECIES.REPORTED == 1,]$group.id)),
             "filter 2 unique complete checklists")
  x9 = paste(length(unique(data[data$timegroups == "before 2000" &
                                   data$ALL.SPECIES.REPORTED == 1,]$group.id)),
             "pre-2000 checklists")
  
  databins = data %>%
    filter(ALL.SPECIES.REPORTED == 1) %>%
    group_by(timegroups) %>% reframe(lists = n_distinct(group.id), year = round(median(year))) %>%
    arrange(year)
  
  datah = data0 %>%
    filter(ALL.SPECIES.REPORTED == 1, CATEGORY == "species" | CATEGORY == "issf") %>%
    group_by(COMMON.NAME,timegroups) %>% reframe(locs = n_distinct(LOCALITY.ID), 
                                                 cells = n_distinct(gridg4)) %>%
    group_by(COMMON.NAME,timegroups) %>%
    filter(locs > locationlimit, cells > gridlimit) %>%
    group_by(COMMON.NAME) %>% reframe(years = n()) %>%
    group_by(COMMON.NAME) %>%
    filter(years == 14) %>%
    mutate(ht = 1) %>% select (COMMON.NAME,ht)
  
  datar = data0 %>%
    filter(ALL.SPECIES.REPORTED == 1, CATEGORY == "species" | CATEGORY == "issf", year > 2014) %>%
    group_by(COMMON.NAME,year) %>% reframe(locs = n_distinct(LOCALITY.ID), 
                                           cells = n_distinct(gridg4)) %>%
    group_by(COMMON.NAME,year) %>%
    filter(locs > locationlimit, cells > gridlimit) %>%
    group_by(COMMON.NAME) %>% reframe(years = n()) %>%
    group_by(COMMON.NAME) %>%
    filter(years == 8) %>%
    mutate(rt = 1) %>% select(COMMON.NAME,rt)
  
  dataresth1 = data0 %>%
    filter(ALL.SPECIES.REPORTED == 1, CATEGORY == "species" | CATEGORY == "issf") %>%
    group_by(COMMON.NAME,timegroups) %>% reframe(cells = n_distinct(gridg4)) %>%
    group_by(COMMON.NAME,timegroups) %>%
    filter(cells <= gridlimit) %>%
    group_by(COMMON.NAME) %>% reframe(years = n()) %>%
    group_by(COMMON.NAME) %>%
    filter(years == 14) %>%
    select (COMMON.NAME)
  
  speciesresth = data.frame(species = intersect(unique(dataresth1$COMMON.NAME),resident))
  speciesresth$validh = NA
  
  for (i in 1:length(speciesresth$species))
  {
    tempresth1 = data0 %>%
      filter(COMMON.NAME == speciesresth$species[i]) %>%
      distinct(gridg1)
    tempresth1 = tempresth1 %>% left_join(data0)
    tempresth1 = tempresth1 %>%
      group_by(timegroups) %>% reframe(n = n_distinct(group.id)) %>%
      group_by(timegroups) %>%
      filter(n > listlimit)
    if (length(tempresth1$timegroups) == 14)
      speciesresth$validh[speciesresth$species == speciesresth$species[i]] = 1
  }
  
  datarestr1 = data0 %>%
    filter(ALL.SPECIES.REPORTED == 1, CATEGORY == "species" | CATEGORY == "issf", year > 2014) %>%
    group_by(COMMON.NAME,timegroups) %>% reframe(cells = n_distinct(gridg4)) %>%
    group_by(COMMON.NAME,timegroups) %>%
    filter(cells <= gridlimit) %>%
    group_by(COMMON.NAME) %>% reframe(years = n()) %>%
    group_by(COMMON.NAME) %>%
    filter(years == 8) %>%
    select (COMMON.NAME)
  
  speciesrestr = data.frame(species = intersect(unique(datarestr1$COMMON.NAME),resident))
  speciesrestr$validr = NA
  
  for (i in 1:length(unique(datarestr1$COMMON.NAME)))
  {
    temprestr1 = data0 %>%
      filter(COMMON.NAME == speciesrestr$species[i]) %>%
      distinct(gridg1)
    temprestr1 = temprestr1 %>% left_join(data0)
    temprestr1 = temprestr1 %>%
      filter(year > 2014) %>%
      group_by(timegroups) %>% reframe(n = n_distinct(group.id)) %>%
      group_by(timegroups) %>%
      filter(n > listlimit)
    if (length(temprestr1$timegroups) == 8)
      speciesrestr$validr[speciesrestr$species == speciesrestr$species[i]] = 1
  }
  
  dataf = fullmap
  names(dataf)[1:2] = c("COMMON.NAME","SCIENTIFIC.NAME")
  
  dataf = left_join(dataf,datah,by = c("COMMON.NAME"))
  dataf = left_join(dataf,datar,by = c("COMMON.NAME"))
  
  specieslist = dataf %>%
    filter((Essential == 1 | Endemic.Region != "None" | 
              ht == 1 | rt == 1) & 
             (Breeding.Activity.Period != "Nocturnal" | 
                Non.Breeding.Activity.Period != "Nocturnal" | 
                COMMON.NAME == "Jerdon's Courser") & 
             (is.na(Discard))) %>%
    select(COMMON.NAME,ht,rt)
  dataf$ht[dataf$Breeding.Activity.Period == "Nocturnal" &
             dataf$Non.Breeding.Activity.Period == "Nocturnal"] = NA
  dataf$rt[dataf$Breeding.Activity.Period == "Nocturnal" &
             dataf$Non.Breeding.Activity.Period == "Nocturnal"] = NA
  
  specieslist$ht[specieslist$COMMON.NAME %in% c("Besra","Horsfield's Bushlark","Common Flameback",
                                                "Eastern Orphean Warbler","Richard's Pipit")] = NA
  specieslist$rt[specieslist$COMMON.NAME %in% c("Besra","Horsfield's Bushlark","Common Flameback",
                                                "Eastern Orphean Warbler","Richard's Pipit")] = NA
  
  restrictedspecieslist  = data.frame(species = specieslist$COMMON.NAME)
  restrictedspecieslist = left_join(restrictedspecieslist,speciesresth)
  restrictedspecieslist = left_join(restrictedspecieslist,speciesrestr)
  
  restrictedspecieslist = restrictedspecieslist %>%
    filter(!is.na(validh) | !is.na(validr))
  
  names(restrictedspecieslist) = c("COMMON.NAME","ht","rt")
  
  restrictedspecieslist$ht[restrictedspecieslist$COMMON.NAME %in% 
                             c("Besra","Horsfield's Bushlark","Common Flameback",
                               "Eastern Orphean Warbler","Richard's Pipit")] = NA
  restrictedspecieslist$rt[restrictedspecieslist$COMMON.NAME %in% 
                             c("Besra","Horsfield's Bushlark","Common Flameback",
                               "Eastern Orphean Warbler","Richard's Pipit")] = NA
  
  # select only forest species
  specieslist$ht[!specieslist$COMMON.NAME %in% woodland] = NA
  specieslist$rt[!specieslist$COMMON.NAME %in% woodland] = NA
  
  restrictedspecieslist$ht[!restrictedspecieslist$COMMON.NAME %in% woodland] = NA
  restrictedspecieslist$rt[!restrictedspecieslist$COMMON.NAME %in% woodland] = NA
  
  
  check1 = restrictedspecieslist$COMMON.NAME[!is.na(restrictedspecieslist$ht)]
  check2 = restrictedspecieslist$COMMON.NAME[!is.na(restrictedspecieslist$rt)]
  
  randomcheck_a = data0 %>% filter(ALL.SPECIES.REPORTED == 1, 
                                   COMMON.NAME %in% restrictedspecieslist$COMMON.NAME) %>%
    group_by(COMMON.NAME) %>% reframe(n = n_distinct(gridg1)) %>%
    group_by(COMMON.NAME) %>% filter(n > 7)
  
  randomcheck_b = data0 %>% filter(ALL.SPECIES.REPORTED == 1, 
                                   COMMON.NAME %in% restrictedspecieslist$COMMON.NAME) %>%
    group_by(COMMON.NAME) %>% reframe(n = n_distinct(gridg1)) %>%
    group_by(COMMON.NAME) %>% filter(n <= 7)
  
  restrictedspecieslist_a = restrictedspecieslist %>% filter(COMMON.NAME %in% randomcheck_a$COMMON.NAME)
  restrictedspecieslist_a$mixed = 1
  restrictedspecieslist_b = restrictedspecieslist %>% filter(COMMON.NAME %in% randomcheck_b$COMMON.NAME)
  restrictedspecieslist_b$mixed = 0
  
  restrictedspecieslist = rbind(restrictedspecieslist_a,restrictedspecieslist_b)
  
  t1 = dataf %>%
    filter((ht == 1 | rt == 1) & (Breeding.Activity.Period != "Nocturnal" |
                                    Non.Breeding.Activity.Period != "Nocturnal"))
  x10 = paste(length(t1$COMMON.NAME),"filter 1 number of species")
  t2 = dataf %>%
    filter((Endemic.Region != "None" | 
              ht == 1 | rt == 1) & (Breeding.Activity.Period != "Nocturnal" |
                                      Non.Breeding.Activity.Period != "Nocturnal"))
  x11 = paste(length(t2$COMMON.NAME),"filter 2 number of species")
  t3 = dataf %>%
    filter((Essential == 1 | Endemic.Region != "None" | 
              ht == 1 | rt == 1) & (Breeding.Activity.Period != "Nocturnal" |
                                      Non.Breeding.Activity.Period != "Nocturnal"))
  x12 = paste(length(t3$COMMON.NAME),"filter 3 number of species")
  
  specieslist1 = specieslist
  specieslist1$selected = 1
  specieslist1 = specieslist1 %>% select(COMMON.NAME,selected)
  
  dataf = dataf %>%
    select(COMMON.NAME,SCIENTIFIC.NAME,ht,rt)
  
  dataf = left_join(dataf,specieslist1)
  names(dataf) = c("COMMON.NAME","SCIENTIFIC.NAME","Long.Term.Analysis","Current.Analysis",
                   "Selected.SOIB")
  
  dataf[is.na(dataf)] = ""
  dataf[dataf == 1] = "X"
  
  dataf$Long.Term.Analysis[dataf$COMMON.NAME %in% check1] = "X"
  dataf$Current.Analysis[dataf$COMMON.NAME %in% check2] = "X"
  
  dataf = dataf %>% select("COMMON.NAME","SCIENTIFIC.NAME","Long.Term.Analysis","Current.Analysis",
                           "Selected.SOIB")
  
  
  ## filter out woodland species only
  dataf$Long.Term.Analysis[!dataf$COMMON.NAME %in% woodland] = ""
  dataf$Current.Analysis[!dataf$COMMON.NAME %in% woodland] = ""
  dataf$Selected.SOIB[!dataf$COMMON.NAME %in% woodland] = ""
  
  sampledcells = c(length(unique(data0$gridg0)),length(unique(data0$gridg1)),
                   length(unique(data0$gridg2)),length(unique(data0$gridg3)),
                   length(unique(data0$gridg4)))
  
  stats = c(x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12)
  
  #### additional filtering safeguards - proportion of range sampled during every timegroup
  
  totalrange = data %>%
    filter(COMMON.NAME %in% dataf$COMMON.NAME, ALL.SPECIES.REPORTED == 1) %>%
    group_by(COMMON.NAME) %>% reframe(totalrange25km = n_distinct(gridg1))
  
  proprange2000 = data %>%
    filter(COMMON.NAME %in% dataf$COMMON.NAME, ALL.SPECIES.REPORTED == 1) %>%
    filter(timegroups == "before 2000") %>%
    group_by(COMMON.NAME) %>% reframe(proprange25km2000 = n_distinct(gridg1))
  
  proprange2022 = data %>%
    filter(COMMON.NAME %in% dataf$COMMON.NAME, ALL.SPECIES.REPORTED == 1) %>%
    filter(timegroups == "2022") %>%
    group_by(COMMON.NAME) %>% reframe(proprange25km2022 = n_distinct(gridg1))
  
  proprange.current = data %>%
    filter(COMMON.NAME %in% dataf$COMMON.NAME, ALL.SPECIES.REPORTED == 1) %>%
    filter(timegroups %in% c("2015","2016","2017","2018","2019","2020","2021","2022")) %>%
    group_by(COMMON.NAME, timegroups) %>% reframe(proprange25km.current = n_distinct(gridg1)) %>%
    group_by(COMMON.NAME) %>% reframe(proprange25km.current = mean(proprange25km.current))
  
  range25km = left_join(totalrange,proprange2000)
  range25km = left_join(range25km,proprange.current)
  range25km = left_join(range25km,proprange2022)
  range25km = range25km %>%
    mutate(proprange25km2000 = proprange25km2000/totalrange25km,
           proprange25km.current = proprange25km.current/totalrange25km,
           proprange25km2022 = proprange25km2022/totalrange25km)
  
  #### additional filtering safeguards - proportional sampling within each 25km grid cell
  
  samp5km = data %>%
    filter(ALL.SPECIES.REPORTED == 1) %>%
    group_by(gridg1) %>% reframe(n = n_distinct(gridg0))
  
  spec25km = data %>%
    filter(ALL.SPECIES.REPORTED == 1) %>%
    filter(COMMON.NAME %in% dataf$COMMON.NAME) %>%
    distinct(COMMON.NAME,gridg1)
  
  samp25km5km = spec25km %>% left_join(samp5km) %>%
    group_by(COMMON.NAME) %>% reframe(mean5km = mean(n), ci5km = 1.96*sd(n)/sqrt(n()))
  
  dataf = dataf %>% left_join(range25km)
  dataf = dataf %>% left_join(samp25km5km)
  
  
  write.csv(dataf,"fullspecieslist_mask_woodland.csv",row.names = F)
  
  locs_write = data0 %>% 
    filter(ALL.SPECIES.REPORTED == 1) %>%
    distinct(LOCALITY.ID,group.id,month,timegroups)
  write.csv(locs_write,"masks_analyses/sub_samp_locs_mask_woodland.csv",row.names = F)
  
  save(specieslist,restrictedspecieslist,databins,file = "masks_analyses/specieslists_mask_woodland.RData")
  save(data,sampledcells,databins,stats,file = "masks_analyses/dataforanalyses_mask_woodland.RData")
  
  
  
  
  
  #### cropland mask
  
  data0 = data_base %>% filter(maskCrp == 1)
  data = data0 %>% select(-CATEGORY,-LOCALITY.ID,-REVIEWED,-APPROVED,-ST_NM,-DISTRICT,
                           -LOCALITY.TYPE,-pa.name,-maskWdl,-maskCrp,-maskOne,
                           -LATITUDE,-LONGITUDE,-TIME.OBSERVATIONS.STARTED,-PROTOCOL.TYPE,
                           -DURATION.MINUTES,-EFFORT.DISTANCE.KM,-day,-cyear,-EXOTIC.CODE)
  
  x7 = paste(nrow(data[data$ALL.SPECIES.REPORTED == 1,]),
             "filter 1 usable observations")
  x8 = paste(length(unique(data[data$ALL.SPECIES.REPORTED == 1,]$group.id)),
             "filter 2 unique complete checklists")
  x9 = paste(length(unique(data[data$timegroups == "before 2000" &
                                   data$ALL.SPECIES.REPORTED == 1,]$group.id)),
             "pre-2000 checklists")
  
  databins = data %>%
    filter(ALL.SPECIES.REPORTED == 1) %>%
    group_by(timegroups) %>% reframe(lists = n_distinct(group.id), year = round(median(year))) %>%
    arrange(year)
  
  datah = data0 %>%
    filter(ALL.SPECIES.REPORTED == 1, CATEGORY == "species" | CATEGORY == "issf") %>%
    group_by(COMMON.NAME,timegroups) %>% reframe(locs = n_distinct(LOCALITY.ID), 
                                                 cells = n_distinct(gridg4)) %>%
    group_by(COMMON.NAME,timegroups) %>%
    filter(locs > locationlimit, cells > gridlimit) %>%
    group_by(COMMON.NAME) %>% reframe(years = n()) %>%
    group_by(COMMON.NAME) %>%
    filter(years == 14) %>%
    mutate(ht = 1) %>% select (COMMON.NAME,ht)
  
  datar = data0 %>%
    filter(ALL.SPECIES.REPORTED == 1, CATEGORY == "species" | CATEGORY == "issf", year > 2014) %>%
    group_by(COMMON.NAME,year) %>% reframe(locs = n_distinct(LOCALITY.ID), 
                                           cells = n_distinct(gridg4)) %>%
    group_by(COMMON.NAME,year) %>%
    filter(locs > locationlimit, cells > gridlimit) %>%
    group_by(COMMON.NAME) %>% reframe(years = n()) %>%
    group_by(COMMON.NAME) %>%
    filter(years == 8) %>%
    mutate(rt = 1) %>% select(COMMON.NAME,rt)
  
  dataresth1 = data0 %>%
    filter(ALL.SPECIES.REPORTED == 1, CATEGORY == "species" | CATEGORY == "issf") %>%
    group_by(COMMON.NAME,timegroups) %>% reframe(cells = n_distinct(gridg4)) %>%
    group_by(COMMON.NAME,timegroups) %>%
    filter(cells <= gridlimit) %>%
    group_by(COMMON.NAME) %>% reframe(years = n()) %>%
    group_by(COMMON.NAME) %>%
    filter(years == 14) %>%
    select (COMMON.NAME)
  
  speciesresth = data.frame(species = intersect(unique(dataresth1$COMMON.NAME),resident))
  speciesresth$validh = NA
  
  for (i in 1:length(speciesresth$species))
  {
    tempresth1 = data0 %>%
      filter(COMMON.NAME == speciesresth$species[i]) %>%
      distinct(gridg1)
    tempresth1 = tempresth1 %>% left_join(data0)
    tempresth1 = tempresth1 %>%
      group_by(timegroups) %>% reframe(n = n_distinct(group.id)) %>%
      group_by(timegroups) %>%
      filter(n > listlimit)
    if (length(tempresth1$timegroups) == 14)
      speciesresth$validh[speciesresth$species == speciesresth$species[i]] = 1
  }
  
  datarestr1 = data0 %>%
    filter(ALL.SPECIES.REPORTED == 1, CATEGORY == "species" | CATEGORY == "issf", year > 2014) %>%
    group_by(COMMON.NAME,timegroups) %>% reframe(cells = n_distinct(gridg4)) %>%
    group_by(COMMON.NAME,timegroups) %>%
    filter(cells <= gridlimit) %>%
    group_by(COMMON.NAME) %>% reframe(years = n()) %>%
    group_by(COMMON.NAME) %>%
    filter(years == 8) %>%
    select (COMMON.NAME)
  
  speciesrestr = data.frame(species = intersect(unique(datarestr1$COMMON.NAME),resident))
  speciesrestr$validr = NA
  
  for (i in 1:length(unique(datarestr1$COMMON.NAME)))
  {
    temprestr1 = data0 %>%
      filter(COMMON.NAME == speciesrestr$species[i]) %>%
      distinct(gridg1)
    temprestr1 = temprestr1 %>% left_join(data0)
    temprestr1 = temprestr1 %>%
      filter(year > 2014) %>%
      group_by(timegroups) %>% reframe(n = n_distinct(group.id)) %>%
      group_by(timegroups) %>%
      filter(n > listlimit)
    if (length(temprestr1$timegroups) == 8)
      speciesrestr$validr[speciesrestr$species == speciesrestr$species[i]] = 1
  }
  
  dataf = fullmap
  names(dataf)[1:2] = c("COMMON.NAME","SCIENTIFIC.NAME")
  
  dataf = left_join(dataf,datah,by = c("COMMON.NAME"))
  dataf = left_join(dataf,datar,by = c("COMMON.NAME"))
  
  specieslist = dataf %>%
    filter((Essential == 1 | Endemic.Region != "None" | 
              ht == 1 | rt == 1) & 
             (Breeding.Activity.Period != "Nocturnal" | 
                Non.Breeding.Activity.Period != "Nocturnal" | 
                COMMON.NAME == "Jerdon's Courser") & 
             (is.na(Discard))) %>%
    select(COMMON.NAME,ht,rt)
  dataf$ht[dataf$Breeding.Activity.Period == "Nocturnal" &
             dataf$Non.Breeding.Activity.Period == "Nocturnal"] = NA
  dataf$rt[dataf$Breeding.Activity.Period == "Nocturnal" &
             dataf$Non.Breeding.Activity.Period == "Nocturnal"] = NA
  
  specieslist$ht[specieslist$COMMON.NAME %in% c("Besra","Horsfield's Bushlark","Common Flameback",
                                                "Eastern Orphean Warbler","Richard's Pipit")] = NA
  specieslist$rt[specieslist$COMMON.NAME %in% c("Besra","Horsfield's Bushlark","Common Flameback",
                                                "Eastern Orphean Warbler","Richard's Pipit")] = NA
  
  restrictedspecieslist  = data.frame(species = specieslist$COMMON.NAME)
  restrictedspecieslist = left_join(restrictedspecieslist,speciesresth)
  restrictedspecieslist = left_join(restrictedspecieslist,speciesrestr)
  
  restrictedspecieslist = restrictedspecieslist %>%
    filter(!is.na(validh) | !is.na(validr))
  
  names(restrictedspecieslist) = c("COMMON.NAME","ht","rt")
  
  restrictedspecieslist$ht[restrictedspecieslist$COMMON.NAME %in% 
                             c("Besra","Horsfield's Bushlark","Common Flameback",
                               "Eastern Orphean Warbler","Richard's Pipit")] = NA
  restrictedspecieslist$rt[restrictedspecieslist$COMMON.NAME %in% 
                             c("Besra","Horsfield's Bushlark","Common Flameback",
                               "Eastern Orphean Warbler","Richard's Pipit")] = NA
  
  # select only openland species
  specieslist$ht[!specieslist$COMMON.NAME %in% openland] = NA
  specieslist$rt[!specieslist$COMMON.NAME %in% openland] = NA
  
  restrictedspecieslist$ht[!restrictedspecieslist$COMMON.NAME %in% openland] = NA
  restrictedspecieslist$rt[!restrictedspecieslist$COMMON.NAME %in% openland] = NA
  
  
  
  check1 = restrictedspecieslist$COMMON.NAME[!is.na(restrictedspecieslist$ht)]
  check2 = restrictedspecieslist$COMMON.NAME[!is.na(restrictedspecieslist$rt)]
  
  randomcheck_a = data0 %>% filter(ALL.SPECIES.REPORTED == 1, 
                                   COMMON.NAME %in% restrictedspecieslist$COMMON.NAME) %>%
    group_by(COMMON.NAME) %>% reframe(n = n_distinct(gridg1)) %>%
    group_by(COMMON.NAME) %>% filter(n > 7)
  
  randomcheck_b = data0 %>% filter(ALL.SPECIES.REPORTED == 1, 
                                   COMMON.NAME %in% restrictedspecieslist$COMMON.NAME) %>%
    group_by(COMMON.NAME) %>% reframe(n = n_distinct(gridg1)) %>%
    group_by(COMMON.NAME) %>% filter(n <= 7)
  
  restrictedspecieslist_a = restrictedspecieslist %>% filter(COMMON.NAME %in% randomcheck_a$COMMON.NAME)
  restrictedspecieslist_a$mixed = 1
  restrictedspecieslist_b = restrictedspecieslist %>% filter(COMMON.NAME %in% randomcheck_b$COMMON.NAME)
  restrictedspecieslist_b$mixed = 0
  
  restrictedspecieslist = rbind(restrictedspecieslist_a,restrictedspecieslist_b)
  

  t1 = dataf %>%
    filter((ht == 1 | rt == 1) & (Breeding.Activity.Period != "Nocturnal" |
                                    Non.Breeding.Activity.Period != "Nocturnal"))
  x10 = paste(length(t1$COMMON.NAME),"filter 1 number of species")
  t2 = dataf %>%
    filter((Endemic.Region != "None" | 
              ht == 1 | rt == 1) & (Breeding.Activity.Period != "Nocturnal" |
                                      Non.Breeding.Activity.Period != "Nocturnal"))
  x11 = paste(length(t2$COMMON.NAME),"filter 2 number of species")
  t3 = dataf %>%
    filter((Essential == 1 | Endemic.Region != "None" | 
              ht == 1 | rt == 1) & (Breeding.Activity.Period != "Nocturnal" |
                                      Non.Breeding.Activity.Period != "Nocturnal"))
  x12 = paste(length(t3$COMMON.NAME),"filter 3 number of species")
  
  specieslist1 = specieslist
  specieslist1$selected = 1
  specieslist1 = specieslist1 %>% select(COMMON.NAME,selected)
  
  dataf = dataf %>%
    select(COMMON.NAME,SCIENTIFIC.NAME,ht,rt)
  
  dataf = left_join(dataf,specieslist1)
  names(dataf) = c("COMMON.NAME","SCIENTIFIC.NAME","Long.Term.Analysis","Current.Analysis",
                   "Selected.SOIB")
  
  dataf[is.na(dataf)] = ""
  dataf[dataf == 1] = "X"
  
  dataf$Long.Term.Analysis[dataf$COMMON.NAME %in% check1] = "X"
  dataf$Current.Analysis[dataf$COMMON.NAME %in% check2] = "X"
  
  dataf = dataf %>% select("COMMON.NAME","SCIENTIFIC.NAME","Long.Term.Analysis","Current.Analysis",
                           "Selected.SOIB")
  
  ## filter out openland species only
  dataf$Long.Term.Analysis[!dataf$COMMON.NAME %in% openland] = ""
  dataf$Current.Analysis[!dataf$COMMON.NAME %in% openland] = ""
  dataf$Selected.SOIB[!dataf$COMMON.NAME %in% openland] = ""
  
  
  sampledcells = c(length(unique(data0$gridg0)),length(unique(data0$gridg1)),
                   length(unique(data0$gridg2)),length(unique(data0$gridg3)),
                   length(unique(data0$gridg4)))
  
  stats = c(x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12)
  
  #### additional filtering safeguards - proportion of range sampled during every timegroup
  
  totalrange = data %>%
    filter(COMMON.NAME %in% dataf$COMMON.NAME, ALL.SPECIES.REPORTED == 1) %>%
    group_by(COMMON.NAME) %>% reframe(totalrange25km = n_distinct(gridg1))
  
  proprange2000 = data %>%
    filter(COMMON.NAME %in% dataf$COMMON.NAME, ALL.SPECIES.REPORTED == 1) %>%
    filter(timegroups == "before 2000") %>%
    group_by(COMMON.NAME) %>% reframe(proprange25km2000 = n_distinct(gridg1))
  
  proprange2022 = data %>%
    filter(COMMON.NAME %in% dataf$COMMON.NAME, ALL.SPECIES.REPORTED == 1) %>%
    filter(timegroups == "2022") %>%
    group_by(COMMON.NAME) %>% reframe(proprange25km2022 = n_distinct(gridg1))
  
  proprange.current = data %>%
    filter(COMMON.NAME %in% dataf$COMMON.NAME, ALL.SPECIES.REPORTED == 1) %>%
    filter(timegroups %in% c("2015","2016","2017","2018","2019","2020","2021","2022")) %>%
    group_by(COMMON.NAME, timegroups) %>% reframe(proprange25km.current = n_distinct(gridg1)) %>%
    group_by(COMMON.NAME) %>% reframe(proprange25km.current = mean(proprange25km.current))
  
  range25km = left_join(totalrange,proprange2000)
  range25km = left_join(range25km,proprange.current)
  range25km = left_join(range25km,proprange2022)
  range25km = range25km %>%
    mutate(proprange25km2000 = proprange25km2000/totalrange25km,
           proprange25km.current = proprange25km.current/totalrange25km,
           proprange25km2022 = proprange25km2022/totalrange25km)
  
  
  #### additional filtering safeguards - proportional sampling within each 25km grid cell
  
  samp5km = data %>%
    filter(ALL.SPECIES.REPORTED == 1) %>%
    group_by(gridg1) %>% reframe(n = n_distinct(gridg0))
  
  spec25km = data %>%
    filter(ALL.SPECIES.REPORTED == 1) %>%
    filter(COMMON.NAME %in% dataf$COMMON.NAME) %>%
    distinct(COMMON.NAME,gridg1)
  
  samp25km5km = spec25km %>% left_join(samp5km) %>%
    group_by(COMMON.NAME) %>% reframe(mean5km = mean(n), ci5km = 1.96*sd(n)/sqrt(n()))
  
  dataf = dataf %>% left_join(range25km)
  dataf = dataf %>% left_join(samp25km5km)
  
  
  write.csv(dataf,"fullspecieslist_mask_cropland.csv",row.names = F)
  
  locs_write = data0 %>% 
    filter(ALL.SPECIES.REPORTED == 1) %>%
    distinct(LOCALITY.ID,group.id,month,timegroups)
  write.csv(locs_write,"masks_analyses/sub_samp_locs_mask_cropland.csv",row.names = F)
  
  save(specieslist,restrictedspecieslist,databins,file = "masks_analyses/specieslists_mask_cropland.RData")
  save(data,sampledcells,databins,stats,file = "masks_analyses/dataforanalyses_mask_cropland.RData")
  
  
  
  
  
  
  
  #### ONEland mask
  
  data0 = data_base %>% filter(maskOne == 1)
  data = data0 %>% select(-CATEGORY,-LOCALITY.ID,-REVIEWED,-APPROVED,-ST_NM,-DISTRICT,
                           -LOCALITY.TYPE,-pa.name,-maskWdl,-maskCrp,-maskOne,
                           -LATITUDE,-LONGITUDE,-TIME.OBSERVATIONS.STARTED,-PROTOCOL.TYPE,
                           -DURATION.MINUTES,-EFFORT.DISTANCE.KM,-day,-cyear,-EXOTIC.CODE)
  
  x7 = paste(nrow(data[data$ALL.SPECIES.REPORTED == 1,]),
             "filter 1 usable observations")
  x8 = paste(length(unique(data[data$ALL.SPECIES.REPORTED == 1,]$group.id)),
             "filter 2 unique complete checklists")
  x9 = paste(length(unique(data[data$timegroups == "before 2000" &
                                   data$ALL.SPECIES.REPORTED == 1,]$group.id)),
             "pre-2000 checklists")
  
  databins = data %>%
    filter(ALL.SPECIES.REPORTED == 1) %>%
    group_by(timegroups) %>% reframe(lists = n_distinct(group.id), year = round(median(year))) %>%
    arrange(year)
  
  datah = data0 %>%
    filter(ALL.SPECIES.REPORTED == 1, CATEGORY == "species" | CATEGORY == "issf") %>%
    group_by(COMMON.NAME,timegroups) %>% reframe(locs = n_distinct(LOCALITY.ID), 
                                                 cells = n_distinct(gridg4)) %>%
    group_by(COMMON.NAME,timegroups) %>%
    filter(locs > locationlimit, cells > gridlimit) %>%
    group_by(COMMON.NAME) %>% reframe(years = n()) %>%
    group_by(COMMON.NAME) %>%
    filter(years == 14) %>%
    mutate(ht = 1) %>% select (COMMON.NAME,ht)
  
  datar = data0 %>%
    filter(ALL.SPECIES.REPORTED == 1, CATEGORY == "species" | CATEGORY == "issf", year > 2014) %>%
    group_by(COMMON.NAME,year) %>% reframe(locs = n_distinct(LOCALITY.ID), 
                                           cells = n_distinct(gridg4)) %>%
    group_by(COMMON.NAME,year) %>%
    filter(locs > locationlimit, cells > gridlimit) %>%
    group_by(COMMON.NAME) %>% reframe(years = n()) %>%
    group_by(COMMON.NAME) %>%
    filter(years == 8) %>%
    mutate(rt = 1) %>% select(COMMON.NAME,rt)
  
  dataresth1 = data0 %>%
    filter(ALL.SPECIES.REPORTED == 1, CATEGORY == "species" | CATEGORY == "issf") %>%
    group_by(COMMON.NAME,timegroups) %>% reframe(cells = n_distinct(gridg4)) %>%
    group_by(COMMON.NAME,timegroups) %>%
    filter(cells <= gridlimit) %>%
    group_by(COMMON.NAME) %>% reframe(years = n()) %>%
    group_by(COMMON.NAME) %>%
    filter(years == 14) %>%
    select (COMMON.NAME)
  
  speciesresth = data.frame(species = intersect(unique(dataresth1$COMMON.NAME),resident))
  speciesresth$validh = NA
  
  for (i in 1:length(speciesresth$species))
  {
    tempresth1 = data0 %>%
      filter(COMMON.NAME == speciesresth$species[i]) %>%
      distinct(gridg1)
    tempresth1 = tempresth1 %>% left_join(data0)
    tempresth1 = tempresth1 %>%
      group_by(timegroups) %>% reframe(n = n_distinct(group.id)) %>%
      group_by(timegroups) %>%
      filter(n > listlimit)
    if (length(tempresth1$timegroups) == 14)
      speciesresth$validh[speciesresth$species == speciesresth$species[i]] = 1
  }
  
  datarestr1 = data0 %>%
    filter(ALL.SPECIES.REPORTED == 1, CATEGORY == "species" | CATEGORY == "issf", year > 2014) %>%
    group_by(COMMON.NAME,timegroups) %>% reframe(cells = n_distinct(gridg4)) %>%
    group_by(COMMON.NAME,timegroups) %>%
    filter(cells <= gridlimit) %>%
    group_by(COMMON.NAME) %>% reframe(years = n()) %>%
    group_by(COMMON.NAME) %>%
    filter(years == 8) %>%
    select (COMMON.NAME)
  
  speciesrestr = data.frame(species = intersect(unique(datarestr1$COMMON.NAME),resident))
  speciesrestr$validr = NA
  
  for (i in 1:length(unique(datarestr1$COMMON.NAME)))
  {
    temprestr1 = data0 %>%
      filter(COMMON.NAME == speciesrestr$species[i]) %>%
      distinct(gridg1)
    temprestr1 = temprestr1 %>% left_join(data0)
    temprestr1 = temprestr1 %>%
      filter(year > 2014) %>%
      group_by(timegroups) %>% reframe(n = n_distinct(group.id)) %>%
      group_by(timegroups) %>%
      filter(n > listlimit)
    if (length(temprestr1$timegroups) == 8)
      speciesrestr$validr[speciesrestr$species == speciesrestr$species[i]] = 1
  }
  
  dataf = fullmap
  names(dataf)[1:2] = c("COMMON.NAME","SCIENTIFIC.NAME")
  
  dataf = left_join(dataf,datah,by = c("COMMON.NAME"))
  dataf = left_join(dataf,datar,by = c("COMMON.NAME"))
  
  specieslist = dataf %>%
    filter((Essential == 1 | Endemic.Region != "None" | 
              ht == 1 | rt == 1) & 
             (Breeding.Activity.Period != "Nocturnal" | 
                Non.Breeding.Activity.Period != "Nocturnal" | 
                COMMON.NAME == "Jerdon's Courser") & 
             (is.na(Discard))) %>%
    select(COMMON.NAME,ht,rt)
  dataf$ht[dataf$Breeding.Activity.Period == "Nocturnal" &
             dataf$Non.Breeding.Activity.Period == "Nocturnal"] = NA
  dataf$rt[dataf$Breeding.Activity.Period == "Nocturnal" &
             dataf$Non.Breeding.Activity.Period == "Nocturnal"] = NA
  
  specieslist$ht[specieslist$COMMON.NAME %in% c("Besra","Horsfield's Bushlark","Common Flameback",
                                                "Eastern Orphean Warbler","Richard's Pipit")] = NA
  specieslist$rt[specieslist$COMMON.NAME %in% c("Besra","Horsfield's Bushlark","Common Flameback",
                                                "Eastern Orphean Warbler","Richard's Pipit")] = NA
  
  restrictedspecieslist  = data.frame(species = specieslist$COMMON.NAME)
  restrictedspecieslist = left_join(restrictedspecieslist,speciesresth)
  restrictedspecieslist = left_join(restrictedspecieslist,speciesrestr)
  
  restrictedspecieslist = restrictedspecieslist %>%
    filter(!is.na(validh) | !is.na(validr))
  
  names(restrictedspecieslist) = c("COMMON.NAME","ht","rt")
  
  restrictedspecieslist$ht[restrictedspecieslist$COMMON.NAME %in% 
                             c("Besra","Horsfield's Bushlark","Common Flameback",
                               "Eastern Orphean Warbler","Richard's Pipit")] = NA
  restrictedspecieslist$rt[restrictedspecieslist$COMMON.NAME %in% 
                             c("Besra","Horsfield's Bushlark","Common Flameback",
                               "Eastern Orphean Warbler","Richard's Pipit")] = NA
  
  # select only openland species
  specieslist$ht[!specieslist$COMMON.NAME %in% openland] = NA
  specieslist$rt[!specieslist$COMMON.NAME %in% openland] = NA
  
  restrictedspecieslist$ht[!restrictedspecieslist$COMMON.NAME %in% openland] = NA
  restrictedspecieslist$rt[!restrictedspecieslist$COMMON.NAME %in% openland] = NA
  
  
  
  check1 = restrictedspecieslist$COMMON.NAME[!is.na(restrictedspecieslist$ht)]
  check2 = restrictedspecieslist$COMMON.NAME[!is.na(restrictedspecieslist$rt)]
  
  randomcheck_a = data0 %>% filter(ALL.SPECIES.REPORTED == 1, 
                                   COMMON.NAME %in% restrictedspecieslist$COMMON.NAME) %>%
    group_by(COMMON.NAME) %>% reframe(n = n_distinct(gridg1)) %>%
    group_by(COMMON.NAME) %>% filter(n > 7)
  
  randomcheck_b = data0 %>% filter(ALL.SPECIES.REPORTED == 1, 
                                   COMMON.NAME %in% restrictedspecieslist$COMMON.NAME) %>%
    group_by(COMMON.NAME) %>% reframe(n = n_distinct(gridg1)) %>%
    group_by(COMMON.NAME) %>% filter(n <= 7)
  
  restrictedspecieslist_a = restrictedspecieslist %>% filter(COMMON.NAME %in% randomcheck_a$COMMON.NAME)
  restrictedspecieslist_a$mixed = 1
  restrictedspecieslist_b = restrictedspecieslist %>% filter(COMMON.NAME %in% randomcheck_b$COMMON.NAME)
  restrictedspecieslist_b$mixed = 0
  
  restrictedspecieslist = rbind(restrictedspecieslist_a,restrictedspecieslist_b)
  

  
  
  t1 = dataf %>%
    filter((ht == 1 | rt == 1) & (Breeding.Activity.Period != "Nocturnal" |
                                    Non.Breeding.Activity.Period != "Nocturnal"))
  x10 = paste(length(t1$COMMON.NAME),"filter 1 number of species")
  t2 = dataf %>%
    filter((Endemic.Region != "None" | 
              ht == 1 | rt == 1) & (Breeding.Activity.Period != "Nocturnal" |
                                      Non.Breeding.Activity.Period != "Nocturnal"))
  x11 = paste(length(t2$COMMON.NAME),"filter 2 number of species")
  t3 = dataf %>%
    filter((Essential == 1 | Endemic.Region != "None" | 
              ht == 1 | rt == 1) & (Breeding.Activity.Period != "Nocturnal" |
                                      Non.Breeding.Activity.Period != "Nocturnal"))
  x12 = paste(length(t3$COMMON.NAME),"filter 3 number of species")
  
  specieslist1 = specieslist
  specieslist1$selected = 1
  specieslist1 = specieslist1 %>% select(COMMON.NAME,selected)
  
  dataf = dataf %>%
    select(COMMON.NAME,SCIENTIFIC.NAME,ht,rt)
  
  dataf = left_join(dataf,specieslist1)
  names(dataf) = c("COMMON.NAME","SCIENTIFIC.NAME","Long.Term.Analysis","Current.Analysis",
                   "Selected.SOIB")
  
  ## filter out openland species only
  dataf$Long.Term.Analysis[!dataf$COMMON.NAME %in% openland] = ""
  dataf$Current.Analysis[!dataf$COMMON.NAME %in% openland] = ""
  dataf$Selected.SOIB[!dataf$COMMON.NAME %in% openland] = ""
  
  
  dataf[is.na(dataf)] = ""
  dataf[dataf == 1] = "X"
  
  dataf$Long.Term.Analysis[dataf$COMMON.NAME %in% check1] = "X"
  dataf$Current.Analysis[dataf$COMMON.NAME %in% check2] = "X"
  
  dataf = dataf %>% select("COMMON.NAME","SCIENTIFIC.NAME","Long.Term.Analysis","Current.Analysis",
                           "Selected.SOIB")
  
  sampledcells = c(length(unique(data0$gridg0)),length(unique(data0$gridg1)),
                   length(unique(data0$gridg2)),length(unique(data0$gridg3)),
                   length(unique(data0$gridg4)))
  
  stats = c(x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12)
  
  #### additional filtering safeguards - proportion of range sampled during every timegroup
  
  totalrange = data %>%
    filter(COMMON.NAME %in% dataf$COMMON.NAME, ALL.SPECIES.REPORTED == 1) %>%
    group_by(COMMON.NAME) %>% reframe(totalrange25km = n_distinct(gridg1))
  
  proprange2000 = data %>%
    filter(COMMON.NAME %in% dataf$COMMON.NAME, ALL.SPECIES.REPORTED == 1) %>%
    filter(timegroups == "before 2000") %>%
    group_by(COMMON.NAME) %>% reframe(proprange25km2000 = n_distinct(gridg1))
  
  proprange2022 = data %>%
    filter(COMMON.NAME %in% dataf$COMMON.NAME, ALL.SPECIES.REPORTED == 1) %>%
    filter(timegroups == "2022") %>%
    group_by(COMMON.NAME) %>% reframe(proprange25km2022 = n_distinct(gridg1))
  
  proprange.current = data %>%
    filter(COMMON.NAME %in% dataf$COMMON.NAME, ALL.SPECIES.REPORTED == 1) %>%
    filter(timegroups %in% c("2015","2016","2017","2018","2019","2020","2021","2022")) %>%
    group_by(COMMON.NAME, timegroups) %>% reframe(proprange25km.current = n_distinct(gridg1)) %>%
    group_by(COMMON.NAME) %>% reframe(proprange25km.current = mean(proprange25km.current))
  
  range25km = left_join(totalrange,proprange2000)
  range25km = left_join(range25km,proprange.current)
  range25km = left_join(range25km,proprange2022)
  range25km = range25km %>%
    mutate(proprange25km2000 = proprange25km2000/totalrange25km,
           proprange25km.current = proprange25km.current/totalrange25km,
           proprange25km2022 = proprange25km2022/totalrange25km)
  
  
  #### additional filtering safeguards - proportional sampling within each 25km grid cell
  
  samp5km = data %>%
    filter(ALL.SPECIES.REPORTED == 1) %>%
    group_by(gridg1) %>% reframe(n = n_distinct(gridg0))
  
  spec25km = data %>%
    filter(ALL.SPECIES.REPORTED == 1) %>%
    filter(COMMON.NAME %in% dataf$COMMON.NAME) %>%
    distinct(COMMON.NAME,gridg1)
  
  samp25km5km = spec25km %>% left_join(samp5km) %>%
    group_by(COMMON.NAME) %>% reframe(mean5km = mean(n), ci5km = 1.96*sd(n)/sqrt(n()))
  
  dataf = dataf %>% left_join(range25km)
  dataf = dataf %>% left_join(samp25km5km)
  
  
  write.csv(dataf,"fullspecieslist_mask_oneland.csv",row.names = F)
  
  locs_write = data0 %>% 
    filter(ALL.SPECIES.REPORTED == 1) %>%
    distinct(LOCALITY.ID,group.id,month,timegroups)
  write.csv(locs_write,"masks_analyses/sub_samp_locs_mask_oneland.csv",row.names = F)
  
  save(specieslist,restrictedspecieslist,databins,file = "masks_analyses/specieslists_mask_oneland.RData")
  save(data,sampledcells,databins,stats,file = "masks_analyses/dataforanalyses_mask_oneland.RData")
  
  
  
  
  
  #### PA mask
  
  data0 = data_base %>% filter(!is.na(pa.name))
  data = data0 %>% select(-CATEGORY,-LOCALITY.ID,-REVIEWED,-APPROVED,-ST_NM,-DISTRICT,
                          -LOCALITY.TYPE,-pa.name,-maskWdl,-maskCrp,-maskOne,
                          -LATITUDE,-LONGITUDE,-TIME.OBSERVATIONS.STARTED,-PROTOCOL.TYPE,
                          -DURATION.MINUTES,-EFFORT.DISTANCE.KM,-day,-cyear,-EXOTIC.CODE)
  
  x7 = paste(nrow(data[data$ALL.SPECIES.REPORTED == 1,]),
             "filter 1 usable observations")
  x8 = paste(length(unique(data[data$ALL.SPECIES.REPORTED == 1,]$group.id)),
             "filter 2 unique complete checklists")
  x9 = paste(length(unique(data[data$timegroups == "before 2000" &
                                  data$ALL.SPECIES.REPORTED == 1,]$group.id)),
             "pre-2000 checklists")
  
  databins = data %>%
    filter(ALL.SPECIES.REPORTED == 1) %>%
    group_by(timegroups) %>% reframe(lists = n_distinct(group.id), year = round(median(year))) %>%
    arrange(year)
  
  datah = data0 %>%
    filter(ALL.SPECIES.REPORTED == 1, CATEGORY == "species" | CATEGORY == "issf") %>%
    group_by(COMMON.NAME,timegroups) %>% reframe(locs = n_distinct(LOCALITY.ID), 
                                                 cells = n_distinct(gridg4)) %>%
    group_by(COMMON.NAME,timegroups) %>%
    filter(locs > locationlimit, cells > gridlimit) %>%
    group_by(COMMON.NAME) %>% reframe(years = n()) %>%
    group_by(COMMON.NAME) %>%
    filter(years == 14) %>%
    mutate(ht = 1) %>% select (COMMON.NAME,ht)
  
  datar = data0 %>%
    filter(ALL.SPECIES.REPORTED == 1, CATEGORY == "species" | CATEGORY == "issf", year > 2014) %>%
    group_by(COMMON.NAME,year) %>% reframe(locs = n_distinct(LOCALITY.ID), 
                                           cells = n_distinct(gridg4)) %>%
    group_by(COMMON.NAME,year) %>%
    filter(locs > locationlimit, cells > gridlimit) %>%
    group_by(COMMON.NAME) %>% reframe(years = n()) %>%
    group_by(COMMON.NAME) %>%
    filter(years == 8) %>%
    mutate(rt = 1) %>% select(COMMON.NAME,rt)
  
  dataresth1 = data0 %>%
    filter(ALL.SPECIES.REPORTED == 1, CATEGORY == "species" | CATEGORY == "issf") %>%
    group_by(COMMON.NAME,timegroups) %>% reframe(cells = n_distinct(gridg4)) %>%
    group_by(COMMON.NAME,timegroups) %>%
    filter(cells <= gridlimit) %>%
    group_by(COMMON.NAME) %>% reframe(years = n()) %>%
    group_by(COMMON.NAME) %>%
    filter(years == 14) %>%
    select (COMMON.NAME)
  
  speciesresth = data.frame(species = intersect(unique(dataresth1$COMMON.NAME),resident))
  speciesresth$validh = NA
  
  for (i in 1:length(speciesresth$species))
  {
    tempresth1 = data0 %>%
      filter(COMMON.NAME == speciesresth$species[i]) %>%
      distinct(gridg1)
    tempresth1 = tempresth1 %>% left_join(data0)
    tempresth1 = tempresth1 %>%
      group_by(timegroups) %>% reframe(n = n_distinct(group.id)) %>%
      group_by(timegroups) %>%
      filter(n > listlimit)
    if (length(tempresth1$timegroups) == 14)
      speciesresth$validh[speciesresth$species == speciesresth$species[i]] = 1
  }
  
  datarestr1 = data0 %>%
    filter(ALL.SPECIES.REPORTED == 1, CATEGORY == "species" | CATEGORY == "issf", year > 2014) %>%
    group_by(COMMON.NAME,timegroups) %>% reframe(cells = n_distinct(gridg4)) %>%
    group_by(COMMON.NAME,timegroups) %>%
    filter(cells <= gridlimit) %>%
    group_by(COMMON.NAME) %>% reframe(years = n()) %>%
    group_by(COMMON.NAME) %>%
    filter(years == 8) %>%
    select (COMMON.NAME)
  
  speciesrestr = data.frame(species = intersect(unique(datarestr1$COMMON.NAME),resident))
  speciesrestr$validr = NA
  
  for (i in 1:length(unique(datarestr1$COMMON.NAME)))
  {
    temprestr1 = data0 %>%
      filter(COMMON.NAME == speciesrestr$species[i]) %>%
      distinct(gridg1)
    temprestr1 = temprestr1 %>% left_join(data0)
    temprestr1 = temprestr1 %>%
      filter(year > 2014) %>%
      group_by(timegroups) %>% reframe(n = n_distinct(group.id)) %>%
      group_by(timegroups) %>%
      filter(n > listlimit)
    if (length(temprestr1$timegroups) == 8)
      speciesrestr$validr[speciesrestr$species == speciesrestr$species[i]] = 1
  }
  
  dataf = fullmap
  names(dataf)[1:2] = c("COMMON.NAME","SCIENTIFIC.NAME")
  
  dataf = left_join(dataf,datah,by = c("COMMON.NAME"))
  dataf = left_join(dataf,datar,by = c("COMMON.NAME"))
  
  specieslist = dataf %>%
    filter((Essential == 1 | Endemic.Region != "None" | 
              ht == 1 | rt == 1) & 
             (Breeding.Activity.Period != "Nocturnal" | 
                Non.Breeding.Activity.Period != "Nocturnal" | 
                COMMON.NAME == "Jerdon's Courser") & 
             (is.na(Discard))) %>%
    select(COMMON.NAME,ht,rt)
  dataf$ht[dataf$Breeding.Activity.Period == "Nocturnal" &
             dataf$Non.Breeding.Activity.Period == "Nocturnal"] = NA
  dataf$rt[dataf$Breeding.Activity.Period == "Nocturnal" &
             dataf$Non.Breeding.Activity.Period == "Nocturnal"] = NA
  
  specieslist$ht[specieslist$COMMON.NAME %in% c("Besra","Horsfield's Bushlark","Common Flameback",
                                                "Eastern Orphean Warbler","Richard's Pipit")] = NA
  specieslist$rt[specieslist$COMMON.NAME %in% c("Besra","Horsfield's Bushlark","Common Flameback",
                                                "Eastern Orphean Warbler","Richard's Pipit")] = NA
  
  restrictedspecieslist  = data.frame(species = specieslist$COMMON.NAME)
  restrictedspecieslist = left_join(restrictedspecieslist,speciesresth)
  restrictedspecieslist = left_join(restrictedspecieslist,speciesrestr)
  
  restrictedspecieslist = restrictedspecieslist %>%
    filter(!is.na(validh) | !is.na(validr))
  
  names(restrictedspecieslist) = c("COMMON.NAME","ht","rt")
  
  restrictedspecieslist$ht[restrictedspecieslist$COMMON.NAME %in% 
                             c("Besra","Horsfield's Bushlark","Common Flameback",
                               "Eastern Orphean Warbler","Richard's Pipit")] = NA
  restrictedspecieslist$rt[restrictedspecieslist$COMMON.NAME %in% 
                             c("Besra","Horsfield's Bushlark","Common Flameback",
                               "Eastern Orphean Warbler","Richard's Pipit")] = NA
  
  check1 = restrictedspecieslist$COMMON.NAME[!is.na(restrictedspecieslist$ht)]
  check2 = restrictedspecieslist$COMMON.NAME[!is.na(restrictedspecieslist$rt)]
  
  randomcheck_a = data0 %>% filter(ALL.SPECIES.REPORTED == 1, 
                                   COMMON.NAME %in% restrictedspecieslist$COMMON.NAME) %>%
    group_by(COMMON.NAME) %>% reframe(n = n_distinct(gridg1)) %>%
    group_by(COMMON.NAME) %>% filter(n > 7)
  
  randomcheck_b = data0 %>% filter(ALL.SPECIES.REPORTED == 1, 
                                   COMMON.NAME %in% restrictedspecieslist$COMMON.NAME) %>%
    group_by(COMMON.NAME) %>% reframe(n = n_distinct(gridg1)) %>%
    group_by(COMMON.NAME) %>% filter(n <= 7)
  
  restrictedspecieslist_a = restrictedspecieslist %>% filter(COMMON.NAME %in% randomcheck_a$COMMON.NAME)
  restrictedspecieslist_a$mixed = 1
  restrictedspecieslist_b = restrictedspecieslist %>% filter(COMMON.NAME %in% randomcheck_b$COMMON.NAME)
  restrictedspecieslist_b$mixed = 0
  
  restrictedspecieslist = rbind(restrictedspecieslist_a,restrictedspecieslist_b)
  
  t1 = dataf %>%
    filter((ht == 1 | rt == 1) & (Breeding.Activity.Period != "Nocturnal" |
                                    Non.Breeding.Activity.Period != "Nocturnal"))
  x10 = paste(length(t1$COMMON.NAME),"filter 1 number of species")
  t2 = dataf %>%
    filter((Endemic.Region != "None" | 
              ht == 1 | rt == 1) & (Breeding.Activity.Period != "Nocturnal" |
                                      Non.Breeding.Activity.Period != "Nocturnal"))
  x11 = paste(length(t2$COMMON.NAME),"filter 2 number of species")
  t3 = dataf %>%
    filter((Essential == 1 | Endemic.Region != "None" | 
              ht == 1 | rt == 1) & (Breeding.Activity.Period != "Nocturnal" |
                                      Non.Breeding.Activity.Period != "Nocturnal"))
  x12 = paste(length(t3$COMMON.NAME),"filter 3 number of species")
  
  specieslist1 = specieslist
  specieslist1$selected = 1
  specieslist1 = specieslist1 %>% select(COMMON.NAME,selected)
  
  dataf = dataf %>%
    select(COMMON.NAME,SCIENTIFIC.NAME,ht,rt)
  
  dataf = left_join(dataf,specieslist1)
  names(dataf) = c("COMMON.NAME","SCIENTIFIC.NAME","Long.Term.Analysis","Current.Analysis",
                   "Selected.SOIB")
  
  dataf[is.na(dataf)] = ""
  dataf[dataf == 1] = "X"
  
  dataf$Long.Term.Analysis[dataf$COMMON.NAME %in% check1] = "X"
  dataf$Current.Analysis[dataf$COMMON.NAME %in% check2] = "X"
  
  dataf = dataf %>% select("COMMON.NAME","SCIENTIFIC.NAME","Long.Term.Analysis","Current.Analysis",
                           "Selected.SOIB")
  
  sampledcells = c(length(unique(data0$gridg0)),length(unique(data0$gridg1)),
                   length(unique(data0$gridg2)),length(unique(data0$gridg3)),
                   length(unique(data0$gridg4)))
  
  stats = c(x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12)
  
  #### additional filtering safeguards - proportion of range sampled during every timegroup
  
  totalrange = data %>%
    filter(COMMON.NAME %in% dataf$COMMON.NAME, ALL.SPECIES.REPORTED == 1) %>%
    group_by(COMMON.NAME) %>% reframe(totalrange25km = n_distinct(gridg1))
  
  proprange2000 = data %>%
    filter(COMMON.NAME %in% dataf$COMMON.NAME, ALL.SPECIES.REPORTED == 1) %>%
    filter(timegroups == "before 2000") %>%
    group_by(COMMON.NAME) %>% reframe(proprange25km2000 = n_distinct(gridg1))
  
  proprange2022 = data %>%
    filter(COMMON.NAME %in% dataf$COMMON.NAME, ALL.SPECIES.REPORTED == 1) %>%
    filter(timegroups == "2022") %>%
    group_by(COMMON.NAME) %>% reframe(proprange25km2022 = n_distinct(gridg1))
  
  proprange.current = data %>%
    filter(COMMON.NAME %in% dataf$COMMON.NAME, ALL.SPECIES.REPORTED == 1) %>%
    filter(timegroups %in% c("2015","2016","2017","2018","2019","2020","2021","2022")) %>%
    group_by(COMMON.NAME, timegroups) %>% reframe(proprange25km.current = n_distinct(gridg1)) %>%
    group_by(COMMON.NAME) %>% reframe(proprange25km.current = mean(proprange25km.current))
  
  range25km = left_join(totalrange,proprange2000)
  range25km = left_join(range25km,proprange.current)
  range25km = left_join(range25km,proprange2022)
  range25km = range25km %>%
    mutate(proprange25km2000 = proprange25km2000/totalrange25km,
           proprange25km.current = proprange25km.current/totalrange25km,
           proprange25km2022 = proprange25km2022/totalrange25km)
  
  
  #### additional filtering safeguards - proportional sampling within each 25km grid cell
  
  samp5km = data %>%
    filter(ALL.SPECIES.REPORTED == 1) %>%
    group_by(gridg1) %>% reframe(n = n_distinct(gridg0))
  
  spec25km = data %>%
    filter(ALL.SPECIES.REPORTED == 1) %>%
    filter(COMMON.NAME %in% dataf$COMMON.NAME) %>%
    distinct(COMMON.NAME,gridg1)
  
  samp25km5km = spec25km %>% left_join(samp5km) %>%
    group_by(COMMON.NAME) %>% reframe(mean5km = mean(n), ci5km = 1.96*sd(n)/sqrt(n()))
  
  dataf = dataf %>% left_join(range25km)
  dataf = dataf %>% left_join(samp25km5km)
  
  
  write.csv(dataf,"fullspecieslist_mask_pa.csv",row.names = F)
  
  locs_write = data0 %>% 
    filter(ALL.SPECIES.REPORTED == 1) %>%
    distinct(LOCALITY.ID,group.id,month,timegroups)
  write.csv(locs_write,"masks_analyses/sub_samp_locs_mask_pa.csv",row.names = F)
  
  save(specieslist,restrictedspecieslist,databins,file = "masks_analyses/specieslists_mask_pa.RData")
  save(data,sampledcells,databins,stats,file = "masks_analyses/dataforanalyses_mask_pa.RData")
}








### expandbyspecies ########################################


## ensure that the working directory has list of India's birds with scientific names 
## (just a safety mechanism for the function to work for small subsets, needs to be enabled if required)
## only need to input data, the species of interest and the complete list of India's bird species
## also groupspecs if required (a dataframe with all relevant list level info), it is defaulted to data

expandbyspecies = function(data, species)
{
  require(tidyverse)
  
  data$gridg1 = as.factor(data$gridg1)
  data$gridg2 = as.factor(data$gridg2)
  data$gridg3 = as.factor(data$gridg3)
  data$gridg4 = as.factor(data$gridg4)

  data$timegroups = as.factor(data$timegroups)
  
  ## considers only complete lists
  
  checklistinfo = data %>%
    distinct(gridg1,gridg2,gridg3,gridg4,
             ALL.SPECIES.REPORTED,OBSERVER.ID,
             #city,
             #DURATION.MINUTES,EFFORT.DISTANCE.KM,
             group.id,month,year,no.sp,timegroups,
             timegroups1)
  
  checklistinfo = checklistinfo %>%
    filter(ALL.SPECIES.REPORTED == 1) %>%
    group_by(group.id) %>% slice(1) %>% ungroup
  
  ## expand data frame to include all bird species in every list
  
  expanded = checklistinfo
  expanded$COMMON.NAME = species
  
  ## join the two, deal with NAs next
  
  expanded = left_join(expanded,data)
  expanded = expanded %>%
    dplyr::select(-c("COMMON.NAME","gridg2","gridg4","OBSERVER.ID",
                     "ALL.SPECIES.REPORTED","group.id","year","timegroups1"))
  
  ## deal with NAs
  
  expanded = expanded %>% mutate(OBSERVATION.COUNT = replace(OBSERVATION.COUNT, is.na(OBSERVATION.COUNT), "0"))
  
  
  expanded = expanded %>%
    mutate(OBSERVATION.COUNT=replace(OBSERVATION.COUNT, OBSERVATION.COUNT != "0", "1"))
  
  
  
  expanded$OBSERVATION.COUNT = as.numeric(expanded$OBSERVATION.COUNT)

  return(expanded)
}



### error operations ########################################

errordiv = function(x1,x2,se1,se2)
{
  r = x1/x2
  t = data.frame(se1/x1,se2/x2)
  ser = r*sqrt(t[,1]^2 + t[,2]^2)
  a = data.frame(freq = numeric(length(r)))
  a$freq = r
  a$se = ser
  return(a)
}

erroradd = function(vec)
{
  err = sqrt(sum(vec^2))
  return(err)
}

simerrordiv = function(x1,x2,se1,se2)
{
  tp = data.frame(num = clogloglink(rnorm(1000,x1,se1), inverse = T), 
                  den = clogloglink(rnorm(1000,x2,se2), inverse = T))
  tp = tp %>%
    reframe(rat = num/den, val = num)

  return(tp)
}






### occupancy ########################################

## occupancy analyses for bird abundance/range, reports area in units of 10000 sq. km.
## Requires tidyverse, reshape2, data.table and unmarked
## type = trivial, null, nosp, nosptime, nb, nosptimenb

occufreq = function(data, species, areag, rerun = F, datatofill)
{
  require(tidyverse)
  require(reshape2)
  require(data.table)
  require(unmarked)
  
  load("neighbours.RData")
  
  migstatus = read.csv("Migratory Status - Migratory Status.csv")

  migstatus = migstatus %>%
    mutate(mig = 
             case_when(!is.na(Summer.Visitor) & !is.na(Winter.Visitor) ~ "LM",
                       !is.na(Resident) & !is.na(Winter.Visitor) ~ "LM",
                       !is.na(Summer.Visitor) & !is.na(Resident) ~ "LM",
                       !is.na(Summer.Visitor) ~ "S",
                       !is.na(Winter.Visitor) | !is.na(Strictly.Passage) ~ "W/P",
                       !is.na(Uncertain.Vagrant) & is.na(Resident) ~ "U",
                       TRUE ~ "R")
    ) %>%
    select(eBird.English.Name,mig)
  
  migstatus$mig[migstatus$eBird.English.Name %in% c("Himalayan Cuckoo","Common Cuckoo",
                                                    "Watercock")] = "S"
  
  migstatus$mig[migstatus$eBird.English.Name %in% c("Indian Skimmer","Black-bellied Tern",
                                                    "Black-capped Kingfisher",
                                                    "Mountain Chiffchaff","Red-rumped Swallow",
                                                    "Fire-capped Tit")] = "R"
  
  migstatus$mig[migstatus$eBird.English.Name %in% c("Smoky Warbler","Wallcreeper",
                                                    "Long-billed Pipit")] = "W/P"
  
  if(rerun)
  {
    species = datatofill$species
    
    temp = datatofill %>%
      filter((!is.na(trivB) & (is.na(occB) | is.na(occB.ci))) | 
               (!is.na(trivM) & (is.na(occM) | is.na(occM.ci))))
    
    species = species[species %in% temp$species]
  }
  
  migstatus = migstatus %>%
    filter(eBird.English.Name %in% species)
  
  species = as.character(migstatus$eBird.English.Name)
  speciesf = species
  mig = migstatus$mig
  migf = mig
  
  spec = species[mig == "LM"]
  species = c(species,spec)
  mig[mig == "LM"] = "MS"
  mig = c(mig,rep("MW",length(spec)))
  
  data = data %>%
    filter(year > 2013)
  
  data = data %>%
    mutate(OBSERVATION.COUNT = replace(OBSERVATION.COUNT, !is.na(OBSERVATION.COUNT), "1"))
  
  data$OBSERVATION.COUNT = as.numeric(data$OBSERVATION.COUNT)
  
  # create dataframe to store occupancy and detection proabability 
  # estimates across species and spatial resolutions
  

  est = array(data=NA,dim=c(length(speciesf),11),
              dimnames=list(speciesf,c("detprobB","occB","occB.ci","trivB","sampareaB","detprobM",
                                       "occM","occM.ci","trivM","sampareaM","status")))
  
  if(rerun)
  {
    est[,1] = datatofill$detprobB
    est[,2] = datatofill$occB*10000
    est[,3] = datatofill$occB.ci*10000
    est[,4] = datatofill$trivB*10000
    est[,5] = datatofill$sampareaB*10000
    est[,6] = datatofill$detprobM
    est[,7] = datatofill$occM*10000
    est[,8] = datatofill$occM.ci*10000
    est[,9] = datatofill$trivM*10000
    est[,10] = datatofill$sampareaM*10000
    est[,11] = datatofill$migstatus
    
    if (migstatus == "LM")
    {
      index = 1:length(species)
      x = numeric(0)
      
      for (i in unique(species))
      {
        temp = datatofill %>%
          filter(species == i, migstatus == "LM")
        
        if (length(temp$species) != 0)
        {
          if (!is.na(temp$trivB) & (!is.na(temp$occB) & !is.na(temp$occB.ci)))
          {
            x1 = intersect(index[species == i], index[mig == "MS"])
            x = c(x,index[x1])
          }
          if (!is.na(temp$trivM) & (!is.na(temp$occM) & !is.na(temp$occM.ci)))
          {
            x2 = intersect(index[species == i], index[mig == "MW"])
            x = c(x,index[x2])
          }
        }
      }
      species = species[-x]
      mig = mig[-x]
    }
    

  }

  for(s in 1:length(species))
  {
    if(rerun)
    {
      if (is.na(datatofill$sampareaB[s]) & is.na(datatofill$sampareaM[s]))
        next
    }
    
    if (mig[s] == "S" | mig[s] == "W/P")
    {
      temp1 = data %>%
        filter(COMMON.NAME == species[s]) %>%
        distinct(month)
      
      datac = temp1 %>% left_join(data)
    }
    
    if (mig[s] == "R" | mig[s] == "U")
    {
      datac = data
    }
    
    if (mig[s] == "MS")
    {
      datac = data %>%
        filter(month %in% c(5:8))
    }
    
    if (mig[s] == "MW")
    {
      datac = data %>%
        filter(month %in% c(11:12,1:2))
    }
    
    datay = datac %>%
      group_by(gridg1,group.id) %>% slice(1) %>% ungroup %>%
      group_by(gridg1) %>% reframe(medianlla = median(no.sp)) %>%
      group_by(gridg1) %>%
      reframe(medianlla = round(mean(medianlla)))
    medianlla = datay$medianlla
    
    sampledarea = left_join(datac,areag,by = c("gridg1" = "id"))
    sampledarea = sampledarea %>% distinct(gridg1,area)
    len = length(sampledarea$area)
    sampledarea = sum(sampledarea$area)
    
    if (length(datac$COMMON.NAME[datac$COMMON.NAME == species[s]]) == 0)
    {
      estdf = data.frame(rep(rownames(est)))
      names(estdf) = "species"
      
      estdf$detprobB = NA
      estdf$occB = NA
      estdf$occB.ci = NA
      estdf$trivB = NA
      estdf$sampareaB = NA
      estdf$detprobM = NA
      estdf$occM = NA
      estdf$occM.ci = NA
      estdf$trivM = NA
      estdf$sampareaM = NA
      
      names(migstatus)[2] = "migstatus"
      estdf = left_join(estdf,migstatus,by = c("species" = "eBird.English.Name"))
      
      return(estdf)
    }
    
    selexp = expandbyspecies(datac,species[s])
    
    selexp = selexp[sample(1:nrow(selexp)),]
    
    selexp$month[selexp$month %in% c(11,12,1,2)] = "Win"
    selexp$month[selexp$month %in% c(3,4,5,6)] = "Sum"
    selexp$month[selexp$month %in% c(7,8,9,10)] = "Mon"
    
    nb8g = nb8g1
    lpg = selexp %>%
      group_by(gridg1) %>% summarize(lpg = n())
    listcutoff = quantile(lpg$lpg, 0.95, na.rm=TRUE)
    inc = datac %>%
      mutate(gridg = gridg1)
    selexp = selexp %>% 
      arrange(gridg1) %>%
      mutate(gridg = gridg1) %>%
      group_by(gridg) %>% mutate(group.id = 1:n()) %>% ungroup %>%
      left_join(areag,by = c("gridg" = "id"))
      
    
    nbt = selexp %>%
      group_by(gridg) %>% summarize(fl = sum(OBSERVATION.COUNT)) %>%
      mutate(fl=replace(fl, fl > 1, 1))
    nbt$nb8 = 0
      
    nbti = inc %>%
      filter(COMMON.NAME == species[s]) %>%
      left_join(areag,by = c("gridg" = "id")) %>%
      group_by(gridg) %>% summarize(fl = sum(OBSERVATION.COUNT), area = mean(area)) %>%
      mutate(fl=replace(fl, fl > 1, 1))
      
    fil = sum(nbti$fl*nbti$area)
      
      
    setDT(selexp)
    
    det = dcast(selexp, gridg ~ group.id, value.var = "OBSERVATION.COUNT")
    cov.month = dcast(selexp, gridg ~ group.id, value.var = "month")
    cov.nosp = dcast(selexp, gridg ~ group.id, value.var = "no.sp")
    
    det = setDF(det)
    cov.month = setDF(cov.month)
    cov.nosp = setDF(cov.nosp)
    
    det = det[,1:listcutoff]
    cov.month = cov.month[,1:listcutoff]
    cov.nosp = cov.nosp[,1:listcutoff]
      
    nbt$gridg = as.character(nbt$gridg)
    nbti$gridg = as.character(nbti$gridg)

    for (i in 1:length(nbt$gridg))
    {
      temp = as.numeric(nb8g[[nbt$gridg[i]]])
      sm = sum(nbti[nbti$gridg %in% temp,]$fl)/length(temp)
      nbt$nb8[i] = sm
    }
        
    nbt$gridg = as.character(nbt$gridg)
    tp = nbt
    tp1 = nbt %>% select(-fl)
    #tp = left_join(nbti,tp1)
    nbt = nbt[,-2]
    
    nbtx = tp[tp$fl != 1,]
    
    detn = data.frame(gridg = det[,1])
    detn= left_join(detn,nbt)

    umf = unmarkedFrameOccu(y=det[,-1], siteCovs = data.frame(nb8g = detn$nb8), 
                            obsCovs = list(cov1 = cov.nosp[,-1], 
                            cov2 = cov.month[,-1]))
    
    
    if (mig[s] == "R")
    {
      occ_det = tryCatch({occu(~log(cov1)*cov2 ~nb8g, data=umf, starts = c(0,0,0,0,0,0,0,0))},
                         error = function(cond){"skip"})
      
      newdat1 = data.frame(cov1=medianlla, cov2=factor(c("Mon","Win","Sum")))
      newdat2 = data.frame(nb8g=nbtx$nb8)
    }
    
    if (mig[s] != "R")
    {
      occ_det = tryCatch({occu(~log(cov1) ~nb8g, data=umf, starts = c(0,0,0,0))},
                         error = function(cond){"skip"})
      
      newdat1 = data.frame(cov1=medianlla)
      newdat2 = data.frame(nb8g=nbtx$nb8)
    }
    

    if (!is.character(occ_det))
    {
      f1 = predict(occ_det, newdata = newdat1, type = "det")
      f1 = mean(f1$Predicted)
      f2 = predict(occ_det, newdata = newdat2, type = "state")
      f2$nb = newdat2$nb8g
      f2$gridg = nbtx$gridg
      f2 = left_join(f2,areag,by = c("gridg" = "id"))
      f2 = f2 %>% filter(!is.na(Predicted))
      f2a = sum(f2$Predicted*f2$area) + fil
      f2b = round((erroradd(f2$SE*f2$area))*1.96)
      
      
      if (mig[s] == "R" | mig[s] == "MS" | mig[s] == "S")
      {
        est[species[s],"detprobB"] =  f1
        est[species[s],"occB"] = f2a
        est[species[s],"occB.ci"] = f2b
      }
      
      if (mig[s] == "W/P" | mig[s] == "MW" | mig[s] == "U")
      {
        est[species[s],"detprobM"] =  f1
        est[species[s],"occM"] = f2a
        est[species[s],"occM.ci"] = f2b
      }

    }
    
    if (mig[s] == "R" | mig[s] == "MS" | mig[s] == "S")
    {
      est[species[s],"trivB"] = fil
      est[species[s],"sampareaB"] = sampledarea
    }
    
    if (mig[s] == "W/P" | mig[s] == "MW" | mig[s] == "U")
    {
      est[species[s],"trivM"] = fil
      est[species[s],"sampareaM"] = sampledarea
    }
  }  
  estdf = data.frame(rep(rownames(est)))
  names(estdf) = "species"

  
  estdf$detprobB = round(as.numeric(est[,1]),3)
  estdf$occB = round(as.numeric(est[,2]),3)/10000
  estdf$occB.ci = round(as.numeric(est[,3]),3)/10000
  estdf$trivB = round(as.numeric(est[,4]),3)/10000
  estdf$sampareaB = round(as.numeric(est[,5]),3)/10000
  estdf$detprobM = round(as.numeric(est[,6]),3)
  estdf$occM = round(as.numeric(est[,7]),3)/10000
  estdf$occM.ci = round(as.numeric(est[,8]),3)/10000
  estdf$trivM = round(as.numeric(est[,9]),3)/10000
  estdf$sampareaM = round(as.numeric(est[,10]),3)/10000
  
  names(migstatus)[2] = "migstatus"
  estdf = left_join(estdf,migstatus,by = c("species" = "eBird.English.Name"))

  return(estdf)
}

SoIBoccupancy = function(data,species,areag)
{
  a = occufreq(data,species,areag)
  c = 0
  repeat
  {
    c = c + 1
    temp = a %>%
      filter((!is.na(trivB) & (is.na(occB) | is.na(occB.ci))) | 
               (!is.na(trivM) & (is.na(occM) | is.na(occM.ci))))
    if(length(temp$species) == 0)
      break
    if(c == 10)
      break
    a = occufreq(data,species,areag,rerun=T,datatofill=a)
  }
  return(a)
}



#### create a set of locations
## locs is a data frame with location, group id info

createrandomlocs = function(locs)
{
  require(tidyverse)
  locs1 = locs %>% 
    group_by(LOCALITY.ID,month,timegroups) %>% sample_n(1)
  return(locs1$group.id)
}



singlespeciesrun = function(data,species,specieslist,restrictedspecieslist)
{
  require(tidyverse)
  require(merTools)
  
  data1 = data
  specieslist1 = specieslist %>% filter(COMMON.NAME %in% species)
  
  specieslist2 = specieslist1 %>%
    dplyr::filter(COMMON.NAME == species)
  
  flag = 0
  if (species %in% restrictedspecieslist$COMMON.NAME)
  {
    flag = 1
    restrictedlist1 = restrictedspecieslist %>% filter(COMMON.NAME == species)
    specieslist2$ht = restrictedlist1$ht
    specieslist2$rt = restrictedlist1$rt
    
    if (restrictedlist1$mixed == 0)
      flag = 2
  }
  
  ## filters data based on whether the species has been selected for long-term trends (ht) 
  ## or short-term trends (rt) 
  
  if (is.na(specieslist2$ht) & !is.na(specieslist2$rt))
  {
    data1 = data1 %>%
      filter(year >= 2015)
  }
  
  temp = data1 %>%
    filter(COMMON.NAME == species) %>%
    distinct(gridg3,month)
  data1 = temp %>% left_join(data1)
  
  datay = data1 %>%
    group_by(gridg3,gridg1,group.id) %>% slice(1) %>% ungroup %>%
    group_by(gridg3,gridg1) %>% reframe(medianlla = median(no.sp)) %>%
    group_by(gridg3) %>% reframe(medianlla = mean(medianlla)) %>%
    reframe(medianlla = round(mean(medianlla)))
  
  medianlla = datay$medianlla
  
  
  ## expand dataframe to include absences as well
  
  ed = expandbyspecies(data1,species)
  
  ed$month = as.numeric(ed$month)
  ed$month[ed$month %in% c(12,1,2)] = "Win"
  ed$month[ed$month %in% c(3,4,5)] = "Sum"
  ed$month[ed$month %in% c(6,7,8)] = "Mon"
  ed$month[ed$month %in% c(9,10,11)] = "Aut"
  ed$month = as.factor(ed$month)
  
  tm = unique(data1$timegroups)
  #rm(data, pos = ".GlobalEnv")
  
  ## the model
  
  if (flag == 0)
  {
    m1 = glmer(OBSERVATION.COUNT ~ month + month:log(no.sp) + timegroups + (1|gridg3/gridg1), data = ed, 
               family=binomial(link = 'cloglog'), nAGQ = 0, control = glmerControl(optimizer = "bobyqa"))
  }
  
  if (flag == 1)
  {
    m1 = glmer(OBSERVATION.COUNT ~ month + month:log(no.sp) + timegroups + (1|gridg1), data = ed, 
               family=binomial(link = 'cloglog'), nAGQ = 0, control = glmerControl(optimizer = "bobyqa"))
  }
  
  if (flag == 2)
  {
    m1 = glm(OBSERVATION.COUNT ~ month + month:log(no.sp) + timegroups, data = ed, 
             family=binomial(link = 'cloglog'))
  }
  
  ## prepare a new data file to predict
  
  f = data.frame(unique(tm))
  f = do.call("rbind", replicate(length(unique(ed$month)),f,simplify=F))
  names(f) = "timegroups"
  f$month = rep(unique(ed$month), each = length(f$timegroups)/length(unique(ed$month)))
  ltemp = data.frame(timegroups = f$timegroups,
                     no.sp = medianlla, month = f$month,
                     gridg1 = data1$gridg1[1], gridg3 = data1$gridg3[1])
  
  f1 = data.frame(timegroups = tm)
  
  f2 = data.frame(freq = numeric(length(ltemp$no.sp)))
  f2$se = numeric(length(ltemp$no.sp))
  f2$timegroups = ltemp$timegroups
  
  
  if (flag != 2)
  {
    #pred = predict(m1, newdata = ltemp, type = "response", re.form = NA, allow.new.levels=TRUE)
    pred = predictInterval(m1, newdata = ltemp, which = "fixed",
                        level = 0.48, type = "linear.prediction")
    f2$freqt = pred$fit
    f2$set = pred$fit-pred$lwr
  }
  
  if (flag == 2)
  {
    pred = predict(m1, newdata = ltemp, type = "link", se.fit = T)
    f2$freqt = pred$fit
    f2$set = pred$se.fit
  }
  
  fx = f2 %>%
    filter(!is.na(freqt) & !is.na(se)) %>%
    group_by(timegroups) %>% reframe(freq = mean(freqt), se = mean(set))
  f1 = left_join(f1,fx)
  
  f1$timegroups = factor(f1$timegroups, levels = c("before 2000","2000-2006","2007-2010",
                                                   "2011-2012","2013","2014","2015","2016",
                                                   "2017","2018","2019","2020","2021","2022"))
  f1 = f1[order(f1$timegroups),]
  names(f1)[1] = "timegroupsf"
  mp = data.frame(timegroupsf = c("before 2000","2000-2006","2007-2010",
                                  "2011-2012","2013","2014","2015","2016",
                                  "2017","2018","2019","2020","2021","2022"), 
                  timegroups = as.numeric(databins$year))
  f1 = left_join(mp,f1)
  
  tocomb = c(species,f1$freq,f1$se)
  return(tocomb)
}
