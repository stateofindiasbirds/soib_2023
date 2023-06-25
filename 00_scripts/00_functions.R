
### 01 readcleanrawdata ########################################

## read and clean raw data and add important columns like group id, seasonality variables
## place raw txt file (India download) in working directory 

readcleanrawdata = function(rawpath = "00_data/ebd_IN_relMay-2023.txt", 
                            sensitivepath = "00_data/ebd_sensitive_relMay-2023_IN.txt")
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
  
  write.csv(temp,"00_data/indiaspecieslist.csv", row.names=FALSE)
  
  ## create location file for LULC
  
  locdat = data %>% distinct(LOCALITY.ID,LATITUDE,LONGITUDE)
  write.csv(locdat,"00_data/eBird_location_data.csv", row.names=FALSE)
  
  
  ## choosing important columns required for further analyses
  
  imp = c("CATEGORY","COMMON.NAME","OBSERVATION.COUNT",
          "LOCALITY.ID", "REVIEWED","APPROVED","EXOTIC.CODE",
          "LOCALITY.TYPE","STATE","COUNTY",
          "LATITUDE","LONGITUDE","OBSERVATION.DATE","TIME.OBSERVATIONS.STARTED",
          "OBSERVER.ID","PROTOCOL.TYPE",
          "DURATION.MINUTES","EFFORT.DISTANCE.KM",
          "ALL.SPECIES.REPORTED","group.id","SAMPLING.EVENT.IDENTIFIER")
  

  # no of days in every month, and cumulative number
  days = c(31,28,31,30,31,30,31,31,30,31,30,31)
  cdays = c(0,31,59,90,120,151,181,212,243,273,304,334)
  
  data = data %>%
    # create a column "group.id" which can help remove duplicate checklists
    mutate(group.id = ifelse(is.na(GROUP.IDENTIFIER), 
                             SAMPLING.EVENT.IDENTIFIER, GROUP.IDENTIFIER)) %>%
    dplyr::select(all_of(imp)) %>%
    # other useful columns
    # set date, add month, year and day columns using package LUBRIDATE
    mutate(OBSERVATION.DATE = as.Date(OBSERVATION.DATE), 
           month = month(OBSERVATION.DATE),
           day = day(OBSERVATION.DATE) + cdays[month], 
           #week = week(OBSERVATION.DATE),
           #fort = ceiling(day/14),
           cyear = year(OBSERVATION.DATE)) %>%
    dplyr::select(-c("OBSERVATION.DATE")) %>%
    mutate(year = ifelse(month > 5, cyear, cyear-1)) %>% # from June to May
    # add number of species/list length column (no.sp), for list length analyses (lla)
    group_by(group.id) %>% 
    mutate(no.sp = n_distinct(COMMON.NAME)) %>%
    ungroup()
  
  
  # remove probable mistakes
  source("00_scripts/rm_prob_mistakes.R")
  data <- rm_prob_mistakes(data)

  
  # need to combine several closely related species and slashes/spuhs
  # so, first changing their category to species since they will be combined next
  data = data %>%
    mutate(CATEGORY = case_when(COMMON.NAME %in% c(
      "Green/Greenish Warbler", "Siberian/Amur Stonechat", "Red-necked/Little Stint",
      "Western/Eastern Yellow Wagtail", "Common/Himalayan Buzzard",
      "Eurasian/Eastern Marsh-Harrier", "Lesser/Greater Sand-Plover", "Baikal/Spotted Bush Warbler",
      "Lemon-rumped/Sichuan Leaf Warbler", "Red-rumped/Striated Swallow",
      "Bank Swallow/Pale Sand Martin", "Riparia sp.", "Greater/Mongolian Short-toed Lark",
      "Taiga/Red-breasted Flycatcher", "Tricolored x Chestnut Munia (hybrid)", "Little/House Swift", 
      "Pin-tailed/Swinhoe's Snipe", "Booted/Sykes's Warbler", "Iduna sp."
      ) ~ "species",
      TRUE ~ CATEGORY)) %>%
    # combining species, slashes and spuhs
    mutate(COMMON.NAME = case_when(
      COMMON.NAME %in% c("Green Warbler", "Green/Greenish Warbler") ~ "Greenish Warbler",
      COMMON.NAME %in% c("Amur Stonechat", "Siberian/Amur Stonechat") ~ "Siberian Stonechat",
      COMMON.NAME %in% c("Red-necked Stint", "Red-necked/Little Stint") ~ "Little Stint",
      COMMON.NAME %in% c("Eastern Yellow Wagtail", 
                         "Western/Eastern Yellow Wagtail") ~ "Western Yellow Wagtail",
      COMMON.NAME %in% c("Himalayan Buzzard", 
                         "Common/Himalayan Buzzard") ~ "Common Buzzard",
      COMMON.NAME %in% c("Eastern Marsh-Harrier", 
                         "Eurasian/Eastern Marsh-Harrier") ~ "Eurasian Marsh-Harrier",
      COMMON.NAME %in% c("Greater Sand-Plover", 
                         "Lesser/Greater Sand-Plover") ~ "Lesser Sand-Plover",
      COMMON.NAME %in% c("Baikal Bush Warbler", 
                         "Baikal/Spotted Bush Warbler") ~ "Spotted Bush Warbler",
      COMMON.NAME %in% c("Sichuan Leaf Warbler", 
                         "Lemon-rumped/Sichuan Leaf Warbler") ~ "Lemon-rumped Warbler",
      COMMON.NAME %in% c("Striated Swallow", 
                         "Red-rumped/Striated Swallow") ~ "Red-rumped Swallow",
      COMMON.NAME %in% c("Pale Sand Martin", "Bank Swallow/Pale Sand Martin", 
                          "Riparia sp.") ~ "Gray-throated Martin",
      COMMON.NAME %in% c("Mongolian Short-toed Lark", 
                         "Greater/Mongolian Short-toed Lark") ~ "Greater Short-toed Lark",
      COMMON.NAME %in% c("Taiga Flycatcher", 
                         "Taiga/Red-breasted Flycatcher") ~ "Red-breasted Flycatcher",
      COMMON.NAME %in% c("Chestnut Munia", 
                         "Tricolored x Chestnut Munia (hybrid)") ~ "Tricolored Munia",
      COMMON.NAME %in% c("House Swift", "Little/House Swift") ~ "Little Swift",
      COMMON.NAME %in% c("Swinhoe's Snipe", 
                         "Pin-tailed/Swinhoe's Snipe") ~ "Pin-tailed Snipe",
      COMMON.NAME %in% c("Sykes's Warbler", "Booted/Sykes's Warbler",
                         "Iduna sp.") ~ "Booted Warbler",
      TRUE ~ COMMON.NAME
    ))
  
  
  ## setup eBird data ##
  
  ## slice by single group ID, remove repetitions
  ## remove repeats by retaining only a single group.id + species combination
  
  data = data %>%
    group_by(group.id, COMMON.NAME) %>% 
    slice(1) %>% 
    ungroup() %>% 
    filter(year < 2023) %>% 
    rename(ST_NM = STATE,
           DISTRICT = COUNTY) %>% 
    dplyr::select(-SAMPLING.EVENT.IDENTIFIER)
  
  assign("data", data, .GlobalEnv)
  
  # save workspace
  save(data, file = "00_data/rawdata.RData")
  rm(data, pos = ".GlobalEnv")
  
}

### 02 addmapvars ########################################

# Load sf map objects and add to dataset. 
# - admin & PA boundaries;
# - square grids at 5 resolutions (5, 25, 50, 100, 200 km*km), unclipped and clipped to India;
# - queen and rook neighbours info for 4 grids (25, 50, 100, 200)
# See the India Maps repo:
# https://github.com/birdcountindia/india-maps/blob/main/scripts/create_maps_sf.R

addmapvars = function(datapath = "00_data/rawdata.RData", 
                      mappath1 = "00_data/grids_sf_full.RData", 
                      mappath2 = "00_data/grids_g0_sf.RData",
                      mappath3 = "00_data/maps_sf.RData",
                      papath = "00_data/maps_pa_sf.RData",
                      maskspath = "00_data/habmasks_sf.RData")
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
  
  # to later filter pelagics
  india_buff_sf <- india_buff_sf %>% mutate(INLAND = 1)

  
  
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
    # grid cells
    st_join(g0_sf %>% dplyr::select(GRID.G0)) %>% 
    st_join(g1_sf %>% dplyr::select(GRID.G1)) %>% 
    st_join(g2_sf %>% dplyr::select(GRID.G2)) %>% 
    st_join(g3_sf %>% dplyr::select(GRID.G3)) %>% 
    st_join(g4_sf %>% dplyr::select(GRID.G4)) %>% 
    st_join(india_buff_sf %>% dplyr::select(INLAND)) %>% 
    st_drop_geometry()
  
  temp = temp %>% 
    distinct(NAME,GRID.G0,GRID.G1,GRID.G2,GRID.G3,GRID.G4,group.id,X) %>% 
    group_by(group.id) %>% 
    slice(1) %>% 
    ungroup() %>% 
    magrittr::set_colnames(c("pa.name","gridg0","gridg1","gridg2","gridg3",
                             "gridg4","group.id","INLAND"))
  
  data = data %>% 
    left_join(temp) %>% 
    # removes pelagics
    filter(!is.na(INLAND)) %>% 
    dplyr::select(-INLAND) %>% 
    left_join(habmasks_sf)
  
  ### 
  
  assign("data",data,.GlobalEnv)

  save(data, file="00_data/data.RData")
  rm(data, pos = ".GlobalEnv")
  
}


###    completelistcheck ########################################

## remove all probable errors
## type can be "trends" or "range"
## to use in dataspeciesfilter()

completelistcheck = function(data)
{
  require(tidyverse)
  require(lubridate)

  data = data %>% 
    # create 2 columns from the "TIME.OBSERVATIONS.STARTED' column
    mutate(DATETIME = as_datetime(paste("2023-06-01", # any date, we just need the time
                                        TIME.OBSERVATIONS.STARTED)),
           hr = hour(DATETIME),
           min = minute(DATETIME)) %>% 
    # calculate speed and species/unit time (sut)
    mutate(speed = EFFORT.DISTANCE.KM*60/DURATION.MINUTES, # kmph
           sut = no.sp*60/DURATION.MINUTES, # species per hour
           # calculate hour checklist ended
           end = floor((hr*60 + min + DURATION.MINUTES)/60))
  
  # set thresholds for speed and sut
  vel = 20
  time = 2
  
  
  # exclude any list that may in fact be incomplete ###
  
  # list of on-paper complete lists
  temp = data %>%
    filter(ALL.SPECIES.REPORTED == 1, PROTOCOL.TYPE != "Incidental") %>%
    group_by(group.id) %>% slice(1)
  
  # choose checklists without info on duration with 3 or fewer species
  grp = temp %>%
    filter(no.sp <= 3, is.na(DURATION.MINUTES)) %>%
    distinct(group.id)
  grp = grp$group.id
  
  # exclude records based on various criteria 
  data = data %>%
    mutate(ALL.SPECIES.REPORTED = case_when(
      # fake complete lists
      ALL.SPECIES.REPORTED == 1 & 
        (EFFORT.DISTANCE.KM > 10 | # remove travelling lists covering >10 km
           group.id %in% grp | # lists without info on duration with 3 or fewer species
           speed > vel | # too fast
           (DURATION.MINUTES < 3) | # too short
           (sut < time & no.sp <= 3) | # species per unit time too slow
           PROTOCOL.TYPE == "Incidental" | # incidental
           (!is.na(hr) & ((hr <= 4 & end <= 4) | # nocturnal filter
                            (hr >= 20 & end <= 28)))) ~ 0, 
      # true incomplete lists
      ALL.SPECIES.REPORTED == 0 ~ 0,
      # true complete lists
      TRUE ~ 1
    )) %>% 
    dplyr::select(-speed,-sut,-hr,-min,-end) 
}

###    removevagrants ########################################

## remove vagrants
## to use in dataspeciesfilter()

removevagrants = function(data)
{
  # mapping of SoIB-species-of-interest to a range of variables/classifications
  # (manually created)
  fullmap = read.csv("00_data/SoIB_mapping_2022.csv")
  
  migspecies = fullmap %>%
    filter(!Migratory.Status.Within.India %in% c("Resident",
                                                 "Resident & Altitudinal Migrant",
                                                 "Resident & Local Migrant",
                                                 "Resident & Localized Summer Migrant",
                                                 "Altitudinal Migrant",
                                                 "Resident (Extirpated)")) %>%
    dplyr::select(eBird.English.Name.2022) %>% 
    as.vector() %>% 
    list_c()
  
  d = data %>%
    filter(COMMON.NAME %in% migspecies) %>%
    group_by(gridg4, month, COMMON.NAME) %>%
    reframe(nyear = n_distinct(year)) %>%
    filter(nyear <= 3) %>% 
    dplyr::select(gridg4, month, COMMON.NAME)
  
  d = left_join(d, data) %>%
    filter(year > 2014)
  
  save(d, file = "00_data/vagrantdata.RData")
  
  data = anti_join(data, d)
  return(data)
}



###    dataspeciesfilter ########################################

# <annotation_pending_AV> elaborate below
# select species for State of India's Birds, and species for historical and recent trends
# includes all diurnal endemics (endemicity) and essential species (SelectSpecies)

dataspeciesfilter = function(
    
  # thresholds for species to be considered in each analysis
  locationlimit = 15, # individual locations
  gridlimit = 4, # grid cells
  listlimit = 50, # checklists
  cur_mask = "none"
  
) {
  
  # ensuring only valid cur_mask names are provided
  if (!(cur_mask %in% unique(analyses_metadata$MASK))) {
    return('Invalid mask! Please provide valid mask name, one of: c("none","woodland","cropland","ONEland","PA").')
  }
  
  
  # preparing data for mask ###
  
  cur_metadata <- analyses_metadata %>% filter(MASK == cur_mask)
  
  if (cur_mask == "none"){
    data0 = data_base
  } else if (cur_mask == "woodland"){
    data0 = data_base %>% filter(maskWdl == 1)
  } else if (cur_mask == "cropland"){
    data0 = data_base %>% filter(maskCrp == 1)
  } else if (cur_mask == "ONEland"){
    data0 = data_base %>% filter(maskOne == 1)
  } else if (cur_mask == "PA"){
    data0 = data_base %>% filter(!is.na(pa.name))
  } else {
    # states
    data0 = data_base %>% filter(ST_NM == cur_mask)
  } 
  
  
  # processing dataspeciesfilter --------------------------------------------
  
  data = data0 %>% 
    dplyr::select(-CATEGORY,-REVIEWED,-APPROVED,-ST_NM,-DISTRICT,
                  -LOCALITY.TYPE,-LOCALITY.ID,-pa.name,-maskWdl,-maskCrp,-maskOne,
                  -LATITUDE,-LONGITUDE,-PROTOCOL.TYPE,-EXOTIC.CODE,-day,-cyear,
                  -DURATION.MINUTES,-TIME.OBSERVATIONS.STARTED,-EFFORT.DISTANCE.KM)
  
  stats7 = paste(nrow(data[data$ALL.SPECIES.REPORTED == 1,]),
                 "filter 1 usable observations")
  stats8 = paste(length(unique(data[data$ALL.SPECIES.REPORTED == 1,]$group.id)),
                 "filter 2 unique complete checklists")
  stats9 = paste(length(unique(data[data$timegroups == "before 2000" &
                                      data$ALL.SPECIES.REPORTED == 1,]$group.id)),
                 "pre-2000 checklists")
  
  # summary for each timegroup
  databins = data %>%
    filter(ALL.SPECIES.REPORTED == 1) %>%
    group_by(timegroups) %>% 
    reframe(lists = n_distinct(group.id), 
            year = round(median(year))) %>%
    arrange(year)
  
  
  # historical data (data from before 2000 onwards), used for long-term trends
  # gives list of species for which we have enough data and this analysis can be done
  datah = data0 %>%
    filter(ALL.SPECIES.REPORTED == 1, 
           CATEGORY %in% c("species", "issf")) %>%
    group_by(COMMON.NAME, timegroups) %>%
    reframe(locs = n_distinct(LOCALITY.ID), 
            cells = n_distinct(gridg4)) %>%
    group_by(COMMON.NAME, timegroups) %>%
    filter(locs > locationlimit, cells > gridlimit) %>%
    group_by(COMMON.NAME) %>% 
    reframe(years = n()) %>%
    group_by(COMMON.NAME) %>%
    filter(years == 14) %>%
    mutate(ht = 1) %>% 
    dplyr::select(COMMON.NAME, ht)
  
  # recent data (data from 2015 onwards), used for recent trends
  # gives list of species for which we have enough data and this analysis can be done
  datar = data0 %>%
    filter(ALL.SPECIES.REPORTED == 1, 
           CATEGORY %in% c("species", "issf"), 
           year > 2014) %>%
    group_by(COMMON.NAME, year) %>%
    reframe(locs = n_distinct(LOCALITY.ID), 
            cells = n_distinct(gridg4)) %>%
    group_by(COMMON.NAME, year) %>%
    filter(locs > locationlimit, cells > gridlimit) %>%
    group_by(COMMON.NAME) %>% 
    reframe(years = n()) %>%
    group_by(COMMON.NAME) %>%
    filter(years == 8) %>%
    mutate(rt = 1) %>% 
    dplyr::select(COMMON.NAME, rt)
  
  
  # for other species that don't qualify simple rules above (restricted range)
  dataresth1 = data0 %>%
    filter(ALL.SPECIES.REPORTED == 1, 
           CATEGORY %in% c("species", "issf")) %>%
    group_by(COMMON.NAME, timegroups) %>%
    reframe(cells = n_distinct(gridg4)) %>%
    group_by(COMMON.NAME, timegroups) %>%
    filter(cells <= gridlimit) %>%
    group_by(COMMON.NAME) %>% 
    reframe(years = n()) %>%
    group_by(COMMON.NAME) %>%
    filter(years == 14) %>%
    dplyr::select(COMMON.NAME)
  
  speciesresth = data.frame(species = intersect(unique(dataresth1$COMMON.NAME),
                                                spec_resident),
                            validh = NA_real_)
  
  # if the grids in which species has been reported a few times have sufficient lists
  # from enough years, still consider for analysis
  for (i in 1:length(speciesresth$species))
  {
    tempresth1 = data0 %>%
      filter(COMMON.NAME == speciesresth$species[i]) %>%
      distinct(gridg1) %>%
      left_join(data0) %>%
      group_by(timegroups) %>% 
      reframe(n = n_distinct(group.id)) %>%
      group_by(timegroups) %>%
      filter(n > listlimit)
    
    if (length(tempresth1$timegroups) == 14)
      speciesresth$validh[speciesresth$species == speciesresth$species[i]] = 1
    
  }
  
  datarestr1 = data0 %>%
    filter(ALL.SPECIES.REPORTED == 1, 
           CATEGORY %in% c("species", "issf"), 
           year > 2014) %>%
    group_by(COMMON.NAME, timegroups) %>%
    reframe(cells = n_distinct(gridg4)) %>%
    group_by(COMMON.NAME, timegroups) %>%
    filter(cells <= gridlimit) %>%
    group_by(COMMON.NAME) %>% 
    reframe(years = n()) %>%
    group_by(COMMON.NAME) %>%
    filter(years == 8) %>%
    dplyr::select(COMMON.NAME)
  
  speciesrestr = data.frame(species = intersect(unique(datarestr1$COMMON.NAME),
                                                spec_resident),
                            validr = NA_real_)
  
  for (i in 1:length(unique(datarestr1$COMMON.NAME)))
  {
    temprestr1 = data0 %>%
      filter(COMMON.NAME == speciesrestr$species[i]) %>%
      distinct(gridg1) %>%
      left_join(data0) %>%
      filter(year > 2014) %>%
      group_by(timegroups) %>% 
      reframe(n = n_distinct(group.id)) %>%
      group_by(timegroups) %>%
      filter(n > listlimit)
    
    if (length(temprestr1$timegroups) == 8)
      speciesrestr$validr[speciesrestr$species == speciesrestr$species[i]] = 1
    
  }
  
  
  # full species list (historical + recent) ###
  
  dataf = fullmap
  names(dataf)[1:2] = c("COMMON.NAME","SCIENTIFIC.NAME")
  
  # joining info for normal species (non-range-restricted)
  dataf = dataf %>% 
    left_join(datah, by = c("COMMON.NAME")) %>% 
    left_join(datar, by = c("COMMON.NAME"))
  
  specieslist = dataf %>%
    # <annotation_pending_AV> what does each variable mean? (e.g., Essential)
    # what are we finally filtering for?
    filter((Essential == 1 | Endemic.Region != "None" | ht == 1 | rt == 1) & 
             (Breeding.Activity.Period != "Nocturnal" | 
                Non.Breeding.Activity.Period != "Nocturnal" | 
                COMMON.NAME == "Jerdon's Courser") & 
             (is.na(Discard))) %>%
    dplyr::select(COMMON.NAME, ht, rt)
  
  # <annotation_pending_AV> why filtering dataf also? (instead of specieslist above)
  dataf <- dataf %>% 
    mutate(ht = case_when(Breeding.Activity.Period == "Nocturnal" &
                            Non.Breeding.Activity.Period == "Nocturnal" ~ NA_real_,
                          TRUE ~ ht),
           rt = case_when(Breeding.Activity.Period == "Nocturnal" &
                            Non.Breeding.Activity.Period == "Nocturnal" ~ NA_real_,
                          TRUE ~ rt))
  
  # ignoring species that are frequently misIDd
  specieslist <- specieslist %>% 
    mutate(ht = case_when(COMMON.NAME %in% spec_misid ~ NA_real_,
                          TRUE ~ ht),
           rt = case_when(COMMON.NAME %in% spec_misid ~ NA_real_,
                          TRUE ~ rt))
  
  
  # <annotation_pending_AV> 
  # why left_joining (then removing misIDd specs) separately again? 
  restrictedspecieslist = data.frame(species = specieslist$COMMON.NAME) %>% 
    left_join(speciesresth) %>% 
    left_join(speciesrestr) %>%
    # valid for at least 1 of 2 analyses
    filter(!is.na(validh) | !is.na(validr)) %>% 
    magrittr::set_colnames(c("COMMON.NAME", "ht", "rt")) %>% 
    mutate(ht = case_when(COMMON.NAME %in% spec_misid ~ NA_real_,
                          TRUE ~ ht),
           rt = case_when(COMMON.NAME %in% spec_misid ~ NA_real_,
                          TRUE ~ rt))  
  
  
  # filtering for only species in certain masks ###
  if (cur_mask == "woodland") {
    
    specieslist <- specieslist %>% 
      mutate(ht = case_when(!(COMMON.NAME %in% spec_woodland) ~ NA_real_,
                            TRUE ~ ht),
             rt = case_when(!(COMMON.NAME %in% spec_woodland) ~ NA_real_,
                            TRUE ~ rt))
    
    restrictedspecieslist = restrictedspecieslist %>% 
      mutate(ht = case_when(!(COMMON.NAME %in% spec_woodland) ~ NA_real_,
                            TRUE ~ ht),
             rt = case_when(!(COMMON.NAME %in% spec_woodland) ~ NA_real_,
                            TRUE ~ rt))
    
  } else if (cur_mask %in% c("cropland", "ONEland")) {
    
    specieslist <- specieslist %>% 
      mutate(ht = case_when(!(COMMON.NAME %in% spec_openland) ~ NA_real_,
                            TRUE ~ ht),
             rt = case_when(!(COMMON.NAME %in% spec_openland) ~ NA_real_,
                            TRUE ~ rt))
    
    restrictedspecieslist = restrictedspecieslist %>% 
      mutate(ht = case_when(!(COMMON.NAME %in% spec_openland) ~ NA_real_,
                            TRUE ~ ht),
             rt = case_when(!(COMMON.NAME %in% spec_openland) ~ NA_real_,
                            TRUE ~ rt))
    
  }
  
  
  # <annotation_pending_AV> what exactly are we checking?
  check1 = restrictedspecieslist %>% 
    filter(!is.na(ht)) %>% 
    dplyr::select(COMMON.NAME) %>% as.vector() %>% list_c()
  check2 = restrictedspecieslist %>% 
    filter(!is.na(rt)) %>% 
    dplyr::select(COMMON.NAME) %>% as.vector() %>% list_c()
  
  
  # <annotation_pending_AV> 
  randomcheck_a = data0 %>% 
    filter(ALL.SPECIES.REPORTED == 1, 
           COMMON.NAME %in% restrictedspecieslist$COMMON.NAME) %>%
    group_by(COMMON.NAME) %>% 
    reframe(n = n_distinct(gridg1)) %>%
    group_by(COMMON.NAME) %>% 
    filter(n > 7)
  
  randomcheck_b = data0 %>% 
    filter(ALL.SPECIES.REPORTED == 1, 
           COMMON.NAME %in% restrictedspecieslist$COMMON.NAME) %>%
    group_by(COMMON.NAME) %>% 
    reframe(n = n_distinct(gridg1)) %>%
    group_by(COMMON.NAME) %>% 
    filter(n <= 7)
  
  
  # <annotation_pending_AV> 
  restrictedspecieslist_a = restrictedspecieslist %>% 
    filter(COMMON.NAME %in% randomcheck_a$COMMON.NAME) %>% 
    mutate(mixed = 1)
  restrictedspecieslist_b = restrictedspecieslist %>% 
    filter(COMMON.NAME %in% randomcheck_b$COMMON.NAME) %>% 
    mutate(mixed = 0)
  
  restrictedspecieslist = rbind(restrictedspecieslist_a,restrictedspecieslist_b)
  
  
  # <annotation_pending_AV> 
  t1 = dataf %>%
    filter((ht == 1 | rt == 1) &
             (Breeding.Activity.Period != "Nocturnal" |
                Non.Breeding.Activity.Period != "Nocturnal"))
  t2 = dataf %>%
    filter((Endemic.Region != "None" | ht == 1 | rt == 1) & 
             (Breeding.Activity.Period != "Nocturnal" |
                Non.Breeding.Activity.Period != "Nocturnal"))
  t3 = dataf %>%
    filter((Essential == 1 | Endemic.Region != "None" | ht == 1 | rt == 1) &
             (Breeding.Activity.Period != "Nocturnal" |
                Non.Breeding.Activity.Period != "Nocturnal"))
  
  stats10 = paste(length(t1$COMMON.NAME),"filter 1 number of species")
  stats11 = paste(length(t2$COMMON.NAME),"filter 2 number of species")
  stats12 = paste(length(t3$COMMON.NAME),"filter 3 number of species")
  
  
  # <annotation_pending_AV> 
  specieslist1 = specieslist %>% 
    mutate(selected = 1) %>% 
    dplyr::select(COMMON.NAME, selected)
  
  dataf = dataf %>%
    dplyr::select(COMMON.NAME, SCIENTIFIC.NAME, ht, rt) %>% 
    left_join(specieslist1) %>% 
    magrittr::set_colnames(c("COMMON.NAME","SCIENTIFIC.NAME",
                             "Long.Term.Analysis","Current.Analysis",
                             "Selected.SOIB")) %>%  
    # converting to report table-style with blanks for NAs and Xs for 1s
    mutate(across(everything(), ~ as.character(.))) %>% 
    mutate(across(everything(), ~ replace_na(., replace = ""))) %>% 
    mutate(across(everything(), ~ str_replace(., pattern = "1", replacement = "X"))) %>% 
    # also including species in checks
    mutate(Long.Term.Analysis = if_else(COMMON.NAME %in% check1, "X", Long.Term.Analysis),
           Current.Analysis = if_else(COMMON.NAME %in% check2, "X", Current.Analysis)) %>%
    dplyr::select("COMMON.NAME","SCIENTIFIC.NAME","Long.Term.Analysis","Current.Analysis",
                  "Selected.SOIB")
  
  
  # filtering for only species in certain masks ###
  if (cur_mask == "woodland") {
    
    dataf <- dataf %>% 
      mutate(Long.Term.Analysis = if_else(!(COMMON.NAME %in% spec_woodland), "", Long.Term.Analysis),
             Current.Analysis = if_else(!(COMMON.NAME %in% spec_woodland), "", Current.Analysis),
             Selected.SOIB = if_else(!(COMMON.NAME %in% spec_woodland), "", Selected.SOIB))
    
  } else if (cur_mask %in% c("cropland", "ONEland")) {
    
    dataf <- dataf %>% 
      mutate(Long.Term.Analysis = if_else(!(COMMON.NAME %in% spec_openland), "", Long.Term.Analysis),
             Current.Analysis = if_else(!(COMMON.NAME %in% spec_openland), "", Current.Analysis),
             Selected.SOIB = if_else(!(COMMON.NAME %in% spec_openland), "", Selected.SOIB))
    
  }
  
  
  # number of sampled grid cell at each resolution
  sampledcells = c(length(unique(data0$gridg0)),
                   length(unique(data0$gridg1)),
                   length(unique(data0$gridg2)),
                   length(unique(data0$gridg3)),
                   length(unique(data0$gridg4)))
  
  stats = c(stats1, stats2, stats3, stats4, stats5, stats6,
            stats7, stats8, stats9, stats10, stats11, stats12)
  
  
  
  # additional filtering safeguards - proportion of range sampled during every timegroup
  
  temp = data %>%
    filter(COMMON.NAME %in% dataf$COMMON.NAME, 
           ALL.SPECIES.REPORTED == 1)
  
  totalrange = temp %>%
    group_by(COMMON.NAME) %>% 
    reframe(totalrange25km = n_distinct(gridg1))
  
  proprange2000 = temp %>%
    filter(timegroups == "before 2000") %>%
    group_by(COMMON.NAME) %>% 
    reframe(proprange25km2000 = n_distinct(gridg1))
  
  proprange2022 = temp %>%
    filter(timegroups == "2022") %>%
    group_by(COMMON.NAME) %>% 
    reframe(proprange25km2022 = n_distinct(gridg1))
  
  proprange.current = temp %>%
    filter(timegroups %in% as.character(2015:2022)) %>%
    group_by(COMMON.NAME, timegroups) %>% 
    reframe(proprange25km.current = n_distinct(gridg1)) %>%
    group_by(COMMON.NAME) %>% 
    reframe(proprange25km.current = mean(proprange25km.current))
  
  range25km = totalrange %>% 
    left_join(proprange2000) %>% 
    left_join(proprange.current) %>% 
    left_join(proprange2022) %>%
    mutate(proprange25km2000 = proprange25km2000/totalrange25km,
           proprange25km.current = proprange25km.current/totalrange25km,
           proprange25km2022 = proprange25km2022/totalrange25km)
  
  
  # additional filtering safeguards - proportional sampling within each 25km grid cell
  
  samp5km = data %>%
    filter(ALL.SPECIES.REPORTED == 1) %>%
    group_by(gridg1) %>% 
    reframe(n = n_distinct(gridg0))
  
  spec25km = data %>%
    filter(ALL.SPECIES.REPORTED == 1,
           COMMON.NAME %in% dataf$COMMON.NAME) %>%
    distinct(COMMON.NAME, gridg1)
  
  samp25km5km = spec25km %>% 
    left_join(samp5km) %>%
    group_by(COMMON.NAME) %>% 
    reframe(mean5km = mean(n), 
            ci5km = 1.96*sd(n)/sqrt(n()))
  
  dataf = dataf %>% left_join(range25km) %>% left_join(samp25km5km)
  
  
  # writing filtered data files ---------------------------------------------
  
  # <annotation_pending_AV> short, about each file saved
  
  
  # <annotation_pending_AV>
  write.csv(dataf, row.names = F, 
            file = cur_metadata$FULLSPECLIST.PATH)
  
  
  # <annotation_pending_AV>
  locs_write = data0 %>% 
    filter(ALL.SPECIES.REPORTED == 1) %>%
    distinct(LOCALITY.ID, group.id, month, timegroups)
  
  write.csv(locs_write, row.names = F, 
            file = cur_metadata$LOCS.PATH)
  
  
  # <annotation_pending_AV>
  save(specieslist, restrictedspecieslist, databins, 
       file = cur_metadata$SPECLISTDATA.PATH)
  
  save(data, sampledcells, databins, stats, 
       file = cur_metadata$DATA.PATH)
  
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

# <annotation_pending_AV>
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

# <annotation_pending_AV>
simerrordiv = function(x1, x2, se1, se2)
{
  # takes untransformed (link) mean and SE values, and generates normal dist. from 1000 sims,
  # then transformed 
  # after the function, lower and upper quantiles are selected as limits of 95% CI
  tp = data.frame(num = clogloglink(rnorm(1000, x1, se1), inverse = T), 
                  den = clogloglink(rnorm(1000, x2, se2), inverse = T)) %>%
    reframe(rat = num/den, 
            val = num)

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
  
  load("00_data/neighbours.RData")
  
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



###    create a set of locations ########################################

# <annotation_pending_AV> why do we do this in the first place?

# locs is a data frame with location, group id info

createrandomlocs = function(locs)
{
  require(tidyverse)
  
  locs1 = locs %>% 
    group_by(LOCALITY.ID, month, timegroups) %>% sample_n(1)
  
  return(locs1$group.id)
}


###    singlespeciesrun (run models) ########################################

singlespeciesrun = function(data, species, specieslist, restrictedspecieslist)
{
  require(tidyverse)
  require(merTools)
  
  data1 = data
  
  # <annotation_pending_AV> why this intermediate specieslist1 object? 
  # can't we do with just the second one?
  specieslist1 = specieslist %>% filter(COMMON.NAME %in% species)
  specieslist2 = specieslist1 %>% filter(COMMON.NAME == species)
  
  # three different flags for three different model types that will be run.
  # 0 is normal model, with full random effects. depending on restricted species,
  # model changes slightly.
  flag = 0
  if (species %in% restrictedspecieslist$COMMON.NAME)
  {
    flag = 1
    restrictedlist1 = restrictedspecieslist %>% filter(COMMON.NAME == species)
    specieslist2$ht = restrictedlist1$ht
    specieslist2$rt = restrictedlist1$rt
    
    if (restrictedlist1$mixed == 0) {
      flag = 2
    }
  }
  
  # filters data based on whether the species has been selected for long-term trends (ht) 
  # or short-term trends (rt) 
  # (if only recent, then need to filter for recent years. else, use all years so no filter.)
  
  if (is.na(specieslist2$ht) & !is.na(specieslist2$rt))
  {
    data1 = data1 %>% filter(year >= 2015)
  }
  
  data1 = data1 %>%
    filter(COMMON.NAME == species) %>%
    distinct(gridg3, month) %>% 
    left_join(data1)
  
  tm = data1 %>% distinct(timegroups)
  #rm(data, pos = ".GlobalEnv")
  
  datay = data1 %>%
    group_by(gridg3, gridg1, group.id) %>% 
    slice(1) %>% 
    group_by(gridg3, gridg1) %>% 
    reframe(medianlla = median(no.sp)) %>%
    group_by(gridg3) %>% 
    reframe(medianlla = mean(medianlla)) %>%
    reframe(medianlla = round(mean(medianlla)))
  
  medianlla = datay$medianlla
  
  
  # expand dataframe to include absences as well
  ed = expandbyspecies(data1, species) %>% 
    # converting months to seasons
    mutate(month = as.numeric(month)) %>% 
    mutate(month = case_when(month %in% c(12,1,2) ~ "Win",
                             month %in% c(3,4,5) ~ "Sum",
                             month %in% c(6,7,8) ~ "Mon",
                             month %in% c(9,10,11) ~ "Aut")) %>% 
    mutate(month = as.factor(month))


  # the model ---------------------------------------------------------------
  
  if (flag == 0)
  {
    m1 = glmer(OBSERVATION.COUNT ~ month + month:log(no.sp) + timegroups + (1|gridg3/gridg1), 
               data = ed, family = binomial(link = 'cloglog'), 
               nAGQ = 0, control = glmerControl(optimizer = "bobyqa"))
  }
  
  if (flag == 1)
  {
    m1 = glmer(OBSERVATION.COUNT ~ month + month:log(no.sp) + timegroups + (1|gridg1), 
               data = ed, family = binomial(link = 'cloglog'), 
               nAGQ = 0, control = glmerControl(optimizer = "bobyqa"))
  }
  
  if (flag == 2)
  {
    m1 = glm(OBSERVATION.COUNT ~ month + month:log(no.sp) + timegroups, 
             data = ed, family = binomial(link = 'cloglog'))
  }
  

  # predicting from model ---------------------------------------------------

  # prepare a new data file to predict
  ltemp <- ed %>% 
    group_by(month) %>% 
    reframe(timegroups = unique(tm$timegroups)) %>% 
    mutate(no.sp = medianlla,
           # <annotation_pending_AV> why taking 1st value?
           gridg1 = data1$gridg1[1], 
           gridg3 = data1$gridg3[1])

  f2 <- ltemp %>% 
    dplyr::select(timegroups) %>% 
    # this is not actually needed
    mutate(freq = 0, se = 0)
  
  
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
  
  f1 = f2 %>%
    filter(!is.na(freqt) & !is.na(se)) %>%
    group_by(timegroups) %>% 
    reframe(freq = mean(freqt), se = mean(set)) %>% 
    right_join(tm) %>% 
    left_join(databins %>% distinct(timegroups, year)) %>% 
    rename(timegroupsf = timegroups,
           timegroups = year) %>% 
    mutate(timegroupsf = factor(timegroupsf, 
                               levels = c("before 2000","2000-2006","2007-2010",
                                          "2011-2012","2013","2014","2015","2016",
                                          "2017","2018","2019","2020","2021","2022"))) %>% 
    complete(timegroupsf) %>% 
    arrange(timegroupsf)
  
  
  tocomb = c(species, f1$freq, f1$se)
  return(tocomb)
  # each species's tocomb becomes one column in final trends0 output object
  
}
