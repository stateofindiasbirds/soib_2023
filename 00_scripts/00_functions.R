###   error operations ########################################

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


###   create a set of locations ########################################

# <annotation_pending_AV> why do we do this in the first place?

# locs is a data frame with location, group id info

createrandomlocs = function(locs)
{
  require(tidyverse)
  
  locs1 = locs %>% 
    group_by(LOCALITY.ID, month, timegroups) %>% sample_n(1)
  
  return(locs1$group.id)
}



### readcleanrawdata ########################################

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
  data = rbind(data, sesp) %>%
    # remove unapproved records and records of escapees
    filter(REVIEWED == 0 | APPROVED == 1) %>%
    filter(!EXOTIC.CODE %in% c("X"))
  
  # create and write a file with common names and scientific names of all Indian species
  # useful for mapping
  temp = data %>%
    filter(CATEGORY == "species" | CATEGORY == "issf") %>%
    distinct(COMMON.NAME,SCIENTIFIC.NAME)
  write.csv(temp,"00_data/indiaspecieslist.csv", row.names=FALSE)
  
  # create location file for LULC
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

### addmapvars ########################################

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
    distinct(NAME, GRID.G0, GRID.G1, GRID.G2, GRID.G3, GRID.G4, group.id, INLAND) %>% 
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


### completelistcheck ########################################

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

### removevagrants ########################################

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



### dataspeciesfilter ########################################

# <annotation_pending_AV> elaborate below
# select species for State of India's Birds, and species for historical and recent trends
# includes all diurnal endemics (endemicity) and essential species (SelectSpecies)

dataspeciesfilter = function(cur_mask = "none") {
  
  # ensuring only valid cur_mask names are provided
  if (!(cur_mask %in% unique(analyses_metadata$MASK))) {
    return('Invalid mask! Please provide valid mask name, one of: c("none","woodland","cropland","ONEland","PA").')
  }
  
  

  # preparing data for mask -------------------------------------------------

  cur_metadata <- analyses_metadata %>% filter(MASK == cur_mask)
  
  # thresholds for species to be considered in each analysis
  # - individual locations
  # - grid cells
  # - checklists
  # lowered slightly for states for inclusion; their Statuses will handle rest
  if (cur_metadata$MASK.TYPE == "state") {
    
    locationlimit = 10
    gridlimit = 4 
    listlimit = 30
    
  } else {
    
    locationlimit = 15 
    gridlimit = 4 
    listlimit = 50
    
  }
  
  
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
  
  # list of species recorded from state
  cur_mask_spec <- data %>% distinct(COMMON.NAME) %>% pull(COMMON.NAME)
  
  
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
  
  # return
  
  
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
  
  
  # return
  
  speciesresth = dataresth1 %>% 
    rename(species = COMMON.NAME) %>% 
    inner_join(data.frame(species = spec_resident)) %>% 
    mutate(validh = NA_real_)
  
  if (nrow(speciesresth) > 0) {
  
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
    
  }
  
  speciesrestr = datarestr1 %>% 
    rename(species = COMMON.NAME) %>% 
    inner_join(data.frame(species = spec_resident)) %>% 
    mutate(validr = NA_real_)
  
  if (nrow(speciesrestr) > 0) {
    
    for (i in 1:length(speciesrestr$species))
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
    # filter out species not recorded from current mask
    filter(COMMON.NAME %in% cur_mask_spec) %>% 
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

# for occupancy
# <annotation_pending_AV>

# ensure that the working directory has list of India's birds with scientific names 
# (just a safety mechanism for the function to work for small subsets, needs to be enabled if required)
# only need to input data, the species of interest and the complete list of India's bird species
# also groupspecs if required (a dataframe with all relevant list level info), it is defaulted to data

expandbyspecies = function(data, species)
{
  require(tidyverse)
  
  data <- data %>% 
    mutate(across(contains("gridg"), ~ as.factor(.))) %>% 
    mutate(timegroups = as.factor(timegroups))

  # considers only complete lists
  
  checklistinfo = data %>%
    distinct(gridg1, gridg2, gridg3, gridg4, 
             ALL.SPECIES.REPORTED, OBSERVER.ID, 
             #city,
             #DURATION.MINUTES,EFFORT.DISTANCE.KM,
             group.id, month, year, no.sp, timegroups, timegroups1) %>%
    filter(ALL.SPECIES.REPORTED == 1) %>%
    group_by(group.id) %>% 
    slice(1) %>% 
    ungroup()
  
  # expand data frame to include the bird species in every list
  expanded = checklistinfo %>% 
    mutate(COMMON.NAME = species) %>% 
    left_join(data) %>%
    dplyr::select(-c("COMMON.NAME","gridg2","gridg4","OBSERVER.ID",
                     "ALL.SPECIES.REPORTED","group.id","year","timegroups1",
                     "gridg0","DATETIME")) %>% 
  # deal with NAs (column is character)
  mutate(OBSERVATION.COUNT = case_when(is.na(OBSERVATION.COUNT) ~ 0,
                                       OBSERVATION.COUNT != "0" ~ 1, 
                                       TRUE ~ as.numeric(OBSERVATION.COUNT)))

  return(expanded)
}



### filter data based on migratory status ######################################

filt_data_for_mig <- function(data, species_var, status_var) {
  
  if (status_var == "R") {
    
    data_correct <- data
    
  } else {
    
    correct_months <- if (status_var == "MP") {
      c(9:11, 3:5)
    } else if (status_var == "MS") {
      c(5:8)
    } else if (status_var == "MW") {
      c(11:12, 1:2)
    }
    
    data_correct <- data %>%
      filter(COMMON.NAME == species_var) %>%
      {
        if (status_var != "M") {
          filter(., month %in% correct_months)
        } else {
          .
        }
      } %>% 
      distinct(month) %>% 
      left_join(data)
    
  }
  
  return(data_correct)
  
}

### run models ########################################

# trends
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


# occupancy
occupancyrun = function(data, i, speciesforocc, queen_neighbours)
{
  require(tidyverse)
  require(reshape2)
  require(data.table)
  require(unmarked)
  require(tictoc)
  require(glue)  

  species = speciesforocc$eBird.English.Name.2022[i]
  status = speciesforocc$status[i]

  tic(glue("Modelled occupancy of {species}"))
  
  # filter data only within the known spatial and temporal range of the species
  data = data %>%
    filter(COMMON.NAME == species) %>%
    distinct(gridg3, month) %>% 
    left_join(data)
  
  data_filt_mig <- data %>% 
    # filter (or not) the data based on migratory status
    filt_data_for_mig(species, status)
  
  # expanding data for absences also
  data_exp = expandbyspecies(data_filt_mig, species) %>% 
    filter(!is.na(gridg1)) %>%
    # converting months to seasons
    mutate(month = as.numeric(month)) %>%
    mutate(month = case_when(month %in% c(12, 1, 2) ~ "Win",
                             month %in% c(3, 4, 5) ~ "Sum",
                             month %in% c(6, 7, 8) ~ "Mon",
                             month %in% c(9, 10, 11) ~ "Aut")) %>% 
    mutate(month = as.factor(month))
  
  # reordering the checklists within each grid to minimise bias
  data_exp = data_exp[sample(x = 1:nrow(data_exp)),] 
  
  # number of checklists per grid cell
  lists_per_grid = data_exp %>%
    group_by(gridg1) %>% 
    reframe(lpg = n())
  
  # deciding a cutoff threshold based on 95% quantile---ignore all lists after that
  listcutoff = quantile(lists_per_grid$lpg, 0.95, na.rm = TRUE)
  
  data_exp = data_exp %>%
    arrange(gridg1) %>%
    group_by(gridg1) %>% 
    mutate(group.id = 1:n()) %>% 
    ungroup()
  
  # presences and absences
  occdata_full = data_exp %>%
    group_by(gridg1) %>% 
    # does the grid have the species?
    summarize(presence = sum(OBSERVATION.COUNT)) %>%
    mutate(presence = replace(presence, presence > 1, 1),
           # initialising this column which will later have info on 
           # proportion of neighbouring cells that have the species
           prop_nb = 0,
           gridg1 = as.character(gridg1))
  
  occdata_cell_nb <- occdata_full %>% 
    # numeric vector of neighbours of each cell being iterated over
    mutate(nb_list = map(gridg1, ~ as.numeric(queen_neighbours[[as.numeric(.)]]))) %>% 
    # this gives fewer neighbours than above, because not all neighbours are in data
    # (and even fewer with complete lists, etc.)
    mutate(occdata_nb = map(nb_list, ~ occdata_full %>% 
                              filter(gridg1 %in% .x) %>% 
                              pull(presence))) %>% 
    # numerator: total number of neighbour cells that have the species
    # denominator should be all neighbour cells
    mutate(prop_nb = map2_dbl(occdata_nb, nb_list, ~ sum(.x)/length(.y))) %>%
    dplyr::select(-nb_list, -occdata_nb)
  
  # absences
  occdata_abs = occdata_cell_nb %>% filter(presence != 1)
  occdata_cell_nb = occdata_cell_nb %>% dplyr::select(-presence)
  
  
  # creating matrices for occupancy
  # need data.table because very large objects
  setDT(data_exp)
  
  # matrices of detection/non-, season, and median list length (matrix needed for occ)
  det = dcast(data_exp, gridg1 ~ group.id, value.var = "OBSERVATION.COUNT")
  cov.month = dcast(data_exp, gridg1 ~ group.id, value.var = "month")
  cov.nosp = dcast(data_exp, gridg1 ~ group.id, value.var = "no.sp")
  
  # back to dataframe
  det = setDF(det)
  cov.month = setDF(cov.month)
  cov.nosp = setDF(cov.nosp)
  
  # for every grid cell, selecting only N lists; this removes outlier grid cells 
  # like Bangalore
  det = det[, 1:listcutoff]
  cov.month = cov.month[, 1:listcutoff]
  cov.nosp = cov.nosp[, 1:listcutoff]
  

  # calculate the number of checklists per grid going into the analysis
  samples.grid = det %>% 
    mutate(samples = rowSums(!is.na(dplyr::select(., -gridg1)))) %>%
    dplyr::select(gridg1,samples)
  n.samplespergrid = ncol(det) - 1
  

  
  # input to occupancy modelling
  occdata_UFO = unmarkedFrameOccu(
    y = det[, -1], # response (only 1s and 0s)
    siteCovs = data.frame(prop_nb = occdata_cell_nb$prop_nb),
    obsCovs = list(cov1 = cov.nosp[, -1],
                   cov2 = cov.month[, -1])
  )
  
  # create newdata
  cov.nosp.long = cov.nosp %>%
    pivot_longer(!gridg1, names_to = "sample", values_to = "cov1") %>%
    filter(!is.na(cov1))
  cov.month.long = cov.month %>%
    pivot_longer(!gridg1, names_to = "sample", values_to = "cov2") %>%
    filter(!is.na(cov2))
  newdata.det = cov.month.long %>%
    left_join(cov.nosp.long) %>% dplyr::select(-sample)
  
  newdata.occ = occdata_cell_nb
  
  
  # if not resident, we don't use seasonality as a covariate because data 
  # already filtered to those months
  if (status == "R") {
    
    occ_det = tryCatch({occu(~ log(cov1) * cov2 ~ prop_nb,
                             data = occdata_UFO,
                             starts = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
                             engine = "C")},
                       error = function(cond){"skip"})
    
  } else {
    
    occ_det = tryCatch({occu(~ log(cov1) ~ prop_nb, 
                             data = occdata_UFO, 
                             starts = c(0, 0, 0, 0),
                             engine = "C")},
                       error = function(cond){"skip"})
    
  }
  
  # output of above will be character when errored ("skip")
  if (!is.character(occ_det)) {
    
    # detection probability
    occpred_detection = unmarked::predict(occ_det, 
                                          newdata = newdata.det, type = "det")
    occpred_detection = occpred_detection %>% dplyr::select(-lower, -upper) %>%
      bind_cols(newdata.det) %>%
      filter(!is.na(Predicted)) %>% mutate(pred.inv = 1-Predicted) %>%
      group_by(gridg1) %>% mutate(mean.prod = prod(pred.inv)) %>%
      mutate(se.ratio.inv = SE/pred.inv) %>% ungroup() %>%
      group_by(gridg1) %>% reframe(det = 1-max(mean.prod),
                                   inv.det = max(mean.prod),
                                   det.se = max(mean.prod)*sum(se.ratio.inv))
    

    # occupancy
    occpred_occupancy = unmarked::predict(occ_det, 
                                          newdata = newdata.occ, type = "state")
    occpred_occupancy = occpred_occupancy %>% dplyr::select(-lower, -upper) %>%
      bind_cols(newdata.occ) %>%
      rename(occ = Predicted, occ.se = SE) %>%
      dplyr::select(gridg1, prop_nb, occ, occ.se) %>%
      left_join(occpred_detection) %>%
      left_join(samples.grid) %>%
      left_join(occdata_abs) %>% 
      mutate(presence = case_when(is.na(presence)~1,TRUE~presence)) %>%
      mutate(COMMON.NAME = species, status = status,
             occupancy = occ*inv.det,
             se = occ*inv.det*((det.se/inv.det) + (occ.se/occ)))
      
  }

  toc()
  
  # to combine
  tocomb = occpred_occupancy
  return(tocomb)
  
}

### sensitivity check for long-term trend ------------------------------------------

# simulations

ltt_sens_sim <- function(my_seed, data = modtrends) {
  
  set.seed(my_seed) # for simulations
  
  data_sim = data %>% 
    # calculating CIs
    group_by(COMMON.NAME, timegroups) %>% 
    # 1000 simulations of transformed ratio of present:original values
    # quantiles*100 from these gives us our CI limits for mean_std
    reframe(tp0 = simerrordiv(mean_trans, m1, se_trans, s1)$rat) %>% 
    group_by(COMMON.NAME, timegroups) %>% 
    reframe(lci_std = 100*as.numeric(quantile(tp0, 0.025)),
            rci_std = 100*as.numeric(quantile(tp0, 0.975))) %>% 
    right_join(modtrends, by = c("COMMON.NAME", "timegroups")) %>%
    filter(timegroups == 2022) %>%
    dplyr::select(COMMON.NAME, lci_std, mean_std, rci_std) %>%
    rename(longtermlci = lci_std,
           longtermmean = mean_std,
           longtermrci = rci_std)
  
  return(data_sim)
  
}

# classification step

ltt_sens_class <- function(data) {
  
  data = data %>%
    mutate(
      
      SOIBv2.Long.Term.Status = case_when(
        is.na(longtermmean) ~ "eBird Data Deficient",
        (longtermrci-longtermmean)/longtermmean > 0.5 ~ "eBird Data Inconclusive", # arbitrary
        # else
        # for declines
        longtermrci <= 50 ~ "Rapid Decline", # -100% to -50%
        longtermrci > 50 & longtermrci <= 75 ~ "Decline", # -50% to -25%
        # for increases
        longtermlci >= 150 ~ "Rapid Increase", # +50% to inf
        longtermlci < 150 & longtermlci >= 125 ~ "Increase", # +25% to +50%
        # stable vs inconclusive:
        # if CI is completely below or above the baseline, can't be stable
        longtermlci > 100 | longtermrci < 100 ~ "eBird Data Inconclusive",
        # if one limit is in the Stable zone but other limit passes to Rapid X, can't be stable
        longtermlci <= 50 | longtermrci >= 150 ~ "eBird Data Inconclusive",
        TRUE ~ "Stable"
      )
      
    ) %>% 
    dplyr::select(COMMON.NAME, SOIBv2.Long.Term.Status)
  
  return(data)
  
}
