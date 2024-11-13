# get analyses metadata -------------------------------------------------

get_metadata <- function(mask = NULL) {

  require(tidyverse)
  
  load("00_data/analyses_metadata.RData")

  if(is.null(mask)) {
    return(analyses_metadata)
  } else {

    if (!mask %in% analyses_metadata$MASK) {
      stop("Select valid mask name")
    } else {
      return(analyses_metadata |> filter(MASK == mask))
    }

  }

}

###   error operations ########################################

# function to propagate standard errors while dividing or multiplying values
# arguments are the two means and their SEs
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

# function to take in two means and SEs, simulate 1000 values from each and then get 1000 ratios
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

# function to select a random GROUP.ID from each location in the dataset 
# (spatial subsampling) so that each set of IDs can form the basis for a dataset to use for analysis
# this is then repeated 1000 times in the script "create_random_groupids.R" 
# to create 1000 datasets, each without pseudoreplication

# locs is a data frame with location, group id info

createrandomlocs = function(locs)
{
  require(tidyverse)
  
  locs1 = locs %>% 
    group_by(LOCALITY.ID, month, timegroups) %>% sample_n(1)
  
  return(locs1$group.id)
}


# what are the latest migratory years under consideration? -----------------

soib_year_info <- function(what = "latest_year") {

  # catch input errors
  valid_inputs <- c("latest_year", "timegroup_lab", "timegroup_med", 
                    "cat_years", "cat_start", "iucn_projection")

  if (!what %in% valid_inputs) {
    stop(paste("Choose valid info to obtain regarding current SoIB years: {", 
                stringr::str_flatten_comma(valid_inputs),
              "}"))
  }


  # load latest year data
  load("00_data/current_soib_migyears.RData")


  # latest year
  if (what == "latest_year") {
    return(latest_soib_my)
  }

    
  # timegroup labels or median years
  # (2013 as threshold here is somewhat arbitrary, not sure which years in full_soib_my)
  if (what %in% c("timegroup_lab", "timegroup_med")) {
    
    if (what == "timegroup_lab") {
      pre <- c("before 2000","2000-2006","2007-2010","2011-2012","2013")
      full <- full_soib_my[full_soib_my > 2013] |> as.character()
    } else if (what == "timegroup_med") {
      pre <- c(median_soib_hist_years, 2013)
      full <- full_soib_my[full_soib_my > 2013]
    }

    all <- c(pre, full)
    return(all)
  }


  # cutoff year for CAT
  # 2015 was cutoff in SoIB 2023. So 8 years for CAT.
  # We eventually want 10 years for CAT
  cat_years <- full_soib_my[full_soib_my >= 2015] |> 
    sort() |> 
    tail(10)

  if (what == "cat_years") return(cat_years)
    else if (what == "cat_start") return(min(cat_years))
  
  
  # extra years for IUCN projection
  if (what == "iucn_projection") {
    extra.years = seq(latest_soib_my, length.out = 7) + 1
    return(extra.years)
  }

}


# get column names for IUCN projection values -----------------------------

get_iucn_proj_cols <- function() {
  map(soib_year_info("iucn_projection"), ~ {
    c(glue("proj{.x}.lci"), glue("proj{.x}.mean"), glue("proj{.x}.rci"))
  }) %>% 
    list_c()
}


### readcleanrawdata ########################################

## read and clean raw data and add important columns like group id, seasonality variables
## place raw txt file (India download) in working directory 

readcleanrawdata = function(rawpath = "00_data/ebd_IN_relJun-2024.txt", 
                            sensitivepath = "00_data/ebd_sensitive_relJun-2024_IN.txt")
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
  

  # read sensitive species data

  sesp = read.delim(sensitivepath, colClasses = nms1, sep = "\t", header = T, quote = "", 
                    stringsAsFactors = F, na.strings = c(""," ",NA))

  
  # merge both data frames
  data = rbind(data, sesp) %>%
    # remove unapproved records and records of escapees
    filter(REVIEWED == 0 | APPROVED == 1) %>%
    filter(!EXOTIC.CODE %in% c("X"))
  

  ## choosing important columns required for further analyses
  
  imp = c("CATEGORY","COMMON.NAME","SCIENTIFIC.NAME","OBSERVATION.COUNT",
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
  
  
  # create and write a file with common names and scientific names of all Indian species
  # useful for mapping
  temp = data %>%
    filter(CATEGORY == "species" | CATEGORY == "issf") %>%
    distinct(COMMON.NAME,SCIENTIFIC.NAME)
  write.csv(temp,"00_data/indiaspecieslist.csv", row.names=FALSE)
  
  # create location file for LULC
  locdat = data %>% distinct(LOCALITY.ID, LATITUDE, LONGITUDE)
  write.csv(locdat,"00_data/eBird_location_data.csv", row.names=FALSE)

  
  # need to combine several closely related species and slashes/spuhs
  # so, first changing their category to species since they will be combined next
  data = data %>%
    mutate(SCIENTIFIC.NAME = NULL, # needed it for printing indiaspecieslists
           CATEGORY = case_when(COMMON.NAME %in% c(
      "Green/Greenish Warbler", "Siberian/Amur Stonechat", "Red-necked/Little Stint",
      "Western/Eastern Yellow Wagtail", "Common/Himalayan Buzzard",
      "Western/Eastern Marsh Harrier", "Tibetan/Greater Sand-Plover", "Baikal/Spotted Bush Warbler",
      "Lemon-rumped/Sichuan Leaf Warbler", "Red-rumped/Striated Swallow",
      "Bank Swallow/Pale Martin", "Riparia sp.", "Greater/Mongolian Short-toed Lark",
      "Taiga/Red-breasted Flycatcher", "Tricolored x Chestnut Munia (hybrid)", "Little/House Swift", 
      "Pin-tailed/Swinhoe's Snipe", "Booted/Sykes's Warbler", "Iduna sp.", "Greater/Malabar Flameback"
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
      COMMON.NAME %in% c("Eastern Marsh Harrier", 
                         "Western/Eastern Marsh Harrier") ~ "Western Marsh Harrier",
      COMMON.NAME %in% c("Greater Sand-Plover", 
                         "Tibetan/Greater Sand-Plover") ~ "Tibetan Sand-Plover",
      COMMON.NAME %in% c("Baikal Bush Warbler", 
                         "Baikal/Spotted Bush Warbler") ~ "Spotted Bush Warbler",
      COMMON.NAME %in% c("Sichuan Leaf Warbler", 
                         "Lemon-rumped/Sichuan Leaf Warbler") ~ "Lemon-rumped Warbler",
      COMMON.NAME %in% c("Striated Swallow", 
                         "Red-rumped/Striated Swallow") ~ "Red-rumped Swallow",
      COMMON.NAME %in% c("Pale Martin", "Bank Swallow/Pale Martin", 
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
      COMMON.NAME %in% c("Malabar Flameback", 
                         "Greater/Malabar Flameback") ~ "Greater Flameback",
      TRUE ~ COMMON.NAME
    ))
  
  
  ## setup eBird data ##
  

  # for automatically selecting the latest migratory year to use in 
  # current SoIB run (done annually)
  full_soib_my <- data |> 
    distinct(year, month) |> 
    group_by(year) |> 
    reframe(n_month = n_distinct(month)) |> 
    filter(n_month == 12) |> 
    pull(year)

  latest_soib_my <- max(full_soib_my)
  
  # median years for each historical timegroup
  median_soib_hist_years <- data %>% 
    distinct(group.id, .keep_all = TRUE) %>% 
    filter(ALL.SPECIES.REPORTED == 1) %>%
    mutate(hist_period = case_when(year <= 1999 ~ 1,
                                  year > 1999 & year <= 2006 ~ 2,
                                  year > 2006 & year <= 2010 ~ 3,
                                  year > 2010 & year <= 2012 ~ 4)) %>% 
    group_by(hist_period) %>% 
    reframe(median_year = round(median(year))) %>% 
    arrange(hist_period) %>% 
    pull(median_year)

  save(full_soib_my, latest_soib_my, median_soib_hist_years, 
       file = "00_data/current_soib_migyears.RData")
  # this RData file gets updated each time readcleanrawdata() is run---
  # which is usually only once in each annual or "major" update
  # (intermediate changes usually don't run readcleanrawdata() so year won't change)


  ## remove repeats by retaining only a single group.id + species combination
  
  data = data %>%
    distinct(group.id, COMMON.NAME, .keep_all = TRUE) |> 
    filter(year <= latest_soib_my) %>% 
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
    distinct(group.id, .keep_all = TRUE) |> 
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
    distinct(group.id, .keep_all = TRUE) |> 
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

completelistcheck = function(data) {

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
  
  grp = data %>%
    # only need checklist metadata
    distinct(group.id, .keep_all = TRUE) |> 
    # list of on-paper complete lists
    filter(ALL.SPECIES.REPORTED == 1, PROTOCOL.TYPE != "Incidental") %>%
    # choose checklists without info on duration with 3 or fewer species
    filter(no.sp <= 3, is.na(DURATION.MINUTES)) %>%
    pull(group.id)
  
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
    dplyr::select(-speed,-sut,-hr,-min,-end,-DATETIME)

  return(data)

}

### removevagrants ########################################

## remove vagrants
## to use in dataspeciesfilter()

removevagrants = function(data)
{
  # mapping of SoIB-species-of-interest to a range of variables/classifications
  # (manually created)
  fullmap = read.csv("00_data/SoIB_mapping_2023.csv")
  
  migspecies = fullmap %>%
    filter(!Migratory.Status.Within.India %in% c("Resident",
                                                 "Resident & Altitudinal Migrant",
                                                 "Resident & Local Migrant",
                                                 "Resident & Localized Summer Migrant",
                                                 "Altitudinal Migrant",
                                                 "Resident (Extirpated)")) %>%
    pull(eBird.English.Name.2023)
  
  d = data %>%
    filter(COMMON.NAME %in% migspecies) %>%
    group_by(gridg4, month, COMMON.NAME) %>%
    reframe(nyear = n_distinct(year)) %>%
    filter(nyear <= 3) %>% 
    dplyr::select(gridg4, month, COMMON.NAME)
  
  d = left_join(d, data) %>%
    filter(year >= soib_year_info("cat_start"))
  
  save(d, file = "00_data/vagrantdata.RData")
  
  data = anti_join(data, d)
  return(data)
}



### dataspeciesfilter ########################################

# select species for State of India's Birds, and species for historical and recent trends
# the hierarchical logic is as follows - first any species that meet 
# the data criteria for LTC and CAT are selected
# followed by all diurnal endemics (endemicity) and essential species (SelectSpecies)
# followed by assigning some species from within the previous set for 'restricted' trend analyses

dataspeciesfilter = function(cur_mask = "none", singleyear = interannual_update) {
  
  # ensuring only valid cur_mask names are provided
  if (!(cur_mask %in% unique(get_metadata()$MASK))) {
    return('Invalid mask! Please provide valid mask name, one of: c("none","woodland","cropland","ONEland","PA").')
  }
  
  # metadata
  cur_metadata <- get_metadata(cur_mask)
  

  # processing data filter (updates every time) -------------------------------

  # most of the data filtering happens in filter_data_for_species.R before
  # this function is called, so here only filtering for respective mask
  # and removing some unnecessary columns
  
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
  
  
  # number of sampled grid cell at each resolution
  sampledcells = c(length(unique(data0$gridg0)),
                   length(unique(data0$gridg1)),
                   length(unique(data0$gridg2)),
                   length(unique(data0$gridg3)),
                   length(unique(data0$gridg4)))

  
  data = data0 %>% 
    dplyr::select(-CATEGORY,-REVIEWED,-APPROVED,-ST_NM,-DISTRICT,
                  -LOCALITY.TYPE,-LOCALITY.ID,-pa.name,-maskWdl,-maskCrp,-maskOne,
                  -LATITUDE,-LONGITUDE,-PROTOCOL.TYPE,-EXOTIC.CODE,-day,-cyear,
                  -DURATION.MINUTES,-TIME.OBSERVATIONS.STARTED,-EFFORT.DISTANCE.KM)
  
  stats7 = paste(nrow(data[data$ALL.SPECIES.REPORTED == 1,]),
                 "filter 1 usable observations")
  stats8 = paste(length(unique(data[data$ALL.SPECIES.REPORTED == 1,]$group.id)),
                 "filter 2 unique complete checklists")
  stats9 = paste(length(unique(data[data$timegroups == soib_year_info("timegroup_lab")[1] &
                                      data$ALL.SPECIES.REPORTED == 1,]$group.id)),
                 "pre-2000 checklists")
  
  # summary for each timegroup
  databins = data %>%
    filter(ALL.SPECIES.REPORTED == 1) %>%
    group_by(timegroups) %>% 
    reframe(lists = n_distinct(group.id), 
            year = round(median(year))) %>%
    # for states like TR and NL, no data at all in some historical
    # timegroups, so complete with NAs (cos timegroups column used in run_species_trends)
    complete(timegroups = soib_year_info("timegroup_lab")) %>% 
    mutate(timegroups = factor(timegroups, levels = soib_year_info("timegroup_lab"))) %>% 
    arrange(timegroups, year)
  
  
  # writing filtered data files ---------------------------------------------
  
  # saving a full list of location IDs to be used if required
  locs_write = data0 %>% 
    filter(ALL.SPECIES.REPORTED == 1) %>%
    distinct(LOCALITY.ID, group.id, month, timegroups)
  
  write.csv(locs_write, row.names = F, 
            file = cur_metadata$LOCS.PATH)
  
  
  # saving the full filtered data
  save(data, sampledcells, databins, 
       file = cur_metadata$DATA.PATH)
  
  
  # processing species filter (updates for major SoIB versions) ----------------
  
  # for annual updates, we use the same set of species in analyses as last major version
  # so only thing to be changed in this set of species filters
  # is to update the species names with latest taxonomy
  

  if (singleyear == TRUE) {
    
    # update taxonomy

    load(cur_metadata$SPECLISTDATA.PATH)
    specieslist <- update_species_lists(specieslist, scientific_also = FALSE)
    restrictedspecieslist <- update_species_lists(restrictedspecieslist, scientific_also = FALSE)
    
    dataf <- read.csv(file = cur_metadata$FULLSPECLIST.PATH) %>% 
      update_species_lists(scientific_also = TRUE)
    
    
    # if annual update, don't include stats10-12 (species filters)
    stats = c(stats1, stats2, stats3, stats4, stats5, stats6,
              stats7, stats8, stats9)
    
  } else if (singleyear == FALSE) {
    
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
      filter(years == length(soib_year_info("timegroup_lab"))) %>%
      mutate(ht = 1) %>% 
      dplyr::select(COMMON.NAME, ht)
    
    # recent data (data from 2015 onwards), used for recent trends
    # gives list of species for which we have enough data and this analysis can be done
    datar = data0 %>%
      filter(ALL.SPECIES.REPORTED == 1, 
             CATEGORY %in% c("species", "issf"), 
             year >= soib_year_info("cat_start")) %>%
      group_by(COMMON.NAME, year) %>%
      reframe(locs = n_distinct(LOCALITY.ID), 
              cells = n_distinct(gridg4)) %>%
      group_by(COMMON.NAME, year) %>%
      filter(locs > locationlimit, cells > gridlimit) %>%
      group_by(COMMON.NAME) %>% 
      reframe(years = n()) %>%
      group_by(COMMON.NAME) %>%
      filter(years == length(soib_year_info("cat_years"))) %>%
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
      filter(years == length(soib_year_info("timegroup_lab"))) %>%
      dplyr::select(COMMON.NAME)
    
    datarestr1 = data0 %>%
      filter(ALL.SPECIES.REPORTED == 1, 
             CATEGORY %in% c("species", "issf"), 
             year >= soib_year_info("cat_start")) %>%
      group_by(COMMON.NAME, timegroups) %>%
      reframe(cells = n_distinct(gridg4)) %>%
      group_by(COMMON.NAME, timegroups) %>%
      filter(cells <= gridlimit) %>%
      group_by(COMMON.NAME) %>% 
      reframe(years = n()) %>%
      group_by(COMMON.NAME) %>%
      filter(years == length(soib_year_info("cat_years"))) %>%
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
        
        if (length(tempresth1$timegroups) == length(soib_year_info("timegroup_lab")))
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
          filter(year >= soib_year_info("cat_start")) %>%
          group_by(timegroups) %>% 
          reframe(n = n_distinct(group.id)) %>%
          group_by(timegroups) %>%
          filter(n > listlimit)
        
        if (length(temprestr1$timegroups) == length(soib_year_info("cat_years")))
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
    
    
    # we need country specieslist to derive specieslist for states
    if (cur_metadata$MASK.TYPE == "state") {
      load(file = get_metadata("none") %>% pull(SPECLISTDATA.PATH))
      
      specieslist_nat <- specieslist
      rm(specieslist, restrictedspecieslist)
    }
    
    # to limit number of species in restricted species list for states
    specieslist_rest = dataf %>%
      filter(., 
             (Essential == 1 | Endemic.Region != "Non-endemic" | ht == 1 | rt == 1), 
             (Breeding.Activity.Period != "Nocturnal" | 
                Non.Breeding.Activity.Period != "Nocturnal" | 
                COMMON.NAME == "Jerdon's Courser"),
             (is.na(Discard))
      ) %>%
      # filter out species not recorded from current mask
      filter(COMMON.NAME %in% cur_mask_spec) %>% 
      dplyr::select(COMMON.NAME, ht, rt)
    
    specieslist = dataf %>%
      # for states, we want to include any species reported from the state & with trend for country
      {if (cur_metadata$MASK.TYPE != "state") {
        filter(., 
               (Essential == 1 | Endemic.Region != "Non-endemic" | ht == 1 | rt == 1), 
               (Breeding.Activity.Period != "Nocturnal" | 
                  Non.Breeding.Activity.Period != "Nocturnal" | 
                  COMMON.NAME == "Jerdon's Courser"),
               (is.na(Discard))
        )
      } else {
        filter(.,
               (COMMON.NAME %in% specieslist_nat$COMMON.NAME | ht == 1 | rt == 1), 
               (Breeding.Activity.Period != "Nocturnal" | 
                  Non.Breeding.Activity.Period != "Nocturnal" | 
                  COMMON.NAME == "Jerdon's Courser"), 
               (is.na(Discard))
        )
      }} %>%
      # filter out species not recorded from current mask
      filter(COMMON.NAME %in% cur_mask_spec) %>% 
      dplyr::select(COMMON.NAME, ht, rt)
    
    
    # although specieslist is already extracted previously, ht and rt in dataf need to be
    # synced so that later calculations of stats (number of species that met certain criteria)
    # can be done using this data frame
    dataf <- dataf %>% 
      mutate(ht = case_when(Breeding.Activity.Period == "Nocturnal" &
                              Non.Breeding.Activity.Period == "Nocturnal" ~ NA_real_,
                            TRUE ~ ht),
             rt = case_when(Breeding.Activity.Period == "Nocturnal" &
                              Non.Breeding.Activity.Period == "Nocturnal" ~ NA_real_,
                            TRUE ~ rt))
    
    
    # t1, t2, and t3 are dataframes that are filtered to give information on
    # 1) how many species qualified for SoIB through data availability for trends
    # 2) how many species qualified through endemicity
    # 3) how many qualified because they were noted to be essential for the Indian context
    t1 = dataf %>%
      filter((ht == 1 | rt == 1) &
               (Breeding.Activity.Period != "Nocturnal" |
                  Non.Breeding.Activity.Period != "Nocturnal"))
    t2 = dataf %>%
      filter((Endemic.Region != "Non-endemic" | ht == 1 | rt == 1) & 
               (Breeding.Activity.Period != "Nocturnal" |
                  Non.Breeding.Activity.Period != "Nocturnal"))
    t3 = dataf %>%
      filter((Essential == 1 | Endemic.Region != "Non-endemic" | ht == 1 | rt == 1) &
               (Breeding.Activity.Period != "Nocturnal" |
                  Non.Breeding.Activity.Period != "Nocturnal"))
    
    stats10 = paste(length(t1$COMMON.NAME),"filter 1 number of species")
    stats11 = paste(length(t2$COMMON.NAME),"filter 2 number of species")
    stats12 = paste(length(t3$COMMON.NAME),"filter 3 number of species")
    
    
    stats = c(stats1, stats2, stats3, stats4, stats5, stats6,
              stats7, stats8, stats9, stats10, stats11, stats12)
    
    
    # ignoring species that are frequently misIDd
    specieslist <- specieslist %>% 
      mutate(ht = case_when(COMMON.NAME %in% spec_misid ~ NA_real_,
                            TRUE ~ ht),
             rt = case_when(COMMON.NAME %in% spec_misid ~ NA_real_,
                            TRUE ~ rt))
            
            
    # creation of restrictedspecieslist that only includes species that did not qualify 
    # for trend analysis through the primary logic (see specieslist assignment)
    restrictedspecieslist = {if (cur_metadata$MASK.TYPE != "state") {
      data.frame(species = specieslist$COMMON.NAME)
    } else {
      data.frame(species = specieslist_rest$COMMON.NAME)
    }} %>% 
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


    # check1 and check2 are lists of  species that qualified for restricted analyses for
    # long-term and current analyses
    # these vectors are used to update the columns in dataf that mention whether a species
    # has qualified for either of these analyses
    check1 = restrictedspecieslist %>% 
      filter(!is.na(ht)) %>% 
      pull(COMMON.NAME)
    check2 = restrictedspecieslist %>% 
      filter(!is.na(rt)) %>% 
        pull(COMMON.NAME)


    # randomcheck a and b are to determine whether the models for these restricted species 
    # will include random effects or not - "randomcheck_a" has those species that qualified
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
      
      
    # information from the randomchecks is stored on the restrictedspecieslist data frame
    restrictedspecieslist_a = restrictedspecieslist %>% 
      filter(COMMON.NAME %in% randomcheck_a$COMMON.NAME) %>% 
      mutate(mixed = 1)
    restrictedspecieslist_b = restrictedspecieslist %>% 
      filter(COMMON.NAME %in% randomcheck_b$COMMON.NAME) %>% 
      mutate(mixed = 0)
      
    restrictedspecieslist = rbind(restrictedspecieslist_a,restrictedspecieslist_b)

    
    # creates the column "selected" that is used to provide information about which species
    # have been selected for SoIB when the data frame is written on to the larger data frame
    # with the full species list
    specieslist1 = specieslist %>% 
      mutate(selected = 1) %>% 
      dplyr::select(COMMON.NAME, selected)
    
    dataf = dataf %>%
      dplyr::select(COMMON.NAME, SCIENTIFIC.NAME, ht, rt) %>% 
      left_join(specieslist1) %>% 
      magrittr::set_colnames(c("COMMON.NAME","SCIENTIFIC.NAME",
                               "Long.Term.Analysis","Current.Analysis",
                               "Selected.SoIB")) %>%  
      # converting to report table-style with blanks for NAs and Xs for 1s
      mutate(across(everything(), ~ as.character(.))) %>% 
      mutate(across(everything(), ~ replace_na(., replace = ""))) %>% 
      mutate(across(everything(), ~ str_replace(., pattern = "1", replacement = "X"))) %>% 
      # also including species in checks
      mutate(Long.Term.Analysis = if_else(COMMON.NAME %in% check1, "X", Long.Term.Analysis),
             Current.Analysis = if_else(COMMON.NAME %in% check2, "X", Current.Analysis)) %>%
      dplyr::select("COMMON.NAME","SCIENTIFIC.NAME","Long.Term.Analysis","Current.Analysis",
                    "Selected.SoIB")
    
    
    # filtering for only species in certain masks ###
    if (cur_mask == "woodland") {
      
      dataf <- dataf %>% 
        mutate(Long.Term.Analysis = if_else(!(COMMON.NAME %in% spec_woodland), "", Long.Term.Analysis),
               Current.Analysis = if_else(!(COMMON.NAME %in% spec_woodland), "", Current.Analysis),
               Selected.SoIB = if_else(!(COMMON.NAME %in% spec_woodland), "", Selected.SoIB))
      
    } else if (cur_mask %in% c("cropland", "ONEland")) {
      
      dataf <- dataf %>% 
        mutate(Long.Term.Analysis = if_else(!(COMMON.NAME %in% spec_openland), "", Long.Term.Analysis),
               Current.Analysis = if_else(!(COMMON.NAME %in% spec_openland), "", Current.Analysis),
               Selected.SoIB = if_else(!(COMMON.NAME %in% spec_openland), "", Selected.SoIB))
      
    }
    
    
    
    # additional filtering safeguards - proportion of range sampled during every timegroup
    
    temp = data %>%
      filter(COMMON.NAME %in% dataf$COMMON.NAME, 
             ALL.SPECIES.REPORTED == 1)
    
    totalrange = temp %>%
      group_by(COMMON.NAME) %>% 
      reframe(totalrange25km = n_distinct(gridg1))
    
    proprange2000 = temp %>%
      filter(timegroups == soib_year_info("timegroup_lab")[1]) %>%
      group_by(COMMON.NAME) %>% 
      reframe(proprange25km2000 = n_distinct(gridg1))
    
    proprange.latestyear = temp %>%
      filter(timegroups == as.character(soib_year_info("latest_year"))) %>%
      group_by(COMMON.NAME) %>% 
      reframe(proprange25km.latestyear = n_distinct(gridg1))
    
    proprange.current = temp %>%
      filter(timegroups %in% as.character(soib_year_info("cat_years"))) %>%
      group_by(COMMON.NAME, timegroups) %>% 
      reframe(proprange25km.current = n_distinct(gridg1)) %>%
      group_by(COMMON.NAME) %>% 
      reframe(proprange25km.current = mean(proprange25km.current))
    
    range25km = totalrange %>% 
      left_join(proprange2000) %>% 
      left_join(proprange.current) %>% 
      left_join(proprange.latestyear) %>%
      mutate(proprange25km2000 = proprange25km2000/totalrange25km,
             proprange25km.current = proprange25km.current/totalrange25km,
             proprange25km.latestyear = proprange25km.latestyear/totalrange25km)
    
    
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
    
    
  }
  
  
  # writing filtered species list files ---------------------------------------------
  
  # saving a full species list of the country with all associated metadata including
  # whether they've been included for SoIB and whether they've qualified for any analyses
  write.csv(dataf, row.names = FALSE, 
            file = cur_metadata$FULLSPECLIST.PATH)
  
  
  # saving a data frame called "specieslist" that has both specieslist as well as restrictedspecieslist
  # these is an essential dataframe that is called whenever information is required
  # about which analysis to perfrom for a species
  save(specieslist, restrictedspecieslist, stats, 
       file = cur_metadata$SPECLISTDATA.PATH)
  
}



### expandbyspecies ########################################

# this function adds absences (in addition to presences) to subsets of the data that
# need to be analyzed - for the analysis of a certain species, every complete list 
# without the species is expanded to have 0s added against that species name

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
             group.id, month, year, no.sp, timegroups) %>%
    filter(ALL.SPECIES.REPORTED == 1) %>%
    distinct(group.id, .keep_all = TRUE)
  
  # expand data frame to include the bird species in every list
  expanded = checklistinfo %>% 
    mutate(COMMON.NAME = species) %>% 
    left_join(data) %>%
    dplyr::select(-c("COMMON.NAME","gridg2","gridg4","OBSERVER.ID",
                     "ALL.SPECIES.REPORTED","group.id","year","gridg0")) %>% 
  # deal with NAs (column is character)
  mutate(OBSERVATION.COUNT = case_when(is.na(OBSERVATION.COUNT) ~ 0,
                                       OBSERVATION.COUNT != "0" ~ 1, 
                                       TRUE ~ as.numeric(OBSERVATION.COUNT)))

  return(expanded)
}

# faster version of this function using data.table and dtplyr
# optimising runtime
# previous expandbyspecies() to be retired entirely in next annual update
expand_dt = function(data, species, singleyear = FALSE) {

  require(tidyverse)
  require(dtplyr)
  require(data.table)
  

  setDT(data)
  
  
    data <- data %>% 
      lazy_dt(immutable = FALSE) |> 
      mutate(across(contains("gridg"), ~ as.factor(.))) %>% 
      {if (singleyear == FALSE) {
        mutate(., timegroups = as.factor(timegroups))
      } else if (singleyear == TRUE) {
        .
      }} |> 
      as.data.table()


  # Get distinct rows and filter based on a condition
  # (using base data.table because lazy_dt with immutable == FALSE would
  # modify data even though we are assigning to checklistinfo.
  # and immutable == TRUE copies the data and this is a huge bottleneck)
  # considers only complete lists
  
  if (singleyear == FALSE) {

    checklistinfo <- unique(data[, 
                                 .(gridg1, gridg2, gridg3, gridg4, ALL.SPECIES.REPORTED, OBSERVER.ID, 
                                   group.id, month, year, no.sp, timegroups)
    ])[
      # filter
      ALL.SPECIES.REPORTED == 1
    ]

  } else if (singleyear == TRUE) {

    checklistinfo <- unique(data[, 
                                 .(gridg1, gridg2, gridg3, gridg4, ALL.SPECIES.REPORTED, OBSERVER.ID, 
                                   group.id, month, year, no.sp)
    ])[
      # filter
      ALL.SPECIES.REPORTED == 1
    ]

  }
  

  
  checklistinfo <- checklistinfo[
    , 
    .SD[1], # subset of data
    by = group.id
]
  
    
  # expand data frame to include the bird species in every list
  
  join_by_temp <- if (singleyear == FALSE) {
    c("group.id", "gridg1", "gridg2", "gridg3", "gridg4",
      "ALL.SPECIES.REPORTED", "OBSERVER.ID", "month", "year", 
      "no.sp", "timegroups", "COMMON.NAME")
  } else if (singleyear == TRUE) {
    c("group.id", "gridg1", "gridg2", "gridg3", "gridg4",
      "ALL.SPECIES.REPORTED", "OBSERVER.ID", "month", "year", 
      "no.sp","COMMON.NAME")
  }

    data2 = checklistinfo %>% 
      lazy_dt(immutable = FALSE) |> 
      mutate(COMMON.NAME = species) %>% 
      left_join(data |> lazy_dt(immutable = FALSE),
                by = join_by_temp) %>%
      dplyr::select(-c("COMMON.NAME","gridg2","gridg4","OBSERVER.ID",
                       "ALL.SPECIES.REPORTED","group.id","year","gridg0")) %>% 
      # deal with NAs (column is character)
      mutate(OBSERVATION.COUNT = case_when(is.na(OBSERVATION.COUNT) ~ 0,
                                           OBSERVATION.COUNT != "0" ~ 1, 
                                           TRUE ~ as.numeric(OBSERVATION.COUNT))) |> 
      as_tibble()
  
  rm(join_by_temp)
  
  return(data2)

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
singlespeciesrun = function(data, species, specieslist, restrictedspecieslist, 
                            singleyear = FALSE)
{
  require(tidyverse)
  require(lme4)
  require(merTools)
  require(glue)
  
  data1 = data
  
  # get information for the species of interest 
  specieslist2 = specieslist %>% filter(COMMON.NAME == species)
  
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
  
  if (singleyear == FALSE) {

    if (is.na(specieslist2$ht) & !is.na(specieslist2$rt)) {
      data1 = data1 %>% filter(year >= soib_year_info("cat_start"))
    }
  
  } else if (singleyear == TRUE) {

    data1 = data1 %>% filter(year == soib_year_info("latest_year"))
  }

  
  data1 = data1 %>%
    filter(COMMON.NAME == species) %>%
    distinct(gridg3, month) %>% 
    left_join(data1)
  
  tm = data1 %>% distinct(timegroups)
  #rm(data, pos = ".GlobalEnv")
  
  datay = data1 %>%
    distinct(gridg3, gridg1, group.id, .keep_all = TRUE) %>% 
    group_by(gridg3, gridg1) %>% 
    reframe(medianlla = median(no.sp)) %>%
    group_by(gridg3) %>% 
    reframe(medianlla = mean(medianlla)) %>%
    reframe(medianlla = round(mean(medianlla)))
  
  medianlla = datay$medianlla
  
  
  # expand dataframe to include absences as well
  ed = expand_dt(data1, species) %>% 
    # converting months to seasons
    mutate(month = as.numeric(month)) %>% 
    mutate(month = case_when(month %in% c(12,1,2) ~ "Win",
                             month %in% c(3,4,5) ~ "Sum",
                             month %in% c(6,7,8) ~ "Mon",
                             month %in% c(9,10,11) ~ "Aut")) %>% 
    mutate(month = as.factor(month))


  # the model ---------------------------------------------------------------
  
  fixed_effects <- "OBSERVATION.COUNT ~ month + month:log(no.sp)"
  include_timegroups <- if (singleyear == FALSE) "+ timegroups" else 
                          if (singleyear == TRUE) ""
  random_effects <- if (flag == 0) "+ (1|gridg3/gridg1)" else 
                      if (flag == 1) "+ (1|gridg1)" else 
                      if (flag == 2) ""

  model_formula <- as.formula(glue("{fixed_effects} {include_timegroups} {random_effects}"))

  m1 <- if (flag != 2) {
    glmer(model_formula, 
          data = ed, family = binomial(link = 'cloglog'), 
          nAGQ = 0, control = glmerControl(optimizer = "bobyqa"))
  } else {
    glm(model_formula, 
        data = ed, family = binomial(link = 'cloglog'))
  }
  

  # predicting from model ---------------------------------------------------

  # prepare a new data file to predict

  ltemp <- ed %>% 
    {if (singleyear == FALSE) {
      group_by(., month) %>% 
      reframe(., timegroups = unique(tm$timegroups))
    } else if (singleyear == TRUE) {
      distinct(., month)
    }} %>% 
    mutate(no.sp = medianlla,
           # taking the first value but any random value will do because we do not
           # intend to predict random variation across grids
           gridg1 = data1$gridg1[1], 
           gridg3 = data1$gridg3[1])
  
  f2 <- ltemp %>% 
    {if (singleyear == FALSE) {
      dplyr::select(., timegroups)
    } else if (singleyear == TRUE) {
      .
    }} %>% 
    mutate(freq = 0, se = 0) %>%  # this is not actually needed
    {if (singleyear == FALSE) {
      .
    } else if (singleyear == TRUE) {
      dplyr::select(., freq, se)
    }}
    
  
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
    filter(!is.na(freqt) & !is.na(set)) %>%
    # average across month
    {if (singleyear == FALSE) {
      group_by(., timegroups) %>% 
      reframe(freq = mean(freqt), se = mean(set)) %>% 
      right_join(tm) %>% 
      left_join(databins %>% distinct(timegroups, year)) %>% 
      rename(timegroupsf = timegroups,
             timegroups = year) %>% 
      mutate(timegroupsf = factor(timegroupsf, 
                                  levels = soib_year_info("timegroup_lab"))) %>% 
      complete(timegroupsf) %>% 
      arrange(timegroupsf)
    } else if (singleyear == TRUE) {
      reframe(., freq = mean(freqt), se = mean(set))
    }}
  
  

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

  species = speciesforocc$eBird.English.Name.2023[i]
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
  data_exp = expand_dt(data_filt_mig, species) %>% 
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
    right_join(data, by = c("COMMON.NAME", "timegroups")) %>%
    filter(timegroups == soib_year_info("latest_year")) %>%
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
      
      SoIB.Latest.Long.Term.Status = case_when(
        is.na(longtermmean) ~ "Insufficient Data",
        (longtermrci-longtermmean)/longtermmean > 0.5 ~ "Trend Inconclusive", # arbitrary
        # else
        # for declines
        longtermrci <= 50 ~ "Rapid Decline", # -100% to -50%
        longtermrci > 50 & longtermrci <= 75 ~ "Decline", # -50% to -25%
        # for increases
        longtermlci >= 150 ~ "Rapid Increase", # +50% to inf
        longtermlci < 150 & longtermlci >= 125 ~ "Increase", # +25% to +50%
        # stable vs inconclusive:
        # if CI is completely below or above the baseline, can't be stable
        longtermlci > 100 | longtermrci < 100 ~ "Trend Inconclusive",
        # if one limit is in the Stable zone but other limit passes to Rapid X, can't be stable
        longtermlci <= 50 | longtermrci >= 150 ~ "Trend Inconclusive",
        TRUE ~ "Stable"
      )
      
    ) %>% 
    dplyr::select(COMMON.NAME, SoIB.Latest.Long.Term.Status)
  
  return(data)
  
}

### scale Trend Status bands ------------------------------------------

# scale values in any band to [0, 1]

scale_band <- function(scale_var, min_var, max_var) {
  
  scaled_var <- (scale_var - min_var)/(max_var - min_var)
  
  return(scaled_var)
  
}


# we scale the bands to {-5, -3, -1, 0, 1, 3, 5}
# thresholds for classification of LTT are {0, 50, 75, 100, 125, 150, inf}
# thresholds for classification of CAT are {-4.7, -2.7, -1.1, 0.0, 0.9, 1.6, 3.6}

scale_trends_to_bands <- function(data) {
  
  scaled_data <- data %>% 
    mutate(
      
      # long-term
      ltt_min = case_when(
        SoIB.Latest.Long.Term.Status == "Rapid Decline" ~ 0,
        SoIB.Latest.Long.Term.Status == "Decline" ~ 50,
        SoIB.Latest.Long.Term.Status == "Rapid Increase" ~ 150,
        SoIB.Latest.Long.Term.Status == "Increase" ~ 125,
        # top Stable
        SoIB.Latest.Long.Term.Status == "Stable" & longtermmean >= 100 ~ 100,
        # bottom Stable
        SoIB.Latest.Long.Term.Status == "Stable" & longtermmean < 100 ~ 75,
        TRUE ~ NA
      ),
      
      ltt_max = case_when(
        SoIB.Latest.Long.Term.Status == "Rapid Decline" ~ 50,
        SoIB.Latest.Long.Term.Status == "Decline" ~ 75,
        # taking 200 here (instead of inf) to mirror the delta 50 on the negative side
        SoIB.Latest.Long.Term.Status == "Rapid Increase" ~ 200,
        SoIB.Latest.Long.Term.Status == "Increase" ~ 150,
        # top Stable
        SoIB.Latest.Long.Term.Status == "Stable" & longtermmean >= 100 ~ 125,
        # bottom Stable
        SoIB.Latest.Long.Term.Status == "Stable" & longtermmean < 100 ~ 100,
        TRUE ~ NA
      ),
      
      # here, we need to use mean, lci or rci according to which band it is in
      ltt_prop = case_when(
        SoIB.Latest.Long.Term.Status == "Stable" ~ 
          scale_band(longtermmean, ltt_min, ltt_max),
        SoIB.Latest.Long.Term.Status %in% c("Decline", "Rapid Decline") ~ 
          scale_band(longtermrci, ltt_min, ltt_max),
        SoIB.Latest.Long.Term.Status %in% c("Increase", "Rapid Increase") ~ 
          scale_band(longtermlci, ltt_min, ltt_max),
        TRUE ~ NA
      ),
      
      # new band is sometimes 2 units long, so need to scale to that
      ltt_newrange = case_when(
        SoIB.Latest.Long.Term.Status == "Stable" ~ 1,
        SoIB.Latest.Long.Term.Status %in% c("Decline", "Rapid Decline") ~ 2,
        SoIB.Latest.Long.Term.Status %in% c("Increase", "Rapid Increase") ~ 2,
        TRUE ~ NA
      ),

      # constant to be added to bring the scaled bands (all 0--1 now) to different levels
      ltt_k = case_when(
        SoIB.Latest.Long.Term.Status == "Stable" & longtermmean >= 100 ~ 0,
        SoIB.Latest.Long.Term.Status == "Stable" & longtermmean < 100 ~ -1,
        SoIB.Latest.Long.Term.Status == "Decline" ~ -3,
        SoIB.Latest.Long.Term.Status == "Rapid Decline" ~ -5,
        SoIB.Latest.Long.Term.Status == "Increase" ~ 1,
        SoIB.Latest.Long.Term.Status == "Rapid Increase" ~ 3,
        TRUE ~ NA
      ),

      # current
      cat_min = case_when(
        # taking Stable range to make upper and lower limits as well
        SoIB.Latest.Current.Status == "Rapid Decline" ~ -4.7, 
        SoIB.Latest.Current.Status == "Decline" ~ -2.7,
        SoIB.Latest.Current.Status == "Rapid Increase" ~ 1.6,
        SoIB.Latest.Current.Status == "Increase" ~ 0.9,
        # top Stable
        SoIB.Latest.Current.Status == "Stable" & currentslopemean >= 0 ~ 0,
        # bottom Stable
        SoIB.Latest.Current.Status == "Stable" & currentslopemean < 0 ~ -1.1,
        TRUE ~ NA
      ),
      
      
      cat_max = case_when(
        SoIB.Latest.Current.Status == "Rapid Decline" ~ -2.7,
        SoIB.Latest.Current.Status == "Decline" ~ -1.1,
        SoIB.Latest.Current.Status == "Rapid Increase" ~ 3.6,
        SoIB.Latest.Current.Status == "Increase" ~ 1.6,
        # top Stable
        SoIB.Latest.Current.Status == "Stable" & currentslopemean >= 0 ~ 0.9,
        # bottom Stable
        SoIB.Latest.Current.Status == "Stable" & currentslopemean < 0 ~ 0,
        TRUE ~ NA
      ),
      
      # here, we need to use mean, lci or rci according to which band it is in
      cat_prop = case_when(
        SoIB.Latest.Current.Status == "Stable" ~ 
          scale_band(currentslopemean, cat_min, cat_max),
        SoIB.Latest.Current.Status %in% c("Decline", "Rapid Decline") ~ 
          scale_band(currentsloperci, cat_min, cat_max),
        SoIB.Latest.Current.Status %in% c("Increase", "Rapid Increase") ~ 
          scale_band(currentslopelci, cat_min, cat_max),
        TRUE ~ NA
      ),
      
      # new band is sometimes 2 units long, so need to scale to that
      cat_newrange = case_when(
        SoIB.Latest.Current.Status == "Stable" ~ 1,
        SoIB.Latest.Current.Status %in% c("Decline", "Rapid Decline") ~ 2,
        SoIB.Latest.Current.Status %in% c("Increase", "Rapid Increase") ~ 2,
        TRUE ~ NA
      ),
      
      # constant to be added to bring the scaled bands (all 0--1 now) to different levels
      cat_k = case_when(
        SoIB.Latest.Current.Status == "Stable" & currentslopemean >= 0 ~ 0,
        SoIB.Latest.Current.Status == "Stable" & currentslopemean < 0 ~ -1,
        SoIB.Latest.Current.Status == "Decline" ~ -3,
        SoIB.Latest.Current.Status == "Rapid Decline" ~ -5,
        SoIB.Latest.Current.Status == "Increase" ~ 1,
        SoIB.Latest.Current.Status == "Rapid Increase" ~ 3,
        TRUE ~ NA
      )
      
    ) %>% 
    # shifting to the various new equal bands
    mutate(longterm = ltt_prop*ltt_newrange + ltt_k,
           currentslope = cat_prop*cat_newrange + cat_k) %>% 
    mutate(across(c(longterm, currentslope), ~ case_when(. >= 5 ~ 5, 
                                                         . <= -5 ~ -5,
                                                         TRUE ~ .)))

}

# convert eBird name to India Checklist name ----------------------------------------

specname_to_india_checklist <- function(spec_names, already_show = TRUE) {
  
  names_map <- read.csv("00_data/SoIB_mapping_2023.csv") %>% 
    distinct(eBird.English.Name.2023, India.Checklist.Common.Name)
  
  df_names <- data.frame(OLD = spec_names)
  
  # quit if already India Checklist name (and print if we want to see the message)
  if (all(df_names$OLD %in% names_map$India.Checklist.Common.Name)) {
    
    if (already_show == TRUE){
      print("Species name(s) already align with India Checklist.")
    }
    
    return(spec_names)
  }
  
  df_names <- df_names %>% 
    left_join(names_map, by = c("OLD" = "eBird.English.Name.2023")) %>% 
    rename(NEW = India.Checklist.Common.Name)
  
  if (any(is.na(df_names$NEW))) {
    print("Input species name is not valid eBird name")
    stop()
  }
  
  return(df_names$NEW)
  
}

# update IUCN Status based on latest updated in mapping sheet ---------------------------------------

# input dataframe can be any mapping/main type object with list of species along with IUCN status
# mutates IUCN Status column based on latest Status updated in SoIB_mapping_2023.csv
# preserves column order in input data

# col_specname must be eBird checklist species names

get_latest_IUCN_status <- function(data, col_specname, col_iucn = NULL,
                                     path_mapping = "00_data/SoIB_mapping_2023.csv") {
  
  if (!(is.character(col_specname) & 
        (is.character(col_iucn)) | is.null(col_iucn))) {
    stop("Arguments col_specname and col_iucn can only be character values.")
  }
  
  require(tidyverse)
  
  # col_iucn is the name we want for newly mutated IUCN column
  # (not necessarily name of IUCN column in mapping sheet)
  col_newnames <- if (is.null(col_iucn)) {
    c(col_specname, "IUCN.Category") # default name in mapping sheet
  } else {
    c(col_specname, col_iucn)
  }
  
  col_order <- names(data)
  
  mapping <- read_csv(path_mapping) %>% 
    dplyr::select("eBird.English.Name.2023", "IUCN.Category") %>% 
    magrittr::set_colnames(col_newnames)
  
  data_upd <- data %>% 
    # if IUCN column already exists, remove it before join
    {if (!is.null(col_iucn)) {
      dplyr::select(., -all_of(col_iucn))
    } else {
      .
    }} %>% 
    left_join(mapping, by = col_specname) %>% 
    # if IUCN col existed, preserves exact order; else, will be new col after same old cols
    relocate(all_of(col_order))
  
  return(data_upd)
  
}

# Status categories -----------------------------------------------------------------

get_soib_status_cats <- function(which = NULL) {
  
  cats <- list(
    trend = c("Rapid Decline", "Decline", "Insufficient Data",
              "Trend Inconclusive", "Stable", "Increase", "Rapid Increase"),
    range = c("Historical", "Very Restricted", "Restricted",
              "Moderate", "Large", "Very Large"),
    decline = c("Decline", "Rapid Decline"),
    uncertain = c("Insufficient Data", "Trend Inconclusive"),
    restricted = c("Historical", "Very Restricted", "Restricted"),
    
    # old categories
    trend_soib1 = c("Strong Decline", "Moderate Decline", "Data Deficient",
                    "Uncertain", "Stable", "Moderate Increase", "Strong Increase"),
    decline_soib1 = c("Moderate Decline", "Strong Decline"),
    uncertain_soib1 = c("Data Deficient", "Uncertain")
  )
  
  if (is.null(which)) {
    
    return(cats)
    
  } else {

    if (!which %in% names(cats)) {
      return(glue("Please select one of the following: {names(cats) %>% str_flatten_comma()}"))
    }
    
    return(cats %>% pluck(which))
  }
  
}

# get mapping info for eBird species names --------------------------------

# species names change every year, which proves difficult when working on annual updates
# this function helps map these different names

ebird_tax_mapping <- function() {
  read.csv("00_data/eBird_taxonomy_mapping.csv")
}

# Update specieslists during interannual SoIB updates --------------------

# for interannual updates, we don't rerun (i.e., generate new) specieslists
# mainly because we want to retain the same species set being analysed.
# but nevertheless, taxonomy updates need to reflect.

## update specieslists for each analysis (full, masks, states) for each year based on
## a single mapping file
## Currently it is specific to the years in question but this can be changed in 
## upcoming years

# this update of specieslists happens during the dataspeciesfilter() call itself
# and so gets iterated across masks during each corresponding run


update_species_lists = function(species_list_data, scientific_also = FALSE) {

  # works for both specieslist and restrictedspecieslist, and fullspecieslist

  if (interannual_update == FALSE) {
    stop("Currently running 'major' SoIB update, so specieslists needs to be rerun properly!")
  }

  require(tidyverse)
  
  if (!exists("fullmap")) {
    fullmap <- read.csv("00_data/SoIB_mapping_2023.csv")
  }
  
  

  # when rerunning for same year, need to return unmodified list
  # because species names already updated in prior run
  if (any(!species_list_data$COMMON.NAME %in% ebird_tax_mapping()$eBird.English.Name.2022)) {

    message("Species list is already updated to latest taxonomy. Returning original list.")
    return(species_list_data)

  } else {

    list_new <- species_list_data |> 
      left_join(ebird_tax_mapping(), 
                by = c("COMMON.NAME" = "eBird.English.Name.2022")) |> 
      dplyr::select(-COMMON.NAME) |> 
      relocate(eBird.English.Name.2023) |> # first column is species name
      rename(COMMON.NAME = eBird.English.Name.2023)
    
    if (scientific_also == TRUE) {
      
      list_new <- list_new %>% 
        left_join(fullmap %>% 
                    distinct(eBird.English.Name.2023, eBird.Scientific.Name.2023),
                  by = c("COMMON.NAME" = "eBird.English.Name.2023")) %>% 
        dplyr::select(-SCIENTIFIC.NAME) %>% 
        rename(SCIENTIFIC.NAME = eBird.Scientific.Name.2023) %>% 
        relocate(COMMON.NAME, SCIENTIFIC.NAME)

    }
      
    return(list_new)
  
  }

}


