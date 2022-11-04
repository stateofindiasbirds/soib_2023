### import data ########################################

import_hist_spread_data <- function(init_setup = FALSE) {
  
  if (init_setup == TRUE) {
    
    ### if first time setting up
    
    # filtering data for this analysis
    load("ebd_IN_relJun-2022.RData")
    
    tictoc::tic("distinct() over slice()") # total 64 secs
    timeline <- data %>%
      # filtering for complete lists
      filter(ALL.SPECIES.REPORTED == 1 & PROTOCOL.TYPE != "Incidental") %>%
      # slicing to original checklist-level (GROUP.ID)
      distinct(GROUP.ID,
               LOCALITY.ID, LOCALITY, LOCALITY.TYPE, STATE, COUNTY, LATITUDE, LONGITUDE, # space
               OBSERVATION.DATE, YEAR, MONTH, DAY.M, TIME.OBSERVATIONS.STARTED, # time
               PROTOCOL.TYPE, DURATION.MINUTES, EFFORT.DISTANCE.KM, NUMBER.OBSERVERS, # effort
               TRIP.COMMENTS) %>%  # done in 41 seconds on server (faster than slice)
      # have to group-slice again as space, time, effort can vary btwn observers in each GROUP.ID
      group_by(GROUP.ID) %>%
      slice(1) %>%
      ungroup() %>%
      # adding migratory year column
      mutate(M.YEAR = if_else(MONTH > 5, YEAR, YEAR -1), # from June to May
             M.MONTH = if_else(MONTH > 5, MONTH-5, 12-(5-MONTH))) %>%
      mutate(TIME.SOIB1 = case_when(M.YEAR < 1990 ~ "pre-1990",
                                    M.YEAR %in% 1990:1999 ~ "1990-1999",
                                    M.YEAR %in% 2000:2006 ~ "2000-2006",
                                    M.YEAR %in% 2007:2010 ~ "2007-2010",
                                    M.YEAR > 2010 ~ as.character(M.YEAR)),
             # coarser periods: without splitting historical and 2011-13
             TIME.SOIB2 = case_when(M.YEAR < 2000 ~ "pre-2000",
                                    M.YEAR %in% 2000:2006 ~ "2000-2006",
                                    M.YEAR %in% 2007:2010 ~ "2007-2010",
                                    M.YEAR %in% 2011:2013 ~ "2011-2013",
                                    M.YEAR > 2013 ~ as.character(M.YEAR)),
             # for main paper looking at past, middle and present broadly
             TIME.BROAD = case_when(M.YEAR < 2000 ~ "pre-2000",
                                    M.YEAR %in% 2000:2015 ~ "2000-2015",
                                    M.YEAR > 2015 ~ "2016-present")) %>%
      mutate(TIME.SOIB1 = factor(TIME.SOIB1,
                                 levels = c("pre-1990", "1990-1999", "2000-2006", "2007-2010",
                                            as.character(2011:2021))),
             TIME.SOIB2 = factor(TIME.SOIB2,
                                 levels = c("pre-2000", "2000-2006", "2007-2010", "2011-2013",
                                            as.character(2014:2021))),
             TIME.BROAD = factor(TIME.BROAD,
                                 levels = c("pre-2000", "2000-2015",
                                            as.character(2016:2021))))
    tictoc::toc()
    
    
    # adding map variables to main data
    load("maps.RData", envir = .GlobalEnv) # Ashwin's maps data
    timeline <- joinmapvars(timeline)

    data_hist <- timeline %>% filter(M.YEAR < 2000)
    data_cur <- timeline %>% filter(M.YEAR == 2021)
    
    data0 <- data_hist %>% 
      pivot_longer(c(TIME.SOIB1, TIME.SOIB2), names_to = "PERIOD.TYPE", values_to = "PERIOD") %>% 
      mutate(PERIOD.TYPE = str_remove(PERIOD.TYPE, "TIME.")) %>% 
      bind_rows(data_cur %>% rename(PERIOD = TIME.SOIB1) %>% dplyr::select(-TIME.SOIB2)) %>% 
      mutate(PERIOD = factor(PERIOD, levels = soib_levels))
    
    save(data0, timeline, file = "hist_spread/hist_spread.RData")
    rm(data)
    
    assign("timeline", timeline, envir = .GlobalEnv)
    assign("data0", data0, envir = .GlobalEnv)

  } else if (init_setup == FALSE) {
    
    ### if latest hist_spread.RData exists
    
    # Ashwin's maps data
    load("maps.RData", envir = .GlobalEnv)
    
    # latest .RData with required objects (created in previous section)
    load("hist_spread/hist_spread.RData", envir = .GlobalEnv)

  }
  
  
  # ecoregions
  source("hist_spread/ecoregions.R")
  
  # reclassification of ecoregion data (from discussion with group)
  reclass <- read_csv("hist_spread/Ecoregions2017_reclassification.csv")

  ecoregions <- ecoregions %>% 
    left_join(reclass) %>% 
    dplyr::select(-ECO_NAME, -ECO_RECLASS1, -ECO_RECLASS2, -FINAL_RECLASS) %>% 
    # to merge original ecoregions according to PJ_RECLASS
    group_by(PJ_RECLASS) %>% 
    summarise(geometry = st_union(geometry)) %>% 
    ungroup()
  
  
  assign("reclass", reclass, envir = .GlobalEnv)
  assign("ecoregions", ecoregions, envir = .GlobalEnv)
  
}


### joinmapvars ########################################

# adapted from Ashwin's "addmapvars" function to allow use within data preparation steps
# i.e., inputs and outputs are data objects in the environment, no writing of files involved

# data: main data object to which map vars will be added (needs to be sliced already!)
# admin = T if DISTRICT and ST_NM from shapefile required, else F
# grids = T if grid cells (four resolutions) from shapefile required, else F


#### maps.RData must already be loaded
#### column names here are all uppercase, unlike in Ashwin's function


joinmapvars = function(data, admin = T, grids = T){
  
  require(tidyverse)
  # require(data.table)
  require(sp)
  require(rgeos)
  
  # single object at group ID level (same group ID, same grid/district/state)
  temp0 <- data 

  
  ### add columns with DISTRICT and ST_NM to main data 
  
  if (admin == T) {
    
    temp = temp0 # separate object to prevent repeated slicing (intensive step)
    
    rownames(temp) = temp$GROUP.ID # only to setup adding the group.id column for the future left_join
    coordinates(temp) = ~LONGITUDE + LATITUDE # convert to SPDF
    proj4string(temp) = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
    
    temp = sp::over(temp, districtmap) %>% # returns only ATTRIBUTES of districtmap (DISTRICT and ST_NM)
      dplyr::select(1, 2) %>% 
      rename(DISTRICT = dtname,
             ST_NM = stname) %>% 
      rownames_to_column("GROUP.ID") 
    
    data = left_join(temp, data)
    
  }
  
  ### add grid cell info (at four resolutions) to main data
  
  if (grids == T) {
    
    temp = temp0
    
    rownames(temp) = temp$GROUP.ID
    coordinates(temp) = ~LONGITUDE + LATITUDE
    
    temp = sp::over(temp, gridmapg1) %>% 
      rownames_to_column("GROUP.ID") %>% 
      rename(GRIDG1 = id)
    
    data = left_join(temp, data)
    
    
    temp = temp0
    
    rownames(temp) = temp$GROUP.ID
    coordinates(temp) = ~LONGITUDE + LATITUDE
    
    temp = sp::over(temp, gridmapg2) %>% 
      rownames_to_column("GROUP.ID") %>% 
      rename(GRIDG2 = id)
    
    data = left_join(temp, data)
    
    
    temp = temp0
    
    rownames(temp) = temp$GROUP.ID
    coordinates(temp) = ~LONGITUDE + LATITUDE
    
    temp = sp::over(temp, gridmapg3) %>% 
      rownames_to_column("GROUP.ID") %>% 
      rename(GRIDG3 = id)
    
    data = left_join(temp, data)
    
    
    temp = temp0
    
    rownames(temp) = temp$GROUP.ID
    coordinates(temp) = ~LONGITUDE + LATITUDE
    
    temp = sp::over(temp, gridmapg4) %>% 
      rownames_to_column("GROUP.ID") %>% 
      rename(GRIDG4 = id)
    
    data = left_join(temp, data)
  
  }
  
  return(data)
  
}



### creating maps with ggplot ########################################

# Indirectly referring to variables:
# https://ggplot2-book.org/programming.html#indirectly-referring-to-variables

# The key to calling a tidy evaluation function inside of another function is to 
# quote (with enquo()) and unquote (with !!).
# from https://www.tidyverse.org/blog/2018/07/ggplot2-tidy-evaluation/

gg_map <- function(data, datalong, datalat, sf = TRUE, facetvar, ncol = 2,
                   poly1, poly1long = long, poly1lat = lat,
                   poly2, poly2long = long, poly2lat = lat, poly2type = "grid",
                   mainvar, na_fill = "#ACACAC",
                   title, subtitle, legend_title) {
  
  # if the data object provided is an sf object, it should also be used in the geom_sf() call
  if (sf == TRUE) {
    data_sf <- data
  }
  # if polygon to be plotted within India is grid, size should be bigger than for regions
  if (poly2type == "grid") {
    poly2size <- 0.2
  } else if (poly2type == "region") {
    poly2size <- 0.1
  }
  
  # "enquoting" the data-vars for tidyeval
  
  facetvar <- enquo(facetvar)
  mainvar <- enquo(mainvar)
  # na_fill <- enquo(na_fill)
  # title <- enquo(title)
  # subtitle <- enquo(subtitle)
  # legend_title <- enquo(legend_title)
  
  datalong <- enquo(datalong)
  datalat <- enquo(datalat)
  poly1long <- enquo(poly1long)
  poly1lat <- enquo(poly1lat)
  poly2long <- enquo(poly2long)
  poly2lat <- enquo(poly2lat)
  
  
  # plotting
  
  ggplot(data, aes(!!datalong, !!datalat)) +
    facet_wrap(vars(!!facetvar), ncol = ncol) +
    geom_polygon(data = poly1,
                 aes(!!poly1long, !!poly1lat, group = group),
                 colour = "black", fill = NA, size = 0.2) +
    # geom_polygon(data = poly2,
    #              aes(!!poly2long, !!poly2lat, group = group, fill = !!mainvar)) +
    {if (sf == TRUE) {
      geom_sf(data = data_sf, aes(geometry = geometry, fill = !!mainvar),
              size = poly2size)
    }} +
    scale_fill_viridis_c(na.value = na_fill, option = "inferno", label = scales::comma) +
    labs(title = title,
         subtitle = subtitle,
         fill = legend_title) +
    theme(axis.line = element_blank(),
          axis.title = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          legend.position = "bottom")
  
}
