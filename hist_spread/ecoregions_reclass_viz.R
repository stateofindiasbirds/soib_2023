library(tidyverse)
library(leaflet)
library(raster)
library(sf)
library(glue)
library(htmlwidgets)


### importing ecoregions information:

load("maps.RData", envir = .GlobalEnv)
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

ecoregions_reclass_viz <- leaflet(ecoregions) %>%
  addTiles() %>% 
  addPolygons(color = "#5C5C5C", 
              weight = 0.75, 
              smoothFactor = 0.5,
              opacity = 1.0, 
              fillOpacity = 0.75,
              fillColor = "#ACACAC",
              highlightOptions = highlightOptions(color = "#3D5467", 
                                                  fillColor = "#5B7E9A",
                                                  weight = 1.5,
                                                  bringToFront = TRUE),
              label = ecoregions$PJ_RECLASS,
              labelOptions = labelOptions(style = list("font-weight" = "normal", 
                                                       padding = "3px 8px"),
                                          textsize = "15px"))

saveWidget(ecoregions_reclass_viz, file = "hist_spread/ecoregions_reclass_viz.html")

# ref: https://rstudio.github.io/leaflet/choropleths.html  
# how to save leaflet maps: https://stackoverflow.com/questions/30110377/saving-leaflet-output-as-html