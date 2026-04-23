#########################################################################
########   Cleaning, Visualizing and Aggregating eBird Data   ########
#########################################################################
install.packages("tidyverse")
install.packages("lubridate")
library(dplyr)                # or library(tidyverse)
library(lubridate)
library(tidyverse)

## Read data 
#gwpelican_ebd = read.delim("ebd_IN_grwpel1_202001_202602_unv_smp_relJan-2026.txt", sep = "\t", 
#                  header = T, quote = "", 
#                  stringsAsFactors = F, na.strings = c(""," ",NA))
gwpelican_query = read.csv("Great_White_Pelican.csv", 
                       header = T, 
                       stringsAsFactors = F, na.strings = c(""," ",NA))

## Filter data
gwpelican_clean = gwpelican_query %>% filter(ALL.SPECIES.REPORTED == TRUE)
gwpelican_clean = gwpelican_clean %>% filter(PROTOCOL.NAME %in% c("Traveling","Stationary"))
View(gwpelican_clean)

# Handle observation counts correctly
# NA = present but not counted
# Numbers = real counts
library(dplyr)
gwpelican_clean <- gwpelican_clean %>%
  mutate(
    OBSERVATION.COUNT = na_if(OBSERVATION.COUNT, "X"),
    OBSERVATION.COUNT = as.numeric(OBSERVATION.COUNT)
  )

# Filter NA fields
gwpelican_clean <- gwpelican_clean %>%
  dplyr::filter(
    !is.na(LATITUDE),
    !is.na(LONGITUDE),
    !is.na(OBSERVATION.COUNT),
    !is.na(SAMPLING.EVENT.IDENTIFIER)
  )
View(gwpelican_clean)

# Keep only necessary columns
gwpelican_clean = gwpelican_clean %>% select(COMMON.NAME,SCIENTIFIC.NAME,OBSERVATION.COUNT,STATE,STATE.CODE,COUNTY,COUNTY.CODE,
                                             LOCALITY,LOCALITY.ID,LOCALITY.TYPE,LATITUDE,LONGITUDE,OBSERVATION.DATE,TIME.OBSERVATIONS.STARTED,
                                             OBSERVER.ID,SAMPLING.EVENT.IDENTIFIER,PROJECT.IDENTIFIERS,DURATION.MINUTES,
                                             EFFORT.DISTANCE.KM,NUMBER.OBSERVERS,GROUP.IDENTIFIER)

# PROJECT.NAMES
# Remove Asian Waterbird Census (India) 2026 data
gwpelican_clean <- gwpelican_clean %>%
  filter(!(PROJECT.IDENTIFIERS == "1051" & year(OBSERVATION.DATE) == 2026))

####################### Basic quick checks #########################
names(gwpelican_clean)
head(gwpelican_clean)
gwpelican_clean$SAMPLING.EVENT.IDENTIFIER[1:50]
gwpelican_clean$GROUP.IDENTIFIER[1:50]
# First: check if there's any problem
library(readr)
problems(gwpelican_clean)
unique(gwpelican_clean$scientific_name)
str(gwpelican_clean$observation_count)
# how much of your data is: counted or presence-only
gwpelican_clean %>%
  count(OBSERVATION.COUNT)
#how many usable counts you have
#how many checklists are presence-only
summary(gwpelican_clean$OBSERVATION.COUNT)
sum(is.na(gwpelican_clean$OBSERVATION.COUNT))
# Range - lat: 6–38; long: 68–98 (filter if bad data)
summary(gwpelican_clean$LATITUDE)
summary(gwpelican_clean$LONGITUDE)
summary(gwpelican_clean)
colSums(is.na(gwpelican_clean[, c(
  "LATITUDE",
  "LONGITUDE",
  "OBSERVATION.DATE",
  "SAMPLING.EVENT.IDENTIFIER"
)]))

########################## Filter season wise data ########################## 
# Step 1: Make sure dates are proper Date objects
gwpelican_clean <- gwpelican_clean %>%
  mutate(OBSERVATION.DATE = as.Date(OBSERVATION.DATE))
class(gwpelican_clean$OBSERVATION.DATE)

# Step2: India-relevant seasonal classification
gwpelican_clean <- gwpelican_clean %>%
  mutate(
    month = month(OBSERVATION.DATE),
    season = case_when(
      month %in% c(11, 12, 1, 2)  ~ "Winter",
      month %in% c(3, 4, 5)   ~ "Summer",
      month %in% c(6, 7, 8) ~ "Monsoon",
      month %in% c(9, 10)   ~ "Post-monsoon"
    )
  )

gwpelican_clean %>% count(season)

# Step 3: Filter by a single season
# Winter records only
gwpelican_winter <- gwpelican_clean %>%
  filter(season == "Winter")
# Monsoon records only
gwpelican_monsoon <- gwpelican_clean %>%
  filter(season %in% c("Monsoon"))
# Post-monsoon records only
gwpelican_postmonsoon <- gwpelican_clean %>%
  filter(season %in% c("Post-monsoon"))
# Summer records only
gwpelican_summer <- gwpelican_clean %>%
  filter(season %in% c("Summer"))

sum(gwpelican_winter$OBSERVATION.COUNT)

# Visualizing the data points on India map
#install.packages("rnaturalearth")
#install.packages("rnaturalearthdata")
#library(ggplot2)
#library(sf)
#library(rnaturalearth)
#library(rnaturalearthdata)

# Load leaflet
library(leaflet)
# Plot points
leaflet(gwpelican_clean) %>%
  addTiles() %>%
  addCircleMarkers(
    lng = ~LONGITUDE,
    lat = ~LATITUDE,
    radius = 3,
    color = "blue",
    fillOpacity = 0.6,
    popup = ~paste("Date:", OBSERVATION.DATE)
  )
