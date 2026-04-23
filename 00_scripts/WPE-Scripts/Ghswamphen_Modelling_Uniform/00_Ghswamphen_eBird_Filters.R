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
ghswamphen_query = read.csv("Gray_headed_Swamphen.csv", 
                           header = T, 
                           stringsAsFactors = F, na.strings = c(""," ",NA))

## Filter data
ghswamphen_clean = ghswamphen_query %>% filter(ALL.SPECIES.REPORTED == TRUE)
ghswamphen_clean = ghswamphen_clean %>% filter(PROTOCOL.NAME %in% c("Traveling","Stationary"))
View(ghswamphen_clean)

# Handle observation counts correctly
# NA = present but not counted
# Numbers = real counts
library(dplyr)
ghswamphen_clean <- ghswamphen_clean %>%
  mutate(
    OBSERVATION.COUNT = na_if(OBSERVATION.COUNT, "X"),
    OBSERVATION.COUNT = as.numeric(OBSERVATION.COUNT)
  )

# Filter NA fields
ghswamphen_clean <- ghswamphen_clean %>%
  dplyr::filter(
    !is.na(LATITUDE),
    !is.na(LONGITUDE),
    !is.na(OBSERVATION.COUNT),
    !is.na(SAMPLING.EVENT.IDENTIFIER)
  )
View(ghswamphen_clean)

# For non-shared checklists: use SAMPLING EVENT IDENTIFIER
# For shared checklists: use GROUP IDENTIFIER
ghswamphen_clean$CHECKLIST.ID <- ifelse(
  is.na(ghswamphen_clean$`GROUP.IDENTIFIER`),
  ghswamphen_clean$`SAMPLING.EVENT.IDENTIFIER`,
  ghswamphen_clean$`GROUP.IDENTIFIER`
)
nrow(ghswamphen_clean)    # original rows

library(dplyr)
ghswamphen_clean <- ghswamphen_clean %>%
  group_by(CHECKLIST.ID) %>%
  slice_min(order_by = `SAMPLING.EVENT.IDENTIFIER`) %>%
  ungroup()

# Filter outliers
# Duration = 300 minutes
# Distance = 15 km
# Observers = 60
ghswamphen_clean <- ghswamphen_clean %>%
  filter(DURATION.MINUTES <= 300,
         EFFORT.DISTANCE.KM <= 10,
         NUMBER.OBSERVERS <= 60)

nrow(ghswamphen_clean)   # rows after removing shared duplicates

# Keep only necessary columns
ghswamphen_clean = ghswamphen_clean %>% select(COMMON.NAME,SCIENTIFIC.NAME,OBSERVATION.COUNT,STATE,STATE.CODE,COUNTY,COUNTY.CODE,
                                             LOCALITY,LOCALITY.ID,LOCALITY.TYPE,LATITUDE,LONGITUDE,OBSERVATION.DATE,TIME.OBSERVATIONS.STARTED,
                                             OBSERVER.ID,SAMPLING.EVENT.IDENTIFIER,PROJECT.IDENTIFIERS,PROTOCOL.NAME,DURATION.MINUTES,
                                             EFFORT.DISTANCE.KM,NUMBER.OBSERVERS,GROUP.IDENTIFIER,CHECKLIST.ID)

# Add columns
ghswamphen_clean <- ghswamphen_clean %>%
  mutate(
    OBSERVATION.DATE = as.Date(OBSERVATION.DATE),
    MONTH = month(OBSERVATION.DATE, label = TRUE),
    YEAR = year(OBSERVATION.DATE),
    DAY.OF.YEAR = as.numeric(format(OBSERVATION.DATE, "%j"))
  )

# PROJECT.NAMES
# Remove Asian Waterbird Census (India) 2026 data
ghswamphen_clean <- ghswamphen_clean %>%
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

##################### Visualizing the data points on India map ###############
#install.packages("rnaturalearth")
#install.packages("rnaturalearthdata")
#library(ggplot2)
#library(sf)
#library(rnaturalearth)
#library(rnaturalearthdata)

# Load leaflet
library(leaflet)
# Plot points
leaflet(ghswamphen_clean) %>%
  addTiles() %>%
  addCircleMarkers(
    lng = ~LONGITUDE,
    lat = ~LATITUDE,
    radius = 1,
    color = "blue",
    fillOpacity = 0.5,
    popup = ~paste("Date:", OBSERVATION.DATE)
  )
