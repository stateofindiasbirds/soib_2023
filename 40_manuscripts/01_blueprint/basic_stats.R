library(tidyverse)

regionmap = read.csv("00_data/eBird_district_region_mapping.csv")

require(lubridate)
require(tidyverse)

rawpath = "00_data/ebd_IN_relMay-2023.txt"
sensitivepath = "00_data/ebd_sensitive_relMay-2023_IN.txt"

# select only necessary columns
preimp = c("COMMON.NAME","REVIEWED","APPROVED","STATE","COUNTY",
           "OBSERVATION.DATE","OBSERVER.ID",
           "PROTOCOL.NAME","DURATION.MINUTES","EFFORT.DISTANCE.KM","EXOTIC.CODE",
           "ALL.SPECIES.REPORTED","GROUP.IDENTIFIER","SAMPLING.EVENT.IDENTIFIER")

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

data = data %>%
  # create a column "group.id" which can help remove duplicate checklists
  mutate(group.id = ifelse(is.na(GROUP.IDENTIFIER), 
                           SAMPLING.EVENT.IDENTIFIER, GROUP.IDENTIFIER)) %>%
  # set date, add month, year and day columns using package LUBRIDATE
  mutate(OBSERVATION.DATE = as.Date(OBSERVATION.DATE), 
         month = month(OBSERVATION.DATE),
         cyear = year(OBSERVATION.DATE)) %>%
  dplyr::select(-c("OBSERVATION.DATE")) %>%
  mutate(year = ifelse(month > 5, cyear, cyear-1)) %>% # from June to May
  filter(year < 2023) %>%
  ungroup()



stats2014 = data %>% filter(cyear == 2014) %>% 
  reframe(observations = n(),checklists = n_distinct(SAMPLING.EVENT.IDENTIFIER),observers = n_distinct(OBSERVER.ID))
stats2022 = data %>% filter(cyear == 2022) %>% 
  reframe(observations = n(),checklists = n_distinct(SAMPLING.EVENT.IDENTIFIER),observers = n_distinct(OBSERVER.ID))
statsbefore2014 = data %>% filter(cyear < 2014) %>% 
  reframe(observations = n(),checklists = n_distinct(SAMPLING.EVENT.IDENTIFIER),observers = n_distinct(OBSERVER.ID))
statstotal = data %>%
  reframe(observations = n(),checklists = n_distinct(SAMPLING.EVENT.IDENTIFIER),observers = n_distinct(OBSERVER.ID))



