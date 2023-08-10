# prepare data for Myna

require(lubridate)
require(tidyverse)

rawpath = "00_data/ebd_IN_relMay-2023.txt"

# select only necessary columns
preimp = c("TAXONOMIC.ORDER","CATEGORY","COMMON.NAME","SCIENTIFIC.NAME","EXOTIC.CODE",
           "OBSERVATION.COUNT","STATE","STATE.CODE","COUNTY","COUNTY.CODE","LOCALITY",
           "LOCALITY.ID","LOCALITY.TYPE","REVIEWED","APPROVED","LATITUDE","LONGITUDE",
           "OBSERVATION.DATE","SAMPLING.EVENT.IDENTIFIER","PROTOCOL.TYPE",
           "PROTOCOL.CODE","DURATION.MINUTES","EFFORT.DISTANCE.KM",
           "NUMBER.OBSERVERS","ALL.SPECIES.REPORTED","GROUP.IDENTIFIER",
           "TIME.OBSERVATIONS.STARTED","OBSERVER.ID")

nms = read.delim(rawpath, nrows = 1, sep = "\t", header = T, quote = "", stringsAsFactors = F, 
                 na.strings = c(""," ",NA))
nms = names(nms)
nms[!(nms %in% preimp)] = "NULL"
nms[nms %in% preimp] = NA

# read data from certain columns only
data = read.delim(rawpath, colClasses = nms, sep = "\t", header = T, quote = "", 
                  stringsAsFactors = F, na.strings = c(""," ",NA))

data = data %>%
  filter(is.na(EFFORT.DISTANCE.KM) | EFFORT.DISTANCE.KM <= 50) %>%
  # remove unapproved records and records of escapees
  filter(REVIEWED == 0 | APPROVED == 1) %>%
  mutate(OBSERVATION.DATE = as.Date(OBSERVATION.DATE), 
         month = month(OBSERVATION.DATE),
         cyear = year(OBSERVATION.DATE)) %>%
  mutate(year = ifelse(month > 5, cyear, cyear-1)) %>% # from June to May
  mutate(group.id = ifelse(is.na(GROUP.IDENTIFIER), 
                           SAMPLING.EVENT.IDENTIFIER, GROUP.IDENTIFIER)) %>%
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

source('00_scripts/00_functions.R')
# removing false complete lists
data = completelistcheck(data)

## choosing important columns required for further analyses

imp = c("TAXONOMIC.ORDER","CATEGORY","COMMON.NAME","SCIENTIFIC.NAME","EXOTIC.CODE",
        "OBSERVATION.COUNT","STATE","STATE.CODE","COUNTY","COUNTY.CODE","LOCALITY",
        "LOCALITY.ID","LOCALITY.TYPE","LATITUDE","LONGITUDE",
        "OBSERVATION.DATE","SAMPLING.EVENT.IDENTIFIER","PROTOCOL.TYPE",
        "PROTOCOL.CODE","DURATION.MINUTES","EFFORT.DISTANCE.KM",
        "NUMBER.OBSERVERS","ALL.SPECIES.REPORTED","GROUP.IDENTIFIER",
        "OBSERVER.ID")

data = data %>%
  dplyr::select(all_of(imp))

write_delim(data, file = "00_data/dataforMyna.txt", delim = "\t")

