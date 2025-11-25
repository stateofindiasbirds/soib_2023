# prepare data for Myna

require(lubridate)
require(tidyverse)

source("00_scripts/00_functions.R")
latest_year = soib_year_info()
rawpath = paste("00_data/ebd_IN_relJun-",latest_year+1,".txt",sep="")

# select only necessary columns
preimp = c("TAXONOMIC.ORDER","CATEGORY","COMMON.NAME","SCIENTIFIC.NAME","EXOTIC.CODE",
           "OBSERVATION.COUNT","STATE","STATE.CODE","COUNTY","COUNTY.CODE","LOCALITY",
           "LOCALITY.ID","LOCALITY.TYPE","REVIEWED","APPROVED","LATITUDE","LONGITUDE",
           "OBSERVATION.DATE","SAMPLING.EVENT.IDENTIFIER","PROTOCOL.NAME",
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

source('00_scripts/00_functions.R')
# removing false complete lists
data = completelistcheck(data)

## choosing important columns required for further analyses

imp = c("TAXONOMIC.ORDER","CATEGORY","COMMON.NAME","SCIENTIFIC.NAME","EXOTIC.CODE",
        "OBSERVATION.COUNT","STATE","STATE.CODE","COUNTY","COUNTY.CODE","LOCALITY",
        "LOCALITY.ID","LOCALITY.TYPE","LATITUDE","LONGITUDE",
        "OBSERVATION.DATE","SAMPLING.EVENT.IDENTIFIER","PROTOCOL.NAME",
        "PROTOCOL.CODE","DURATION.MINUTES","EFFORT.DISTANCE.KM",
        "NUMBER.OBSERVERS","ALL.SPECIES.REPORTED","GROUP.IDENTIFIER",
        "OBSERVER.ID")

data = data %>%
  dplyr::select(all_of(imp))

writepath = paste("00_data/Myna/dataforMyna_",latest_year+1,".txt",sep="")

write_delim(data, file = writepath, delim = "\t")







## additional annual data as required for Myna

require(lubridate)
require(tidyverse)

source("00_scripts/00_functions.R")
latest_year = soib_year_info()

imp_full = c("TAXONOMIC.ORDER","CATEGORY","COMMON.NAME","SCIENTIFIC.NAME","EXOTIC.CODE",
        "OBSERVATION.COUNT","STATE","STATE.CODE","COUNTY","COUNTY.CODE","LOCALITY",
        "LOCALITY.ID","LOCALITY.TYPE","LATITUDE","LONGITUDE",
        "OBSERVATION.DATE","SAMPLING.EVENT.IDENTIFIER","PROTOCOL.NAME",
        "PROTOCOL.CODE","DURATION.MINUTES","EFFORT.DISTANCE.KM",
        "NUMBER.OBSERVERS","ALL.SPECIES.REPORTED","GROUP.IDENTIFIER",
        "OBSERVER.ID")

imp = c("CATEGORY","SCIENTIFIC.NAME",
        "OBSERVATION.COUNT","STATE","STATE.CODE","COUNTY","COUNTY.CODE","LOCALITY",
        "LOCALITY.ID","LOCALITY.TYPE","LATITUDE","LONGITUDE",
        "OBSERVATION.DATE","SAMPLING.EVENT.IDENTIFIER","PROTOCOL.NAME",
        "PROTOCOL.CODE","DURATION.MINUTES","EFFORT.DISTANCE.KM",
        "ALL.SPECIES.REPORTED","GROUP.IDENTIFIER",
        "OBSERVER.ID")

rawpath = paste("00_data/Myna/dataforMyna_",latest_year+1,".txt",sep="")

data_current = read.delim(rawpath, sep = "\t", header = T)
data_past = read.delim("00_data/Myna/dataforMyna.txt", sep = "\t", header = T)
data_current_1 = data_current %>%
  mutate(OBSERVATION.DATE = as.Date(OBSERVATION.DATE)) %>%
  filter(!EXOTIC.CODE %in% c("X","P")) %>%
  filter(!COMMON.NAME %in% 
           c("Band-bellied Crake","Spur-winged Lapwing","Cape Petrel","African Openbill","Levant Sparrowhawk"))
data_past_1 = data_past %>%
  mutate(OBSERVATION.DATE = as.Date(OBSERVATION.DATE))

# fullmap = read.csv("00_data/SoIB_mapping_2024.csv") %>%
#   dplyr::select(eBird.English.Name.2024,eBird.Scientific.Name.2024)
# updatemap = ebird_tax_mapping() %>%
#   left_join(fullmap) %>%
#   rename(COMMON.NAME = eBird.English.Name.2023)

data_current_1 = data_current_1 %>%
  mutate(OBSERVATION.DATE = as.Date(OBSERVATION.DATE), 
         month = month(OBSERVATION.DATE),
         cyear = year(OBSERVATION.DATE)) %>%
  mutate(year = ifelse(month > 5, cyear, cyear-1)) %>% # from June to May
  filter(year != latest_year+1) %>%
  ungroup()

data_past_1 = data_past_1 %>%
  mutate(OBSERVATION.DATE = as.Date(OBSERVATION.DATE), 
         month = month(OBSERVATION.DATE),
         cyear = year(OBSERVATION.DATE)) %>%
  mutate(year = ifelse(month > 5, cyear, cyear-1)) %>% # from June to May
  ungroup()

# new data in the current year

newdata_current = data_current_1 %>%
  filter(year == latest_year) %>%
  dplyr::select(all_of(imp))

# new data for the past

newdatatill_past = data_current_1 %>%
  filter(year != latest_year, cyear >= 1900) %>%
  dplyr::select(all_of(imp))
data_past_1 = data_past_1 %>%
  filter(cyear >= 1900) %>%
  dplyr::select(all_of(imp))

data_past_1 = data_past_1 %>%
  mutate(LATITUDE = as.numeric(LATITUDE),
         LONGITUDE = as.numeric(LONGITUDE),
         DURATION.MINUTES = as.numeric(DURATION.MINUTES))

# deleted data from the past, but not where only scientific name  has changed

primary_key1 = data_past_1 %>% 
  dplyr::select(SCIENTIFIC.NAME)

primary_key2 = newdatatill_past %>% 
  dplyr::select(SCIENTIFIC.NAME)

diff = primary_key1 %>%
  setdiff(primary_key2)

## remove any rows that have scientific names that do not appear in the new data

data_past_2 =  data_past_1 %>%
  anti_join(diff)

newdata_past = newdatatill_past %>%
  setdiff(data_past_2)

deleteddata_past  = data_past_2 %>%
  setdiff(newdatatill_past)


## Create new columns for taxonomic order and common name

newdata_current = newdata_current %>%
  mutate(TAXONOMIC.ORDER = "delete",
         COMMON.NAME = "delete",
         EXOTIC.CODE = "delete",
         NUMBER.OBSERVERS = "delete") %>%
  dplyr::select(all_of(imp_full))

newdata_past = newdata_past %>%
  mutate(TAXONOMIC.ORDER = "delete",
         COMMON.NAME = "delete",
         EXOTIC.CODE = "delete",
         NUMBER.OBSERVERS = "delete") %>%
  dplyr::select(all_of(imp_full))

deleteddata_past = deleteddata_past %>%
  mutate(TAXONOMIC.ORDER = "delete",
         COMMON.NAME = "delete",
         EXOTIC.CODE = "delete",
         NUMBER.OBSERVERS = "delete") %>%
  dplyr::select(all_of(imp_full))

## create S-24

source("00_scripts/00_functions.R")

main = read.csv("01_analyses_full/results/SoIB_main.csv") %>%
  dplyr::select(eBird.English.Name.2022,SOIBv2.Priority.Status,
                SOIBv2.Long.Term.Status,SOIBv2.Current.Status,SOIBv2.Range.Status) %>%
  left_join(ebird_tax_mapping()) %>%
  dplyr::select(-eBird.English.Name.2022) %>%
  rename(SOIB.Concern.Status = SOIBv2.Priority.Status,
         SOIB.Long.Term.Status = SOIBv2.Long.Term.Status,
         SOIB.Current.Status = SOIBv2.Current.Status,
         SOIB.Range.Status = SOIBv2.Range.Status)

  

tax_base = read.csv("00_data/SoIB_mapping_2024.csv") %>%
  rename(SOIB.Concern.Status = SoIB.Past.Priority.Status,
                SOIB.Long.Term.Status = SoIB.Past.Long.Term.Status,
                SOIB.Current.Status = SoIB.Past.Current.Status,
                SOIB.Range.Status = SoIB.Past.Range.Status)

tax = read.csv("00_data/SoIB_mapping_2024.csv") %>%
  dplyr::select(-SoIB.Past.Priority.Status,-SoIB.Past.Long.Term.Status,
                -SoIB.Past.Current.Status,-SoIB.Past.Range.Status) %>%
  filter(!eBird.English.Name.2024 %in% 
           c("Band-bellied Crake","Spur-winged Lapwing","Cape Petrel")) %>%
  left_join(main) %>%
  dplyr::select(names(tax_base)) %>%
  rename(eBird.English.Name.2023 = eBird.English.Name.2024,
         eBird.Scientific.Name.2023 = eBird.Scientific.Name.2024)



## write all files

# https://docs.google.com/document/d/1pkb0ftUJ98qgMIlyqlh5bHwvTd71a4aVFtmo_E0hTzk/edit#heading=h.rh4agtvap9l0
write_delim(newdata_current, file = "00_data/Myna/D-24.txt", delim = "\t")
write_delim(newdata_past, file = "00_data/Myna/Plus-D-23.txt", delim = "\t")
write_delim(deleteddata_past, file = "00_data/Myna/Minus-D-23.txt", delim = "\t")
write.csv(tax, file = "00_data/Myna/S-24.csv", row.names = F)
write.csv(diff, file = "00_data/Myna/Minus-S-23.csv", row.names = F)



###########################################################################
#########################################################################

## District test case

dist = "Sangareddy"
dist.code = "IN-TS-SR"

require(lubridate)
require(tidyverse)

source("00_scripts/00_functions.R")
latest_year = soib_year_info()

imp_full = c("TAXONOMIC.ORDER","CATEGORY","COMMON.NAME","SCIENTIFIC.NAME","EXOTIC.CODE",
             "OBSERVATION.COUNT","STATE","STATE.CODE","COUNTY","COUNTY.CODE","LOCALITY",
             "LOCALITY.ID","LOCALITY.TYPE","LATITUDE","LONGITUDE",
             "OBSERVATION.DATE","SAMPLING.EVENT.IDENTIFIER","PROTOCOL.NAME",
             "PROTOCOL.CODE","DURATION.MINUTES","EFFORT.DISTANCE.KM",
             "NUMBER.OBSERVERS","ALL.SPECIES.REPORTED","GROUP.IDENTIFIER",
             "OBSERVER.ID")

imp = c("CATEGORY","SCIENTIFIC.NAME",
        "OBSERVATION.COUNT","STATE","STATE.CODE","COUNTY","COUNTY.CODE","LOCALITY",
        "LOCALITY.ID","LOCALITY.TYPE","LATITUDE","LONGITUDE",
        "OBSERVATION.DATE","SAMPLING.EVENT.IDENTIFIER","PROTOCOL.NAME",
        "PROTOCOL.CODE","DURATION.MINUTES","EFFORT.DISTANCE.KM",
        "ALL.SPECIES.REPORTED","GROUP.IDENTIFIER",
        "OBSERVER.ID")

rawpath = paste("00_data/Myna/dataforMyna_",latest_year+1,".txt",sep="")

data_current = read.delim(rawpath, sep = "\t", header = T)

data_current_1 = data_current %>%
  # filter for district
  filter(COUNTY.CODE == dist.code) %>%
  mutate(OBSERVATION.DATE = as.Date(OBSERVATION.DATE)) %>%
  filter(!EXOTIC.CODE %in% c("X","P")) %>%
  filter(!COMMON.NAME %in% 
           c("Band-bellied Crake","Spur-winged Lapwing","Cape Petrel","African Openbill","Levant Sparrowhawk"))

data_current_1 = data_current_1 %>%
  mutate(OBSERVATION.DATE = as.Date(OBSERVATION.DATE), 
         month = month(OBSERVATION.DATE),
         cyear = year(OBSERVATION.DATE)) %>%
  mutate(year = ifelse(month > 5, cyear, cyear-1)) %>% # from June to May
  filter(year != latest_year+1) %>%
  ungroup()

newdata_current = data_current_1 %>%
  dplyr::select(all_of(imp))

newdata_current = newdata_current %>%
  mutate(TAXONOMIC.ORDER = "delete",
         COMMON.NAME = "delete",
         EXOTIC.CODE = "delete",
         NUMBER.OBSERVERS = "delete") %>%
  dplyr::select(all_of(imp_full))

name = paste("00_data/Myna/",dist,"_data_for_testing.txt",sep="")

write_delim(newdata_current, file = name, delim = "\t")



# subsetting using the district shapefile

data = data_current

require(sf)
mappath3 = "00_data/maps_sf.RData"
load(mappath3)

sf_use_s2(FALSE)

data = data %>%
  mutate(group.id = ifelse(is.na(GROUP.IDENTIFIER), 
                           SAMPLING.EVENT.IDENTIFIER, GROUP.IDENTIFIER))

temp = data %>%
  distinct(group.id, LONGITUDE, LATITUDE) %>% 
  distinct(group.id, .keep_all = TRUE) |> 
  # joining map vars to EBD
  st_as_sf(coords = c("LONGITUDE", "LATITUDE"), remove = F) %>% 
  st_set_crs(st_crs(dists_sf)) %>%
  st_join(dists_sf %>% dplyr::select(DISTRICT.NAME)) %>%
  st_drop_geometry()

temp = temp %>% 
  distinct(DISTRICT.NAME, group.id) %>% 
  distinct(group.id, .keep_all = TRUE) |> 
  magrittr::set_colnames(c("DISTRICT.NAME","group.id"))

data = data %>% 
  left_join(temp)


data_current_1 = data %>%
  # filter for district
  filter(DISTRICT.NAME == dist) %>%
  mutate(OBSERVATION.DATE = as.Date(OBSERVATION.DATE)) %>%
  filter(!EXOTIC.CODE %in% c("X","P")) %>%
  filter(!COMMON.NAME %in% 
           c("Band-bellied Crake","Spur-winged Lapwing","Cape Petrel","African Openbill","Levant Sparrowhawk"))

data_current_1 = data_current_1 %>%
  mutate(OBSERVATION.DATE = as.Date(OBSERVATION.DATE), 
         month = month(OBSERVATION.DATE),
         cyear = year(OBSERVATION.DATE)) %>%
  mutate(year = ifelse(month > 5, cyear, cyear-1)) %>% # from June to May
  filter(year != latest_year+1) %>%
  ungroup()

newdata_current_shp = data_current_1 %>%
  dplyr::select(all_of(imp))

newdata_current_shp = newdata_current_shp %>%
  mutate(TAXONOMIC.ORDER = "delete",
         COMMON.NAME = "delete",
         EXOTIC.CODE = "delete",
         NUMBER.OBSERVERS = "delete") %>%
  dplyr::select(all_of(imp_full))

name = paste("00_data/Myna/",dist,"_data_for_testing_shp.txt",sep="")

write_delim(newdata_current_shp, file = name, delim = "\t")

diff1 = newdata_current %>%
  setdiff(newdata_current_shp)

diff2 = newdata_current_shp %>%
  setdiff(newdata_current)

name1 = paste("00_data/Myna/",dist,"_different_eBirdvsshp.csv",sep="")
name2 = paste("00_data/Myna/",dist,"_different_shpvseBird.csv",sep="")


write.csv(diff1, file = name1, row.names = F)
write.csv(diff2, file = name2, row.names = F)
