library(lubridate)
library(tidyverse)
rawpath1 = paste("00_data/dataforMyna.txt",sep="")
rawpath2 = paste("00_data/dataforMyna_2024.txt",sep="")

data1 = read.delim(rawpath1, sep = "\t", header = T)
data1_com = data1 %>%
  dplyr::select(-TAXONOMIC.ORDER,-COMMON.NAME,-EXOTIC.CODE,-NUMBER.OBSERVERS)
data2 = read.delim(rawpath2, sep = "\t", header = T)
data2_com = data2 %>%
  dplyr::select(-TAXONOMIC.ORDER,-COMMON.NAME,-EXOTIC.CODE,-NUMBER.OBSERVERS)

minusd = read.delim("00_data/Minus-D-23.txt", sep = "\t", header = T)
minusd_com = minusd %>%
  dplyr::select(-TAXONOMIC.ORDER,-COMMON.NAME,-EXOTIC.CODE,-NUMBER.OBSERVERS) %>%
  mutate(LATITUDE = as.character(LATITUDE),
         LONGITUDE = as.character(LONGITUDE),
         DURATION.MINUTES = as.character(DURATION.MINUTES))
data2_com = data2_com %>%
  mutate(LATITUDE = as.character(LATITUDE),
         LONGITUDE = as.character(LONGITUDE),
         DURATION.MINUTES = as.character(DURATION.MINUTES))


duplicates = data1_com %>%
  semi_join(minusd_com)

imp_full = c("TAXONOMIC.ORDER","CATEGORY","COMMON.NAME","SCIENTIFIC.NAME","EXOTIC.CODE",
             "OBSERVATION.COUNT","STATE","STATE.CODE","COUNTY","COUNTY.CODE","LOCALITY",
             "LOCALITY.ID","LOCALITY.TYPE","LATITUDE","LONGITUDE",
             "OBSERVATION.DATE","SAMPLING.EVENT.IDENTIFIER","PROTOCOL.TYPE",
             "PROTOCOL.CODE","DURATION.MINUTES","EFFORT.DISTANCE.KM",
             "NUMBER.OBSERVERS","ALL.SPECIES.REPORTED","GROUP.IDENTIFIER",
             "OBSERVER.ID")

duplicates_to_remove = duplicates %>%
  group_by(across(everything())) %>%    # Group by all columns
  filter(n() > 1) %>%                   # Keep only groups with more than 1 row
  ungroup() %>%
  mutate(TAXONOMIC.ORDER = "delete",
         COMMON.NAME = "delete",
         EXOTIC.CODE = "delete",
         NUMBER.OBSERVERS = "delete") %>%
  dplyr::select(all_of(imp_full))

data2_com_mod = data2_com %>%
  mutate(TAXONOMIC.ORDER = "delete",
         COMMON.NAME = "delete",
         EXOTIC.CODE = "delete",
         NUMBER.OBSERVERS = "delete") %>%
  dplyr::select(all_of(imp_full))

duplicates_to_add = data2_com_mod %>%
  semi_join(duplicates_to_remove)

duplicates_base = data1_com %>%
  group_by(across(everything())) %>%    # Group by all columns
  filter(n() > 1) %>%                   # Keep only groups with more than 1 row
  ungroup()


write_delim(duplicates_to_remove, file = "00_data/Duplicates-Minus-D-23.txt", delim = "\t")

write_delim(duplicates_to_add, file = "00_data/Duplicates-Plus-24.txt", delim = "\t")

check = data1 %>%
  filter(COMMON.NAME == "Greater Coucal",
         TAXONOMIC.ORDER == 3099,
         SAMPLING.EVENT.IDENTIFIER == "S132342612")
