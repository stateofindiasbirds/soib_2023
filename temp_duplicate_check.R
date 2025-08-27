library(lubridate)
library(tidyverse)
rawpath1 = paste("00_data/Myna/dataforMyna.txt",sep="")
rawpath2 = paste("00_data/Myna/dataforMyna_2024.txt",sep="")

data1 = read.delim(rawpath1, sep = "\t", header = T)
data1_com = data1 %>%
  dplyr::select(-TAXONOMIC.ORDER,-COMMON.NAME,-EXOTIC.CODE,-NUMBER.OBSERVERS)
data2 = read.delim(rawpath2, sep = "\t", header = T)
data2_com = data2 %>%
  dplyr::select(-TAXONOMIC.ORDER,-COMMON.NAME,-EXOTIC.CODE,-NUMBER.OBSERVERS)

minusd = read.delim("00_data/Myna/Minus-D-23.txt", sep = "\t", header = T)
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
             "OBSERVATION.DATE","SAMPLING.EVENT.IDENTIFIER","PROTOCOL.NAME",
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


write_delim(duplicates_to_remove, file = "00_data/Myna/Duplicates-Minus-D-23.txt", delim = "\t")

write_delim(duplicates_to_add, file = "00_data/Myna/Duplicates-Plus-24.txt", delim = "\t")



######### fix incorrect priority statuses

s24old = read.csv("00_data/Myna/S-24-old.csv") %>%
  distinct(eBird.Scientific.Name.2022,SOIB.Concern.Status)
s24 = read.csv("00_data/Myna/S-24.csv") %>%
  distinct(eBird.Scientific.Name.2022,SOIB.Concern.Status)

s24old_diff = s24old %>% anti_join(s24) %>% 
  rename(SOIB.Concern.Status.Old = SOIB.Concern.Status)
s24_diff = s24 %>% anti_join(s24old) %>% 
  rename(SOIB.Concern.Status.New = SOIB.Concern.Status) %>%
  left_join(s24old_diff)

write.csv(s24_diff, file = "00_data/Myna/S-24-diff.csv", row.names = F)

# correct D-23 and Plus-D-23

require(lubridate)
require(tidyverse)

source("00_scripts/00_functions.R")

d24 = read.delim("00_data/Myna/D-24.txt", sep = "\t", header = T)
plusd23 = read.delim("00_data/Myna/Plus-D-23.txt", sep = "\t", header = T)

d24_remove = d24 %>% filter(SCIENTIFIC.NAME %in% s24_diff$eBird.Scientific.Name.2022)
plusd23_remove = plusd23 %>% filter(SCIENTIFIC.NAME %in% s24_diff$eBird.Scientific.Name.2022)



######### differences in all other attributes

require(lubridate)
require(tidyverse)

source("00_scripts/00_functions.R")

tax2023 = read.csv("00_data/SoIB_mapping_2023.csv") %>%
  dplyr::select(-SoIB.Past.Priority.Status,-SoIB.Past.Long.Term.Status,
                -SoIB.Past.Current.Status,-SoIB.Past.Range.Status)

names_tax2023 = tax2023 %>%
  dplyr::select(eBird.English.Name.2023,
                eBird.Scientific.Name.2023)

tax2022 = read.csv("00_data/SoIB_mapping_2022.csv") %>%
  dplyr::select(-SOIB.Concern.Status,-SOIB.Long.Term.Status,
                -SOIB.Current.Status,-SOIB.Range.Status) %>%
  left_join(ebird_tax_mapping()) %>%
  dplyr::select(-eBird.English.Name.2022) %>%
  left_join(names_tax2023) %>%
  dplyr::select(names(tax2023))

tax2023 = tax2023 %>%
  filter(eBird.English.Name.2023 %in% tax2022$eBird.English.Name.2023)

# IUCN

att_old = tax2022 %>%
  distinct(eBird.Scientific.Name.2023,IUCN.Category)
att_new = tax2023 %>%
  distinct(eBird.Scientific.Name.2023,IUCN.Category)
  
att_old_diff = att_old %>% anti_join(att_new) %>% 
  rename(IUCN.Category.Old = IUCN.Category)
att_new_diff = att_new %>% anti_join(att_old) %>% 
  rename(IUCN.Category.New = IUCN.Category) %>%
  left_join(att_old_diff)

IUCN.Category = att_new_diff

# WLPA

att_old = tax2022 %>%
  distinct(eBird.Scientific.Name.2023,WPA.Schedule)
att_new = tax2023 %>%
  distinct(eBird.Scientific.Name.2023,WPA.Schedule)

att_old_diff = att_old %>% anti_join(att_new) %>% 
  rename(WPA.Schedule.Old = WPA.Schedule)
att_new_diff = att_new %>% anti_join(att_old) %>% 
  rename(WPA.Schedule.New = WPA.Schedule) %>%
  left_join(att_old_diff)

WPA.Schedule = att_new_diff

# CITES

att_old = tax2022 %>%
  distinct(eBird.Scientific.Name.2023,CITES.Appendix)
att_new = tax2023 %>%
  distinct(eBird.Scientific.Name.2023,CITES.Appendix)

att_old_diff = att_old %>% anti_join(att_new) %>% 
  rename(CITES.Appendix.Old = CITES.Appendix)
att_new_diff = att_new %>% anti_join(att_old) %>% 
  rename(CITES.Appendix.New = CITES.Appendix) %>%
  left_join(att_old_diff)

CITES.Appendix = att_new_diff

# CMS

att_old = tax2022 %>%
  distinct(eBird.Scientific.Name.2023,CMS.Appendix)
att_new = tax2023 %>%
  distinct(eBird.Scientific.Name.2023,CMS.Appendix)

att_old_diff = att_old %>% anti_join(att_new) %>% 
  rename(CMS.Appendix.Old = CMS.Appendix)
att_new_diff = att_new %>% anti_join(att_old) %>% 
  rename(CMS.Appendix.New = CMS.Appendix) %>%
  left_join(att_old_diff)

CMS.Appendix = att_new_diff

# One percent

att_old = tax2022 %>%
  distinct(eBird.Scientific.Name.2023,Onepercent.Estimates)
att_new = tax2023 %>%
  distinct(eBird.Scientific.Name.2023,Onepercent.Estimates)

att_old_diff = att_old %>% anti_join(att_new) %>% 
  rename(Onepercent.Estimates.Old = Onepercent.Estimates)
att_new_diff = att_new %>% anti_join(att_old) %>% 
  rename(Onepercent.Estimates.New = Onepercent.Estimates) %>%
  left_join(att_old_diff)

Onepercent.Estimates = att_new_diff

# Endemic region

att_old = tax2022 %>%
  distinct(eBird.Scientific.Name.2023,Endemic.Region)
att_new = tax2023 %>%
  distinct(eBird.Scientific.Name.2023,Endemic.Region)

att_old_diff = att_old %>% anti_join(att_new) %>% 
  rename(Endemic.Region.Old = Endemic.Region)
att_new_diff = att_new %>% anti_join(att_old) %>% 
  rename(Endemic.Region.New = Endemic.Region) %>%
  left_join(att_old_diff)

Endemic.Region = att_new_diff


# Changes in Indian Birds 8.2

names_tax2023 = read.csv("00_data/SoIB_mapping_2023.csv") %>%
  dplyr::select(India.Checklist.Scientific.Name,
                eBird.Scientific.Name.2023)

tax2023 = read.csv("00_data/SoIB_mapping_2023.csv") %>%
  dplyr::select(eBird.Scientific.Name.2023,IUCN.Category,
                WPA.Schedule,CITES.Appendix,CMS.Appendix)

checklist_latest = read.csv("00_data/Myna/checklist_8.2.csv") %>%
  left_join(names_tax2023) %>%
  dplyr::select(names(tax2023))

checklist_latest_diff = setdiff(checklist_latest,tax2023)


write.csv(IUCN.Category, file = "00_data/Myna/IUCN.Category.csv", row.names = F)
write.csv(WPA.Schedule, file = "00_data/Myna/WPA.Schedule.csv", row.names = F)
write.csv(CITES.Appendix, file = "00_data/Myna/CITES.Appendix.csv", row.names = F)
write.csv(CMS.Appendix, file = "00_data/Myna/CMS.Appendix.csv", row.names = F)
write.csv(Onepercent.Estimates, file = "00_data/Myna/Onepercent.Estimates.csv", row.names = F)
write.csv(checklist_latest_diff, file = "00_data/Myna/checklist_latest_diff.csv", row.names = F)
