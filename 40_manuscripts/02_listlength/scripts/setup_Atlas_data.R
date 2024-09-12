require(lubridate)
require(tidyverse)

rawpath = "00_data/ebd_IN_relMay-2023.txt"
sensitivepath = "00_data/ebd_sensitive_relMay-2023_IN.txt"
KBA_ListIDs = read.csv("40_manuscripts/02_listlength/KBA_ListIDs.csv")

# select only necessary columns
preimp = c("COMMON.NAME","REVIEWED","APPROVED",
           "OBSERVATION.DATE",
           "EXOTIC.CODE",
           "GROUP.IDENTIFIER","SAMPLING.EVENT.IDENTIFIER")

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

KL_Atlas_data = data %>%
  # create a column "group.id" which can help remove duplicate checklists
  mutate(group.id = ifelse(is.na(GROUP.IDENTIFIER), 
                           SAMPLING.EVENT.IDENTIFIER, GROUP.IDENTIFIER)) %>%
  filter(SAMPLING.EVENT.IDENTIFIER %in% KBA_ListIDs$SAMPLING.EVENT.IDENTIFIER) %>%
  # set date, add month, year and day columns using package LUBRIDATE
  mutate(OBSERVATION.DATE = as.Date(OBSERVATION.DATE), 
         month = month(OBSERVATION.DATE)) %>%
  dplyr::select(-c("OBSERVATION.DATE")) %>%
  filter(month %in% c(1:3)) %>%
  dplyr::select(group.id,COMMON.NAME) %>%
  distinct()

save(KL_Atlas_data, file = "40_manuscripts/02_listlength/KL_Atlas_data.RData")
