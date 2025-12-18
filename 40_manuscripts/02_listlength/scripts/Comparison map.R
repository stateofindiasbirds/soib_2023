require(lubridate)
require(tidyverse)
require(sp)
require(ggfortify)
require(sf)
require(mapview)
require(leaflet)
require(rmapshaper)

cols <- c("#869B27", "#E49B36", "#436b74", "#CC6666", 
          "#B69AC9", "#319cc0","#31954E","#493F3D",
          "#EA5599", "#9999CC", "#A13E2B", "#66CC99")

load("00_data/maps_sf.RData")

rawpath = "00_data/ebd_IN_unv_smp_relAug-2025.txt"
sensitivepath = "00_data/ebd_sensitive_relAug-2025_IN.txt"

preimp = c("CATEGORY","COMMON.NAME","SCIENTIFIC.NAME","OBSERVATION.COUNT",
           "LOCALITY.ID","LOCALITY.TYPE","REVIEWED","APPROVED","STATE","COUNTY",
           "LATITUDE","LONGITUDE","OBSERVATION.DATE","TIME.OBSERVATIONS.STARTED","OBSERVER.ID",
           "PROTOCOL.NAME","DURATION.MINUTES","EFFORT.DISTANCE.KM","EXOTIC.CODE",
           "NUMBER.OBSERVERS","ALL.SPECIES.REPORTED","GROUP.IDENTIFIER","SAMPLING.EVENT.IDENTIFIER")

# CATEGORY - species, subspecies, hybrid, etc.; COMMON.NAME - common name of species;
# SCIENTIFIC NAME - scientific name; OBSERVATION.COUNT - count of each species observed in a list;
# LOCALITY.ID - unique location ID; LOCALITY.TYPE - hotspot, etc.;
# LATITUDE and LONGITUDE - coordinates; OBSERVATION.DATE - checklist date; 
# TIME.OBSERVATIONS.STARTED - checklist start time; OBSERVER ID - unique observer ID;
# PROTOCOL TYPE - stationary, traveling, historical, etc.; DURATION.MINUTES - checklist duration;
# EFFORT.DISTANCE.KM - distance traveled; NUMBER.OBSERVERS - no. of birders;
# ALL.SPECIES.REPORTED - indicates whether a checklist is complete or not;
# GROUP.IDENTIFIER - unique ID for every set of shared checklists (NA when not shared);
# SAMPLING.EVENT.IDENTIFIER - unique checlist ID

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


## choosing important columns required for further analyses

imp = c("CATEGORY","COMMON.NAME","SCIENTIFIC.NAME","OBSERVATION.COUNT",
        "LOCALITY.ID", "REVIEWED","APPROVED","EXOTIC.CODE",
        "LOCALITY.TYPE","STATE","COUNTY",
        "LATITUDE","LONGITUDE","OBSERVATION.DATE","TIME.OBSERVATIONS.STARTED",
        "OBSERVER.ID","PROTOCOL.NAME",
        "DURATION.MINUTES","EFFORT.DISTANCE.KM",
        "ALL.SPECIES.REPORTED","group.id","SAMPLING.EVENT.IDENTIFIER")


# no of days in every month, and cumulative number
days = c(31,28,31,30,31,30,31,31,30,31,30,31)
cdays = c(0,31,59,90,120,151,181,212,243,273,304,334)

data = data %>%
  # create a column "group.id" which can help remove duplicate checklists
  mutate(group.id = ifelse(is.na(GROUP.IDENTIFIER), 
                           SAMPLING.EVENT.IDENTIFIER, GROUP.IDENTIFIER)) %>%
  dplyr::select(all_of(imp)) %>%
  # other useful columns
  # set date, add month, year and day columns using package LUBRIDATE
  mutate(OBSERVATION.DATE = as.Date(OBSERVATION.DATE), 
         month = month(OBSERVATION.DATE),
         day = day(OBSERVATION.DATE) + cdays[month], 
         #week = week(OBSERVATION.DATE),
         #fort = ceiling(day/14),
         cyear = year(OBSERVATION.DATE)) %>%
  dplyr::select(-c("OBSERVATION.DATE")) %>%
  mutate(year = ifelse(month > 5, cyear, cyear-1)) %>% # from June to May
  # add number of species/list length column (no.sp), for list length analyses (lla)
  group_by(group.id) %>% 
  mutate(no.sp = n_distinct(COMMON.NAME)) %>%
  ungroup()

data = data %>%
  filter(STATE == "Kerala")

KL_atlas_info = read.csv("40_manuscripts/02_listlength/KBA_ListIDs.csv")

KL_atlas = data %>%
  filter(SAMPLING.EVENT.IDENTIFIER %in% KL_atlas_info$SAMPLING.EVENT.IDENTIFIER) %>%
  filter(month %in% c(1:3)) %>%
  mutate(type = "Atlas") %>%
  distinct(type,LATITUDE,LONGITUDE,group.id)

KL_non_atlas = data %>%
  filter(!group.id %in% KL_atlas$group.id) %>%
  filter(month %in% c(1:3), year %in% c(2015:2019)) %>%
  mutate(type = "General eBirding") %>%
  distinct(type,LATITUDE,LONGITUDE)

KL_data = KL_atlas %>%
  distinct(type,LATITUDE,LONGITUDE) %>%
  bind_rows(KL_non_atlas)


kl_map = dists_sf %>%
  filter(STATE.NAME %in% c("Kerala"))

KL_data$type = factor(KL_data$type, levels = c("General eBirding","Atlas"))

ggp = ggplot(KL_data) +
  facet_wrap(~type) +
  geom_point(data = KL_data, aes(x=LONGITUDE,y=LATITUDE),
             colour = cols[11], size = 0.1) +
  geom_sf(data = kl_map, colour = "black", fill = NA)


ggp1 = ggp +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  #theme(text=element_text(family="Gill Sans MT")) +
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.margin=unit(c(0,0,0,0), "cm"),
        panel.border = element_blank(),
        plot.title = element_text(hjust = 0.5),
        panel.background = element_blank())+
  theme(strip.text = element_blank())+
  coord_sf()



n1 = paste("40_manuscripts/02_listlength/Sampling - atlas vs. general.jpg",sep="")

print(ggp1)
ggsave(file=n1, units="in", width=8, height=6)
