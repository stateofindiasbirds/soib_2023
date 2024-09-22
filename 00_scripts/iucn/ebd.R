library (tidyverse)
library (lubridate)
library (data.table)
library(sf)
source("utils.R")

source("config.R")

# List the interested columns
preimp <-  c( "COMMON.NAME",
              "SAMPLING.EVENT.IDENTIFIER",
              "ALL.SPECIES.REPORTED",
              "GROUP.IDENTIFIER",
              "EFFORT.DISTANCE.KM",
              "PROTOCOL.TYPE",
              "LATITUDE",
              "LONGITUDE",
              "OBSERVATION.DATE",
              "EXOTIC.CODE",
              "APPROVED"
)


#Incase the unzip is not done, uncomment this line
if (unzip)
{
  unzip(paste(dir, ebdfile,'.zip',sep=''))
}

# Read the header plus first row
nms <- read.delim( paste0 (ebdfile,".txt"),
                   nrows = 1, 
                   sep = '\t', 
                   header = T, 
                   quote = "", 
                   stringsAsFactors = F, 
                   na.strings = c ("", " ",NA)) 
nms <- names(nms)
nms [!(nms %in% preimp)] <- "NULL"
nms [nms %in% preimp] <- NA

ebd <- read.delim(paste0(ebdfile,".txt"),
                  colClasses = nms,
                  #                  nrows = 100000, # For testing, this is useful
                  sep = '\t', 
                  header = T, 
                  quote = "", 
                  stringsAsFactors = F, 
                  na.strings = c ("", " ",NA)) 

ebd <- ebd %>% 
            mutate (
                      OBSERVATION.DATE = as.Date(OBSERVATION.DATE),
                      YEAR  = year(OBSERVATION.DATE),
                      MONTH = month(OBSERVATION.DATE),
                      DAY   = month(OBSERVATION.DATE),
                      SOIB_YEAR =  ifelse (MONTH <=5, YEAR-1, YEAR),
                      GROUP.ID = ifelse (is.na(GROUP.IDENTIFIER), 
                                        SAMPLING.EVENT.IDENTIFIER,
                                        GROUP.IDENTIFIER)) %>%
                filter ( is.na(EXOTIC.CODE) | (EXOTIC.CODE != 'P' & EXOTIC.CODE != 'X'), APPROVED == 1) %>%
                filter (SOIB_YEAR <= maxYearforProcessing) %>%
                distinct(COMMON.NAME, GROUP.ID, .keep_all = TRUE)

saveRDS(ebd, "ebd.RDS")

# Vagrant removal step

species <- ebd$COMMON.NAME %>% unique() %>% grep(pattern = "[()/\\\\.]", value = TRUE, invert = TRUE)

filterspecies <- read.csv("species.csv")
filterspecies <- left_join (data.frame(Species = species), filterspecies)
filterspecies$Status[is.na(filterspecies$Status)] <- "BREEDING ONLY"

# Remove vagrant species first
ebd <- ebd %>%
            left_join (filterspecies, by = c("COMMON.NAME" = "Species")) %>%
            filter (Status != 'Not Assessed')

# Remove vagrant records of visiting species by finding species, month and cell where its a vagrant
ebd_v <- ebd %>%  
          filter(SOIB_YEAR >= 2015, COMMON.NAME %in% species, Status == "VISITING ONLY") %>%
          mutate(LATITUDE_200 = floor(LATITUDE / 1.8) * 1.8,
                 LONGITUDE_200 = floor(LONGITUDE / 1.8) * 1.8) %>%
          group_by(COMMON.NAME, LATITUDE_200, LONGITUDE_200, SOIB_YEAR, MONTH) %>%
          mutate(cell_id = paste0(round(LATITUDE_200, 1), "_", round(LONGITUDE_200, 1))) %>%
          ungroup() %>%
          group_by(COMMON.NAME, cell_id, MONTH) %>%
          summarize(year_count = n_distinct(SOIB_YEAR), .groups = 'drop') %>%
          filter(year_count <= 3) %>%
          select(COMMON.NAME, cell_id, MONTH)

# Exclude the vagrant records from the original dataset by removing records from those cells
ebd   <- ebd %>%
            mutate(LATITUDE_200 = floor(LATITUDE / 1.8) * 1.8,
                   LONGITUDE_200 = floor(LONGITUDE / 1.8) * 1.8,
                   cell_id = paste0(round(LATITUDE_200, 1), "_", round(LONGITUDE_200, 1))) %>%
            anti_join(ebd_v, by = c("COMMON.NAME", "cell_id", "MONTH")) %>%
            # Remove the 'cell_id' column as it's not needed in the final dataset
            select(-cell_id, -LATITUDE_200, -LONGITUDE_200)
# This process can result in a few list loss. If list had only vagrants to that cell. Not a useful list anyways.

# For EOO, we need data as an SF
ebd_sf <- st_as_sf(ebd, coords = c("LONGITUDE", "LATITUDE"), crs = 4326)
ebd_sf$geometry <- st_make_valid(ebd_sf$geometry)
saveRDS(ebd_sf, "ebd_sf.RDS")





                         