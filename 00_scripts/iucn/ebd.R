library (tidyverse)
library (lubridate)
library (data.table)
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
                distinct(COMMON.NAME, GROUP.ID, .keep_all = TRUE)

saveRDS(ebd, "ebd.RDS")

# For EOO, we need data as an SF
ebd_sf <- st_as_sf(ebd, coords = c("LONGITUDE", "LATITUDE"), crs = 4326)
ebd_sf$geometry <- st_make_valid(ebd_sf$geometry)
saveRDS(ebd_sf, "ebd_sf.RDS")





                         