# AWC 2026 Data Intake and Filtering
library(lubridate)

source("config.R")

awc26_data <- read.delim(awc26, sep = "\t", header = T, quote = "", 
  stringsAsFactors = F, na.strings = c(""," ",NA))

# Clean column names
names(awc26_data) <- names(awc26_data) %>%
  gsub("^X\\.", "", .) %>%
  gsub("\\.$", "", .)

# Clean all data
awc26_data <- awc26_data %>%
  mutate(
    across(
      where(is.character),
      ~ gsub('"', '', .x)
    )
  )

########################################
# Retain only CAF priority species
########################################
species_list_ebird <- read.csv(CAF_species_list_ebird_names, 
                         header = T, 
                         stringsAsFactors = F, na.strings = c(""," ",NA))

species_list_ebird <- species_list_ebird %>%
  mutate(
    SCIENTIFIC.NAME =
      tolower(str_trim(SCIENTIFIC.NAME))
  )

awc26_data <- awc26_data %>%
  mutate(
    SCIENTIFIC.NAME =
      tolower(str_trim(SCIENTIFIC.NAME))
  ) %>%
  
  filter(
    SCIENTIFIC.NAME %in%
      species_list$SCIENTIFIC.NAME
  )

# Standard Basic Filters
# Is this only for certain methodology? (retained for now. Will move to species config file)
awc26_data = awc26_data %>% filter(ALL.SPECIES.REPORTED == 1)

# Is this only for certain methodology? (retained for now. Will move to species config file)
awc26_data = awc26_data %>% filter(PROTOCOL.NAME %in% c("Traveling","Stationary"))

# Treating X as 1 observation count
awc26_data <- awc26_data %>%
  mutate(
    OBSERVATION.COUNT = ifelse(OBSERVATION.COUNT == "X","1",OBSERVATION.COUNT),
    OBSERVATION.COUNT = as.numeric(OBSERVATION.COUNT)
  )
# Filter NA fields
awc26_data <- awc26_data %>%
  dplyr::filter(
    !is.na(LATITUDE),
    !is.na(LONGITUDE),
    !is.na(OBSERVATION.COUNT),
    !is.na(SAMPLING.EVENT.IDENTIFIER)
  )

# Create a new column called 'CHECKLIST.ID'
awc26_data$CHECKLIST.ID <- ifelse(
  is.na(awc26_data$`GROUP.IDENTIFIER`),
  awc26_data$`SAMPLING.EVENT.IDENTIFIER`,
  awc26_data$`GROUP.IDENTIFIER`
)

# Remove duplicate checklists. Retain the one with highest observation count.
# Note: SEI can be different for the same checklist ID. Remember
awc26_data <- awc26_data %>%
  arrange(
    CHECKLIST.ID,
    desc(OBSERVATION.COUNT),
    SAMPLING.EVENT.IDENTIFIER
  ) %>%
  group_by(CHECKLIST.ID) %>% #group by species also.
  slice(1) %>%
  ungroup()

# Keep only necessary columns
awc26_data = awc26_data %>% select(COMMON.NAME,SCIENTIFIC.NAME,OBSERVATION.COUNT,STATE,STATE.CODE,COUNTY,COUNTY.CODE,
                                   LOCALITY,LOCALITY.ID,LOCALITY.TYPE,LATITUDE,LONGITUDE,OBSERVATION.DATE,TIME.OBSERVATIONS.STARTED,
                                   OBSERVER.ID,SAMPLING.EVENT.IDENTIFIER,PROJECT.IDENTIFIERS,PROTOCOL.NAME,DURATION.MINUTES,
                                   EFFORT.DISTANCE.KM,NUMBER.OBSERVERS,GROUP.IDENTIFIER,CHECKLIST.ID)

# Add date, month and year columns
awc26_data <- awc26_data %>%
  mutate(
    OBSERVATION.DATE = ymd_hms(OBSERVATION.DATE),
    MONTH = as.character(month(OBSERVATION.DATE, label = TRUE)),
    YEAR = year(OBSERVATION.DATE),
    DAY.OF.YEAR = yday(OBSERVATION.DATE)
  )

# Filter to only actual AWC dates
awc26_data <- awc26_data %>%
  filter(
    OBSERVATION.DATE >= as.POSIXct("2025-12-01") &
    OBSERVATION.DATE <  as.POSIXct("2026-03-01")
  )

View(awc26_data)
