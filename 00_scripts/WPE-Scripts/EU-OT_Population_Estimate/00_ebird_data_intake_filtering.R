
library(dplyr)
library(readr)
library(stringr)
library(purrr)
source("config.R")
#source("species_config.R") --> will add this later

# STEP 1 - Read your data
ebird_data <- read.csv(ebird, header = T, 
                         stringsAsFactors = F, na.strings = c(""," ",NA))

# STEP 2 - Standard Basic Filters
# Is this only for certain methodology? (retained for now. Will move to species config file)
ebird_data = ebird_data %>% filter(ALL.SPECIES.REPORTED == TRUE)

# Is this only for certain methodology? (retained for now. Will move to species config file)
ebird_data = ebird_data %>% filter(PROTOCOL.NAME %in% c("Traveling","Stationary"))

# Treating 'X' as 1 observation count
ebird_data <- ebird_data %>%
  mutate(
    OBSERVATION.COUNT = ifelse(OBSERVATION.COUNT == "X","1",OBSERVATION.COUNT),
    OBSERVATION.COUNT = as.numeric(OBSERVATION.COUNT)
  )

# Filter NA fields
ebird_data <- ebird_data %>%
  dplyr::filter(
    !is.na(LATITUDE),
    !is.na(LONGITUDE),
    !is.na(OBSERVATION.COUNT),
    !is.na(SAMPLING.EVENT.IDENTIFIER)
  )

# Create a new column called 'CHECKLIST.ID'
ebird_data$CHECKLIST.ID <- ifelse(
  is.na(ebird_data$`GROUP.IDENTIFIER`),
  ebird_data$`SAMPLING.EVENT.IDENTIFIER`,
  ebird_data$`GROUP.IDENTIFIER`
)

# Remove duplicate checklists. Retain the one with highest observation count.
# Note: SEI can be different for the same checklist ID. Remember for future analysis
ebird_data <- ebird_data %>%
  arrange(
    CHECKLIST.ID,
    desc(OBSERVATION.COUNT),
    SAMPLING.EVENT.IDENTIFIER
  ) %>%
  group_by(CHECKLIST.ID) %>% #group by species also.
  slice(1) %>%
  ungroup()

# Keep only necessary columns
ebird_data = ebird_data %>% select(COMMON.NAME,SCIENTIFIC.NAME,OBSERVATION.COUNT,STATE,STATE.CODE,COUNTY,COUNTY.CODE,
                                               LOCALITY,LOCALITY.ID,LOCALITY.TYPE,LATITUDE,LONGITUDE,OBSERVATION.DATE,TIME.OBSERVATIONS.STARTED,
                                               OBSERVER.ID,SAMPLING.EVENT.IDENTIFIER,PROJECT.IDENTIFIERS,PROTOCOL.NAME,DURATION.MINUTES,
                                               EFFORT.DISTANCE.KM,NUMBER.OBSERVERS,GROUP.IDENTIFIER,CHECKLIST.ID)

# Add date, month and year columns
library(lubridate)
ebird_data <- ebird_data %>%
  mutate(
    OBSERVATION.DATE = mdy(OBSERVATION.DATE),
    MONTH = as.character(month(OBSERVATION.DATE, label = TRUE)),
    YEAR = year(OBSERVATION.DATE),
    DAY.OF.YEAR = as.numeric(format(OBSERVATION.DATE, "%j"))
  )
View(ebird_data)


