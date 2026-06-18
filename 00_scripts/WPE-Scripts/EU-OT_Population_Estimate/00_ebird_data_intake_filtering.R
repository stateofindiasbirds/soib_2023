library(dplyr)
library(readr)
library(stringr)
library(purrr)
library(lubridate)

source("config.R")
#source("species_config.R") --> will add this later

#################################################
# STEP 1 - Read eBird data
#################################################

ebird_data <- read.csv(
  ebird,
  header = TRUE,
  stringsAsFactors = FALSE,
  na.strings = c("", " ", NA)
)

#################################################
# Standardize eBird dates
#################################################

if(grepl("-", ebird_data$OBSERVATION.DATE[1])) {
  
  ebird_data <- ebird_data %>%
    mutate(
      OBSERVATION.DATE = ymd(OBSERVATION.DATE)
    )
  
} else {
  
  ebird_data <- ebird_data %>%
    mutate(
      OBSERVATION.DATE = mdy(OBSERVATION.DATE)
    )
}

#################################################
# Read sensitive species file IF IT EXISTS
#################################################

if(file.exists(sensitive)) {
  
  message("Sensitive species file found. Reading data.")
  sensitive_sp <- read.delim(
    sensitive,
    sep = "\t",
    header = TRUE,
    quote = "",
    stringsAsFactors = FALSE,
    na.strings = c("", " ", NA)
  )
  
  ##############################################################
  # Standardize sensitive species dates and logical columns
  # Filter to required dates
  ##############################################################
  
  sensitive_sp <- sensitive_sp %>%
    mutate(OBSERVATION.DATE = ymd(OBSERVATION.DATE),
           ALL.SPECIES.REPORTED = as.logical(ALL.SPECIES.REPORTED),
           HAS.MEDIA            = as.logical(HAS.MEDIA),
           APPROVED             = as.logical(APPROVED),
           REVIEWED             = as.logical(REVIEWED),
           EXOTIC.CODE          = as.character(EXOTIC.CODE)) %>%
    filter(
        OBSERVATION.DATE >= as.Date("2020-01-01") &
        OBSERVATION.DATE <  as.Date("2026-05-01"))
  
  #################################################
  # NEW: Match columns before merging
  #################################################
  
  common_cols <- intersect(
    names(ebird_data),
    names(sensitive_sp)
  )
  
  ebird_data <- bind_rows(
    ebird_data[, common_cols],
    sensitive_sp[, common_cols]
  )
  
  message("Sensitive species data merged successfully.")
  
} else {
  
  message("Sensitive species file not found. Using eBird data only.")
  
}

##################################
# STEP 2 - Standard Basic Filters
##################################

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
ebird_data <- ebird_data %>%
  mutate(
    MONTH = as.character(month(OBSERVATION.DATE, label = TRUE)),
    YEAR = year(OBSERVATION.DATE),
    DAY.OF.YEAR = as.numeric(format(OBSERVATION.DATE, "%j"))
  )
View(ebird_data)


