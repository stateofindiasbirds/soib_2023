library(dplyr)
library(readr)
library(stringr)
library(purrr)
library(lubridate)

source("config.R")
# source("species_config.R")

#################################################
# STEP 1 - Read eBird data
# Read EVERYTHING as character
#################################################

ebird_data <- read.csv(
  ebird,
  header = TRUE,
  stringsAsFactors = FALSE,
  colClasses = "character",
  na.strings = c("", " ", NA)
)

#################################################
# Required CAF bird list (for filtering later)
#################################################

species_list_ebird <- read.csv(CAF_species_list_ebird_names, 
                               header = T, 
                               stringsAsFactors = F, na.strings = c(""," ",NA))

species_list_ebird <- species_list_ebird %>%
  mutate(SCIENTIFIC.NAME = tolower(str_trim(SCIENTIFIC.NAME)))

####################################################
# STEP 2 - Read sensitive species data IF IT EXISTS
# Read EVERYTHING as character
####################################################

if(file.exists(sensitive)) {
  
  message("Sensitive species file found. Reading data.")
  
  sensitive_sp <- read.delim(
    sensitive,
    sep = "\t",
    header = TRUE,
    quote = "",
    stringsAsFactors = FALSE,
    colClasses = "character",
    na.strings = c("", " ", NA)
  )
  
  #################################################
  # Keep India records and CAF species only
  #################################################
  
  sensitive_sp <- sensitive_sp %>%
    
    filter(COUNTRY.CODE == "IN") %>%
    
    mutate(SCIENTIFIC.NAME =
             tolower(str_trim(SCIENTIFIC.NAME))
    ) %>%
    
    filter(SCIENTIFIC.NAME %in%
             species_list_ebird$SCIENTIFIC.NAME
    )
  
  #################################################
  # Match columns before merging
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

#################################################
# STEP 3 - Standardize datatypes AFTER merging
#################################################

# -------------------------------
# Standardize dates
# Handles both:
# 2020-01-01
# 01/01/2020
# -------------------------------

ebird_data <- ebird_data %>%
  mutate(OBSERVATION.DATE = parse_date_time(
    OBSERVATION.DATE,
    orders = c("ymd", "mdy")
  ) %>%
    as.Date()
  )

# -------------------------------
# Filter to required date range
# -------------------------------

ebird_data <- ebird_data %>%
  filter(OBSERVATION.DATE >= as.Date("2019-11-01") &
           OBSERVATION.DATE < as.Date("2026-05-01"))

# -------------------------------
# Convert logical columns
# -------------------------------

logical_cols <- c(
  "ALL.SPECIES.REPORTED",
  "HAS.MEDIA",
  "APPROVED",
  "REVIEWED"
)

logical_cols <- logical_cols[
  logical_cols %in% names(ebird_data)
]

ebird_data[logical_cols] <- lapply(
  ebird_data[logical_cols],
  function(x) {
    toupper(trimws(x)) %in% c(
      "TRUE",
      "T",
      "1"
    )
  }
)

# -------------------------------
# Convert numeric columns
# -------------------------------

ebird_data <- ebird_data %>%
  mutate(
    LATITUDE = as.numeric(LATITUDE),
    LONGITUDE = as.numeric(LONGITUDE),
    DURATION.MINUTES = as.numeric(DURATION.MINUTES),
    EFFORT.DISTANCE.KM = as.numeric(EFFORT.DISTANCE.KM),
    NUMBER.OBSERVERS = as.numeric(NUMBER.OBSERVERS)
  )

# -------------------------------
# Treat 'X' as 1 observation count
# -------------------------------

ebird_data <- ebird_data %>%
  mutate(
    OBSERVATION.COUNT = ifelse(OBSERVATION.COUNT == "X", "1", OBSERVATION.COUNT),
    OBSERVATION.COUNT = as.numeric(OBSERVATION.COUNT)
  )

#################################################
# STEP 4 - Standard Basic Filters
#################################################
# Is this only for certain methodology? (retained for now. Will move to species config file)
# Complete checklists only
#ebird_data <- ebird_data %>%
#  filter(ALL.SPECIES.REPORTED == TRUE)

# Traveling and Stationary protocols only
# ebird_data <- ebird_data %>%
#  filter(PROTOCOL.NAME %in% c("Traveling", "Stationary"))

#################################################
# STEP 5 - Remove rows with missing values
#################################################

ebird_data <- ebird_data %>%
  filter(
    !is.na(LATITUDE),
    !is.na(LONGITUDE),
    !is.na(OBSERVATION.COUNT),
    !is.na(SAMPLING.EVENT.IDENTIFIER)
  )

#####################################################
# STEP 6 - Create a new column called 'CHECKLIST.ID'
#####################################################

ebird_data$CHECKLIST.ID <- ifelse(
  is.na(ebird_data$GROUP.IDENTIFIER) |
    ebird_data$GROUP.IDENTIFIER == "",
  ebird_data$SAMPLING.EVENT.IDENTIFIER,
  ebird_data$GROUP.IDENTIFIER
)

#################################################
# STEP 7 - Remove duplicate checklists
# Retain the one with highest observation count.
# Note: SEI can be different for the same checklist ID. Remember for future analysis
#################################################

ebird_data <- ebird_data %>%
  arrange(
    CHECKLIST.ID,
    desc(OBSERVATION.COUNT),
    SAMPLING.EVENT.IDENTIFIER
  ) %>%
  group_by(CHECKLIST.ID, SCIENTIFIC.NAME) %>%
  slice(1) %>%
  ungroup()

#################################################
# STEP 8 - Keep only required columns
#################################################

ebird_data <- ebird_data %>%
  select(
    COMMON.NAME,
    SCIENTIFIC.NAME,
    OBSERVATION.COUNT,
    STATE,
    STATE.CODE,
    COUNTY,
    COUNTY.CODE,
    LOCALITY,
    LOCALITY.ID,
    LOCALITY.TYPE,
    LATITUDE,
    LONGITUDE,
    OBSERVATION.DATE,
    TIME.OBSERVATIONS.STARTED,
    OBSERVER.ID,
    SAMPLING.EVENT.IDENTIFIER,
    PROJECT.IDENTIFIERS,
    PROTOCOL.NAME,
    DURATION.MINUTES,
    EFFORT.DISTANCE.KM,
    NUMBER.OBSERVERS,
    GROUP.IDENTIFIER,
    CHECKLIST.ID
  )

###################################################
# STEP 9 - Add month, year and day of year columns
###################################################

ebird_data <- ebird_data %>%
  mutate(
    MONTH = as.character(month(OBSERVATION.DATE,label = TRUE)),
    YEAR = year(OBSERVATION.DATE),
    DAY.OF.YEAR = yday(OBSERVATION.DATE)
  )

#################################################
# Final output
#################################################

View(ebird_data)
