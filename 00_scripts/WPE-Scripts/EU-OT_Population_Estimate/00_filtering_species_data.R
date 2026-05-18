
# STEP 0 — Libraries + Setup
library(dplyr)
library(readr)
library(stringr)
library(purrr)

# STEP 1 — Read your data
library(googlesheets4)
gs4_auth()
species_data <- read_sheet("https://docs.google.com/spreadsheets/d/1pNW5qFOGug1igvMLBS3-101bVYg_iLvpdMNs8suDQrg/edit?usp=sharing", sheet = "species_data")
estimation_units <- read_sheet("https://docs.google.com/spreadsheets/d/1pNW5qFOGug1igvMLBS3-101bVYg_iLvpdMNs8suDQrg/edit?usp=sharing", sheet = "estimation_units")
operation_table <- read_sheet("https://docs.google.com/spreadsheets/d/1pNW5qFOGug1igvMLBS3-101bVYg_iLvpdMNs8suDQrg/edit?usp=sharing", sheet = "operation_table")

#species_data <- read.csv("species_data.csv", header = T, 
#                         stringsAsFactors = F, na.strings = c(""," ",NA))
#estimation_units <- read.csv("estimation_units.csv")
#operation_table <- read.csv("operation_table.csv")

# STEP 2 - Standard Basic Filters
species_data = species_data %>% filter(ALL.SPECIES.REPORTED == TRUE)
species_data = species_data %>% filter(PROTOCOL.NAME %in% c("Traveling","Stationary"))
species_data <- species_data %>%
  mutate(
    OBSERVATION.COUNT = na_if(OBSERVATION.COUNT, "X"),
    OBSERVATION.COUNT = as.numeric(OBSERVATION.COUNT)
  )
# Filter NA fields
species_data <- species_data %>%
  dplyr::filter(
    !is.na(LATITUDE),
    !is.na(LONGITUDE),
    !is.na(OBSERVATION.COUNT),
    !is.na(SAMPLING.EVENT.IDENTIFIER)
  )
# Remove duplicate checklists
species_data$CHECKLIST.ID <- ifelse(
  is.na(species_data$`GROUP.IDENTIFIER`),
  species_data$`SAMPLING.EVENT.IDENTIFIER`,
  species_data$`GROUP.IDENTIFIER`
)
species_data <- species_data %>%
  group_by(CHECKLIST.ID) %>%
  slice_min(order_by = `SAMPLING.EVENT.IDENTIFIER`) %>%
  ungroup()
# Keep only necessary columns
species_data = species_data %>% select(COMMON.NAME,SCIENTIFIC.NAME,OBSERVATION.COUNT,STATE,STATE.CODE,COUNTY,COUNTY.CODE,
                                               LOCALITY,LOCALITY.ID,LOCALITY.TYPE,LATITUDE,LONGITUDE,OBSERVATION.DATE,TIME.OBSERVATIONS.STARTED,
                                               OBSERVER.ID,SAMPLING.EVENT.IDENTIFIER,PROJECT.IDENTIFIERS,PROTOCOL.NAME,DURATION.MINUTES,
                                               EFFORT.DISTANCE.KM,NUMBER.OBSERVERS,GROUP.IDENTIFIER,CHECKLIST.ID)

# Add date, month and year columns
library(lubridate)
species_data <- species_data %>%
  mutate(
    OBSERVATION.DATE = as.Date(OBSERVATION.DATE),
    MONTH = as.character(month(OBSERVATION.DATE, label = TRUE)),
    YEAR = year(OBSERVATION.DATE),
    DAY.OF.YEAR = as.numeric(format(OBSERVATION.DATE, "%j"))
  )
View(species_data)

# Create season year
species_data <- species_data %>%
  mutate(
    MONTH_NUM = match(MONTH, month.abb),
    SEASON_YEAR = ifelse(MONTH_NUM %in% 1:3, YEAR - 1, YEAR)
  )
