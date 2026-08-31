# Merge all data sources
library(dplyr)
library(lubridate)

# STEP 1 — Add DATA.SOURCE column
ebird_data <- ebird_data %>%
  mutate(DATA.SOURCE = "ebird")

awc26_data <- awc26_data %>%
  mutate(DATA.SOURCE = "awc26")

awc20_25_data <- awc20_25_data %>%
  mutate(DATA.SOURCE = "awc")

# STEP 2 — Keep only required columns from AWC20_25
awc20_25_subset <- awc20_25_data %>%
  select(
    COMMON.NAME,SCIENTIFIC.NAME,
    LATITUDE,LONGITUDE,
    COUNTY,COUNTY.CODE,
    STATE,STATE.CODE,
    OBSERVATION.COUNT,
    LOCALITY,
    MONTH,YEAR,
    DATA.SOURCE
  )

# STEP 3 — Find all columns in eBird dataset
all_cols <- names(ebird_data)

# STEP 4 — Add missing columns to awc20_25_subset
missing_cols <- setdiff(
  all_cols,
  names(awc20_25_subset)
)

# Add missing columns as NA
awc20_25_subset[missing_cols] <- NA

# STEP 5 — Reorder columns to match eBird
awc20_25_subset <- awc20_25_subset %>%
  select(all_of(all_cols))

# STEP 6 — Ensure awc26 has same columns
# Add missing columns to awc26 if needed
missing_cols_awc26 <- setdiff(
  all_cols,
  names(awc26_data)
)

awc26_data[missing_cols_awc26] <- NA

# Reorder awc26 columns
awc26_data <- awc26_data %>%
  select(all_of(all_cols))

# STEP 6.5 — Standardize column types
# Convert ALL columns to character
ebird_data <- ebird_data %>%
  mutate(across(everything(),as.character))

awc26_data <- awc26_data %>%
  mutate(across(everything(),as.character))

awc20_25_subset <- awc20_25_subset %>%
  mutate(across(everything(),as.character))


# STEP 7 — Merge all datasets
merged_data <- bind_rows(
  ebird_data,
  awc26_data,
  awc20_25_subset
)

# STEP 8 — Optional QC checks
cat(
  "Total merged rows:",
  nrow(merged_data),
  "\n"
)

table(merged_data$DATA.SOURCE)


# Make all names lower case
merged_data <- merged_data %>%
  mutate(
    COMMON.NAME = str_to_lower(str_trim(COMMON.NAME)),
    SCIENTIFIC.NAME = str_to_lower(str_trim(SCIENTIFIC.NAME)),
    DATA.SOURCE =str_to_lower(str_trim(DATA.SOURCE))
      )

# Convert columns back to appropriate data types
merged_data <- merged_data %>%
  mutate(
    OBSERVATION.COUNT = as.numeric(OBSERVATION.COUNT),
    LATITUDE = as.numeric(LATITUDE),
    LONGITUDE = as.numeric(LONGITUDE),
    DURATION.MINUTES = as.numeric(DURATION.MINUTES),
    EFFORT.DISTANCE.KM = as.numeric(EFFORT.DISTANCE.KM),
    NUMBER.OBSERVERS = as.numeric(NUMBER.OBSERVERS),
    YEAR = as.numeric(YEAR),
    DAY.OF.YEAR = as.numeric(DAY.OF.YEAR),
    OBSERVATION.DATE = suppressWarnings(ymd_hms(OBSERVATION.DATE))
  )


# Create season year
# Species specific configuration - eBirdst - seasonal relative abundance to be used
# PJ: Why 3, should it not be 5 ? Is this same as SoIB year June to May ? 
merged_data <- merged_data %>%
  mutate(
    MONTH.NUM = match(MONTH, month.abb),
    SEASON.YEAR = ifelse(MONTH.NUM %in% 1:3, YEAR - 1, YEAR)
  )
