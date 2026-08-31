library(readxl)
library(dplyr)
library(stringr)
library(measurements)
library(sf)

source("config.R")
awc20_25_data <- read_excel(awc, sheet = "AWC 2006-25", col_types = "text", na = c("", " "))

########################################
# Filtering AWC 2020 - 2025 data
########################################

awc20_25_data <- awc20_25_data %>%
  rename(
    LATITUDE = Latitude,
    LONGITUDE = Longitude,
    OBSERVATION.COUNT = COUNT,
    COMMON.NAME = COMMONNAME,
    SCIENTIFIC.NAME = SPECIESNAME,
    LOCALITY = SITENAME
  ) %>%
  
# Convert to numeric
  mutate(
    YEAR = as.numeric(YEAR),
    OBSERVATION.COUNT = as.numeric(OBSERVATION.COUNT)
  ) %>%
  
# Apply filters
# 1. Keep only selected sources
filter(
  Source %in% c(
    "AWC",
    "SBB",
    "NBA",
    "Others"
  )
) %>%

# 2. Keep only years 2020–2025
filter(
  YEAR >= 2020 &
    YEAR <= 2025
) %>%

# 3. Remove missing coordinates
filter(
  !is.na(LATITUDE),
  !is.na(LONGITUDE),
  
  LATITUDE != "",
  LONGITUDE != ""
) %>%
  
# 4. Remove missing observation counts
filter(
  !is.na(OBSERVATION.COUNT),
  OBSERVATION.COUNT != ""
)

########################################
# Retain only CAF priority species
########################################

species_list <- read.csv(CAF_species_list, 
                         header = T, 
                         stringsAsFactors = F, na.strings = c(""," ",NA))

species_list <- species_list %>%
  mutate(
    SCIENTIFIC.NAME =
      tolower(str_trim(SCIENTIFIC.NAME))
  )

awc20_25_data <- awc20_25_data %>%
  mutate(
    SCIENTIFIC.NAME =
      tolower(str_trim(SCIENTIFIC.NAME))
    ) %>%
      
  filter(
    SCIENTIFIC.NAME %in%
      species_list$SCIENTIFIC.NAME
  )


########################################
# Fix MONTH column
########################################

awc20_25_data <- awc20_25_data %>%
  
  mutate(
    # Convert to character
    MONTH = as.character(MONTH),

    # Trim spaces
    MONTH = str_trim(MONTH),
    
    # Convert to lowercase
    MONTH = str_to_lower(MONTH),
    
    # Standardize month values
    MONTH = case_when(
      MONTH %in% c("0", "00", "1", "01", "jan", "january") ~ "Jan",
      MONTH %in% c("2", "02", "feb", "february") ~ "Feb",
      MONTH %in% c("3", "03", "mar", "march") ~ "Mar",
      MONTH %in% c("4", "04", "apr", "april") ~ "Apr",
      MONTH %in% c("5", "05", "may") ~ "May",
      MONTH %in% c("6", "06", "jun", "june") ~ "Jun",
      MONTH %in% c("7", "07", "jul", "july") ~ "Jul",
      MONTH %in% c("8", "08", "aug", "august") ~ "Aug",
      MONTH %in% c("9", "09", "sep", "september") ~ "Sep",
      MONTH %in% c("10", "oct", "october") ~ "Oct",
      MONTH %in% c("11", "nov", "november") ~ "Nov",
      MONTH %in% c("12", "dec", "december") ~ "Dec",
      
      # Unknown values
      TRUE ~ NA_character_
    )
  )

########################################
# Fix Latitude and Longitude
########################################

convert_coord <- function(x) {
  
  # Convert to character
  x <- as.character(x)
  
  # Trim spaces
  x <- str_trim(x)
  
  # Empty strings -> NA
  x[x == ""] <- NA
  
  # Output vector
  out <- rep(NA_real_, length(x))
  
  # Loop through values
  for(i in seq_along(x)) {
    val <- x[i]
    if(is.na(val)) next
    
    # Standardize text
    val2 <- toupper(val)
    val2 <- gsub("°", " ", val2)
    val2 <- gsub("'", " ", val2)
    val2 <- gsub("\"", " ", val2)
    val2 <- gsub("[A-Z]", " ", val2)
    val2 <- gsub("\\s+", " ", val2)
    val2 <- str_trim(val2)
    
    # Split components
    parts <- unlist(strsplit(val2, " "))
    nums <- suppressWarnings(as.numeric(parts))
    nums <- nums[!is.na(nums)]
    
    # Decimal degrees
    if(length(nums) == 1) {
      dec <- nums[1]
    }
    
    # Degree-minute
    else if(length(nums) == 2) {
      dec <- nums[1] + nums[2]/60
    }
    
    # Degree-minute-second
    else if(length(nums) >= 3) {
      dec <- nums[1] +
        nums[2]/60 +
        nums[3]/3600
    }
    else {
      dec <- NA
    }
    
    # South/West negative
    if(grepl("[SW]", toupper(val))) {
      dec <- -abs(dec)
    }
    out[i] <- dec
  }
  return(out)
}

awc20_25_data <- awc20_25_data %>%
  
  # Convert coordinates to character
  mutate(
    LATITUDE = as.character(LATITUDE),
    LONGITUDE = as.character(LONGITUDE)
  ) %>%
  
  # Remove blanks
  filter(
    !is.na(LATITUDE),
    !is.na(LONGITUDE),
    
    str_trim(LATITUDE) != "",
    str_trim(LONGITUDE) != ""
  ) %>%
  
  # Convert coordinates
  mutate(
    LAT_DECIMAL = convert_coord(LATITUDE),
    LON_DECIMAL = convert_coord(LONGITUDE)
  ) %>%
  
  # Remove failed conversions
  filter(
    !is.na(LAT_DECIMAL),
    !is.na(LON_DECIMAL)
  ) %>%
  
  # Keep only India coordinates
  filter(
    LAT_DECIMAL >= 6 &
      LAT_DECIMAL <= 38,
    
    LON_DECIMAL >= 68 &
      LON_DECIMAL <= 98
  ) %>%
  
  # Replace original columns
  mutate(
    LATITUDE = LAT_DECIMAL,
    LONGITUDE = LON_DECIMAL
  ) %>%
  select(
    -LAT_DECIMAL,
    -LON_DECIMAL
  )

######################################################
# Extracting District and State Codes from Shape file
######################################################

# Load the district shapefile object
load(district_shapefile)
# check object name
ls()
names(dists_sf)


# Convert AWC data into spatial points
awc_points <- st_as_sf(
  awc20_25_data,
  coords = c(
    "LONGITUDE",
    "LATITUDE"
  ),
  crs = 4326,
  remove = FALSE
)

# Ensure shapefile CRS matches
st_crs(awc_points)
st_crs(dists_sf)

# If they dont match
dists_sf <- st_transform(
  dists_sf,
  st_crs(awc_points)
)

# Spatial join
awc_joined <- st_join(
  awc_points,
  dists_sf %>%
    select(
      STATE.NAME,
      DISTRICT.NAME
    ),
  left = TRUE
)

# Convert back to data frame
awc20_25_data <- awc_joined %>%
  st_drop_geometry()

########################################
# Populate STATE.CODE AND COUNTY.CODE
########################################
region_codes <- read.csv(all_region_codes,
                         header = T, 
                         stringsAsFactors = F, na.strings = c(""," ",NA))

# Clean names in AWC data
awc20_25_data <- awc20_25_data %>%
  rename(
    STATE = STATE.NAME,
    COUNTY = DISTRICT.NAME
  ) %>%
  
  mutate(
    STATE = str_to_upper(
      str_trim(STATE)
    ),
    COUNTY = str_to_upper(
      str_trim(COUNTY)
    )
  )

# Clean names in lookup table
region_codes <- region_codes %>%
  
  mutate(
    STATE = str_to_upper(
      str_trim(STATE)
    ),
    COUNTY = str_to_upper(
      str_trim(COUNTY)
    )
  )

# Join lookup table
awc20_25_data <- awc20_25_data %>%
  left_join(
    region_codes %>%
      select(
        STATE,
        COUNTY,
        STATE.CODE,
        COUNTY.CODE
      ),
    
    by = c(
      "STATE",
      "COUNTY"
    )
  )

# Filter NA fields
awc20_25_data <- awc20_25_data %>%
  dplyr::filter(
    !is.na(STATE.CODE),
    !is.na(COUNTY.CODE)
  )

View(awc20_25_data)
