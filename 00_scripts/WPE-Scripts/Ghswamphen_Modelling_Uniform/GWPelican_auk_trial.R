#########################################################################
########   Cleaning, Visualizing and Aggregating AWC Bird Data   ########
#########################################################################
install.packages("auk")
install.packages("tidyverse")
install.packages("lubridate")
library(dplyr)      # or library(tidyverse)
library(auk)        # for auk functions
library(lubridate)

f_ebd <- "/home/subhasmitap/Documents/ebd_data - Till May 2025/ebd_IN_unv_smp_relAug-2025.txt"
View(f_ebd)
head(f_ebd)

readLines(f_ebd, n = 1)

# Extracting single species data
species_name <- "Pelecanus onocrotalus"
species_name <- "Rynchops albicollis"

filters <-
  # 1. reference file
  auk_ebd(f_ebd) %>% 
  # 2. define filters
#  auk_complete() %>% 
#  auk_duration(duration = c(0, 300)) %>% 
#  auk_date(date = c("2001-01-01", "2026-01-01")) %>%
  auk_species(species_name) #%>%
 # auk_protocol(protocol = c("Traveling", "Stationary"))
filters

ebd_filtered <- auk_filter(filters, 
                           file = "All_Pelican.txt",
                           overwrite = TRUE)

readLines(ebd_filtered, n = 1)

AllPelican_df <- read_ebd("All_Pelican.txt")
View(AllPelican_df)

#find how many entries in thedate column are not in the correct date format
# try converting to Date
test_dates <- as.Date(AllPelican_df$observation_date)
# count values that failed to convert
sum(is.na(test_dates))
# See the problematic rows
AllPelican_df[is.na(as.Date(AllPelican_df$observation_date)), "observation_date"]
# check what those values actually are
unique(AllPelican_df$observation_date[
  is.na(as.Date(AllPelican_df$observation_date))
])
# make sure date column is Date format
AllPelican_df$observation_date <- as.Date(AllPelican_df$observation_date)
# Count entries for ALL years (useful for quick check)
table(format(as.Date(AllPelican_df$observation_date), "%Y"))
# count entries from 2024
sum(format(AllPelican_df$observation_date, "%Y") == "2024", na.rm = TRUE)



#First: check if it’s actually a problem
library(readr)
problems(GreatWhitePelican_df)
# Confirm the species filter worked
unique(GreatWhitePelican_df$scientific_name)
str(GreatWhitePelican_df$observation_count)
# how much of your data is: counted or presence-only
GreatWhitePelican_df %>%
  count(observation_count)

# Handle observation counts correctly
# NA = present but not counted
# Numbers = real counts
library(dplyr)
GreatWhitePelican_df <- GreatWhitePelican_df %>%
  mutate(
    observation_count = na_if(observation_count, "X"),
    observation_count = as.numeric(observation_count)
  )

#how many usable counts you have
#how many checklists are presence-only
summary(GreatWhitePelican_df$observation_count)
sum(is.na(GreatWhitePelican_df$observation_count))
# Range - lat: 6–38; long: 68–98 (filter if bad data)
summary(GreatWhitePelican_df$latitude)
summary(GreatWhitePelican_df$longitude)
summary(GreatWhitePelican_df)

#Quick Checks
colSums(is.na(GreatWhitePelican_df[, c(
  "latitude",
  "longitude",
  "observation_date",
  "sampling_event_identifier"
)]))

# Cleaning: Filter NA fields
GreatWhitePelican_df_clean <- GreatWhitePelican_df %>%
  dplyr::filter(
    !is.na(latitude),
    !is.na(longitude),
    !is.na(observation_date),
    !is.na(sampling_event_identifier)
  )
View(GreatWhitePelican_df_clean)

########## Filter Season wise
# Step 1: Make sure dates are proper Date objects
GreatWhitePelican_df_clean <- GreatWhitePelican_df_clean %>%
  mutate(observation_date = as.Date(observation_date))
class(GreatWhitePelican_df_clean$observation_date)

# Step2: India-relevant seasonal classification
GreatWhitePelican_df_clean <- GreatWhitePelican_df_clean %>%
  mutate(
    month = month(observation_date),
    season = case_when(
      month %in% c(11, 12, 1, 2)  ~ "Winter",
      month %in% c(3, 4, 5)   ~ "Summer",
      month %in% c(6, 7, 8) ~ "Monsoon",
      month %in% c(9, 10)   ~ "Post-monsoon"
    )
  )

GreatWhitePelican_df_clean %>% count(season)

# Step 3: Filter by a season
# Winter records only
pelican_winter <- GreatWhitePelican_df_clean %>%
  filter(season == "Winter")
# Monsoon records only
pelican_monsoon <- GreatWhitePelican_df_clean %>%
  filter(season %in% c("Monsoon"))
# Post-monsoon records only
pelican_postmonsoon <- GreatWhitePelican_df_clean %>%
  filter(season %in% c("Post-monsoon"))
# Summer records only
pelican_summer <- GreatWhitePelican_df_clean %>%
  filter(season %in% c("Summer"))

sum(pelican_winter$observation_count)

# Method1: Visualizing the data on India map
install.packages("rnaturalearth")
install.packages("rnaturalearthdata")
library(ggplot2)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)

# Get India boundary
india <- ne_countries(
  country = "India",
  scale = "medium",
  returnclass = "sf"
)

# Plot with sf
ggplot() +
  geom_sf(data = india, fill = "gray95", color = "gray40") +
  geom_sf(
    data = pelican_sf,
    color = "darkred",
    alpha = 0.6,
    size = 1
  ) +
  theme_minimal() +
  labs(title = "Great White Pelican records in India")

# Method3 (Amazing visual): Visualizing the data on India map
# Load leaflet
library(leaflet)
# Plot points
leaflet(pelican_winter) %>%
  addTiles() %>%
  addCircleMarkers(
    lng = ~longitude,
    lat = ~latitude,
    radius = 3,
    color = "blue",
    fillOpacity = 0.6,
    popup = ~paste("Date:", observation_date)
  )


#GreatWhitePelican_df