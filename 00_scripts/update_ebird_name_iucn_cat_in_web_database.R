
# Website needs need database for update IUCN status - website database file

library(tidyverse)

# Updating the IUCN status in the website database ----

database_2024 <- read.csv("20_website/website_database_2024 - Copy.csv") 
# This is the file used to maintain the SoIB website

taxmap <- read.csv("00_data/eBird_taxonomy_mapping.csv")

map_2022 = read.csv("00_data/SoIB_mapping_2022.csv")
map_2023 = read.csv("00_data/SoIB_mapping_2023.csv")
map_2024 = read.csv("00_data/SoIB_mapping_2024.csv")

# Checking if database scientific names come from eBird ----
diff_2022 <- setdiff(database_2024$scientific_name, map_2022$eBird.Scientific.Name.2022)
length(diff_2022)
# 6

diff_2023 <- setdiff(database_2024$scientific_name, map_2023$eBird.Scientific.Name.2023)
length(diff_2023)
# 16

diff_2024 <- setdiff(database_2024$scientific_name, map_2024$eBird.Scientific.Name.2024)
length(diff_2024)
# 38

# Checking if database scientific names come from BLI ----
diff_2022 <- setdiff(database_2024$scientific_name, map_2022$BLI.Scientific.Name)
length(diff_2022)
# 55

diff_2023 <- setdiff(database_2024$scientific_name, map_2023$BLI.Scientific.Name)
length(diff_2023)
# 53

diff_2024 <- setdiff(database_2024$scientific_name, map_2024$BLI.Scientific.Name)
length(diff_2024)
# 55

# Checking if database scientific names come from India checklist ----
diff_2022 <- setdiff(database_2024$scientific_name, map_2022$India.Checklist.Scientific.Name)
length(diff_2022)
# 0

diff_2023 <- setdiff(database_2024$scientific_name, map_2023$India.Checklist.Scientific.Name)
length(diff_2023)
# 15

diff_2024 <- setdiff(database_2024$scientific_name, map_2024$India.Checklist.Scientific.Name)
length(diff_2024)
# 38

# This exercise reveals that the scientific names in the website database come
# from India checklist 2022. So, other information pertaining to 2022 will be 
# pulled from the SoIB mapping 2022

# In the database, pull the corresponding eBird English names from 2022 from
# SoIB mapping 2022 using the Scientific names

database_2024_1 <- database_2024 %>%
  left_join(map_2022 %>% select(eBird.English.Name.2022, India.Checklist.Scientific.Name),
            by = c("scientific_name" = "India.Checklist.Scientific.Name"))

# Oriental Dwarf Kingfisher has been split in 2022 into Black-backed and 
# Rufous-backed. So more rows are generated in the final output

map_2022_fix <- map_2022 %>% filter(eBird.English.Name.2022 != 
"Rufous-backed Dwarf-Kingfisher")

database_2024_1 <- database_2024 %>%
  left_join(map_2022_fix %>% select(eBird.English.Name.2022, India.Checklist.Scientific.Name),
            by = c("scientific_name" = "India.Checklist.Scientific.Name")) %>%
  relocate(eBird.English.Name.2022, .after = post_title)

# Pull the eBird English names 2024 from eBird taxonomy using the eBird English
# Name 2022

database_2024_2 <- database_2024_1 %>%
  left_join(taxmap %>% select(eBird.English.Name.2022, eBird.English.Name.2024),
            by = c("eBird.English.Name.2022")) %>%
  relocate(eBird.English.Name.2024, .after = eBird.English.Name.2022)

length(which(is.na(database_2024_2$eBird.English.Name.2024)))

# Pull the latest IUCN statuses from SoIB mapping 2024 using the eBird English
# Name 2024

colnames(database_2024_1)

database_2024_3 <- database_2024_2 %>%
  left_join(map_2024 %>% select(eBird.English.Name.2024, IUCN.Category),
            by = c("eBird.English.Name.2024")) %>%
  relocate(IUCN.Category, .after = iucn_status)

# Check if all the 12 IUCN changes have been roped in

temp <- database_2024_3 %>% filter(iucn_status != IUCN.Category) %>%
  select(c(post_title, iucn_status, IUCN.Category)) %>%
  unique()

temp <- database_2024_3 %>% filter(iucn_status != IUCN.Category) %>%
    select(c(post_title, iucn_status, IUCN.Category)) %>%
    group_by(post_title) %>%
    slice(1)

# Remove additional columns added previously

database_2025 <- database_2024_3 %>% select(-c(eBird.English.Name.2022,
                                               eBird.English.Name.2024,
                                               iucn_status)) %>%
  rename(iucn_status = IUCN.Category)

# Make sure all column names are similar

temp <- if_else(colnames(database_2025)==colnames(database_2024), TRUE, FALSE)
table(temp)

temp1 <- database_2025 %>% select(c(post_title, iucn_status)) %>%
  unique()

write.csv(database_2025, "20_website/website_database_2025_iucn_update.csv", 
          row.names = FALSE)

