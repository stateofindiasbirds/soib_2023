# Updates the ebird taxonomy mapping file with the 2025 taxonomy

library(tidyverse)

ebird_checklist_2025 <- read.csv("00_data/Clements_v2025-October-2025.csv", header = T)

ebird_tax_2024 <- read.csv("00_data/eBird_taxonomy_mapping.csv", header = T)

# How many english names present in 2024 are not in 2025?

eBird_names_not_in_2025 <- ebird_tax_2024 %>%
  anti_join(ebird_checklist_2025, by = c("eBird.English.Name.2024" = "English.name"))

# There are species present in 2025 but under a different name
# Here, we assume that the scientific name has remained unchanged.
# If the scientific name has also changed, that will be counted as a different
# species and it will not figure in the the taxonomy file anyway. 

# Get species from soib mapping 2024

soib_mapping_2024 <- read.csv("00_data/SoIB_mapping_2024.csv", header = T)

# Get scientific names of all species in the 2024 taxonomy column from the
# mapping file

ebird_tax_names_2024 <- ebird_tax_2024$eBird.English.Name.2024

scientific_names_2024 <- ebird_tax_2024 %>%
  left_join(soib_mapping_2024 %>%
              dplyr::select(c(eBird.English.Name.2024, eBird.Scientific.Name.2024)),
            by = "eBird.English.Name.2024")

# Use these scientific names to get English names from 2025

english_names_2025 <- scientific_names_2024 %>%
  left_join(ebird_checklist_2025 %>%
              dplyr::select(c(English.name, scientific.name)),
            by = join_by(eBird.Scientific.Name.2024 == scientific.name))

# Which English names have changed from 2024 to 2025?

english_names_changed <- english_names_2025 %>%
  mutate(name_changed = ifelse(eBird.English.Name.2024 == English.name, 1, 0))
  

# 4 species
# Get the scientific names for these species from the mapping file 2024 and 
# lookup the common names in the 2025 checklist for these species. 

# This results in
# Whimbrel in 2024 --> Eurasian Whimbrel in 2025
# White Tern in 2024 --> Blue-bellied White-Tern in 2025
# Eurasian Hoopoe in 2024 --> Common Hoopoe in 2025
# Striated Heron in 2024 --> Little Heron in 2025

# Except the above 3 species, copy the 2024 english names into another column 


ebird_tax_2025 <- ebird_tax_2024 %>%
  mutate(eBird.English.Name.2025 = case_when(
    eBird.English.Name.2024 == "Whimbrel" ~ "Eurasian Whimbrel",
    eBird.English.Name.2024 == "White Tern" ~ "Blue-billed White-Tern",
    eBird.English.Name.2024 == "Eurasian Hoopoe" ~ "Common Hoopoe",
    eBird.English.Name.2024 == "Striated Heron" ~ "Little Heron",
    TRUE ~ eBird.English.Name.2024   # fallback = copy original
  ))


write.csv(ebird_tax_2025, file = "00_data/eBird_taxonomy_mapping.csv", row.names = FALSE)




