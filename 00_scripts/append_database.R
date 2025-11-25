library(tidyverse)

past_map = read.csv("00_data/SoIB_mapping_2023.csv")
map = read.csv("00_data/SoIB_mapping_2024.csv")
taxmap = read.csv("00_data/eBird_taxonomy_mapping.csv")

past_database = read.csv("20_website/website_database.csv")

past_map = past_map %>%
  dplyr::select(eBird.English.Name.2023,India.Checklist.Common.Name) %>%
  group_by(India.Checklist.Common.Name) %>% slice(1)
map = map %>%
  dplyr::select(eBird.English.Name.2024,
                IUCN.Category)

past_database = past_database %>%
  left_join(past_map, by = c("post_title" = "India.Checklist.Common.Name")) %>%
  left_join(taxmap) %>%
  left_join(map)

test = past_database %>%
  distinct(post_title,iucn_status,IUCN.Category) %>%
  filter(iucn_status != IUCN.Category)

new_database = past_database %>%
  mutate(iucn_status = IUCN.Category) %>%
  dplyr::select(-eBird.English.Name.2023,-eBird.English.Name.2024,-IUCN.Category)

write.csv(new_database,"20_website/website_database_2025.csv",row.names=F)
