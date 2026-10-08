# These updates are based on an email from Alen on Jan 30, 2026 requesting scientific name changes for 3 species

SoIB_mapping_2024 <- read.csv("00_data/SoIB_mapping_2024.csv", header = T) # Previous file

SoIB_mapping_2024_update <- SoIB_mapping_2024 %>% 
  mutate(eBird.Scientific.Name.2024 = case_when(eBird.Scientific.Name.2024 == "Anthropoides virgo" ~ "Grus virgo",
                                                eBird.Scientific.Name.2024 == "Gygis alba" ~ "Gygis candida",
                                                eBird.Scientific.Name.2024 == "Butorides striata" ~ "Butorides atricapilla",
                                                TRUE ~ eBird.Scientific.Name.2024),
         India.Checklist.Scientific.Name =  case_when(India.Checklist.Scientific.Name == "Anthropoides virgo" ~ "Grus virgo",
                                                      India.Checklist.Scientific.Name == "Gygis alba" ~ "Gygis candida",
                                                      India.Checklist.Scientific.Name == "Butorides striata" ~ "Butorides atricapilla",
                                                      TRUE ~ India.Checklist.Scientific.Name),
         BLI.Scientific.Name =  case_when(BLI.Scientific.Name == "Anthropoides virgo" ~ "Grus virgo",
                                          BLI.Scientific.Name == "Gygis alba" ~ "Gygis candida",
                                          BLI.Scientific.Name == "Butorides striata" ~ "Butorides atricapilla",
                                          TRUE ~ BLI.Scientific.Name))


# Check to see if changes are reflected

SoIB_mapping_2024_subset <- SoIB_mapping_2024 %>% filter(eBird.Scientific.Name.2024 %in% c("Anthropoides virgo",
                                                                                           "Gygis alba",
                                                                                           "Butorides striata")) %>%
  dplyr::select(eBird.English.Name.2024,
                eBird.Scientific.Name.2024,
                India.Checklist.Scientific.Name,
                BLI.Scientific.Name)

SoIB_mapping_2024_update_subset <- SoIB_mapping_2024_update %>% filter(eBird.English.Name.2024 %in% c("Demoiselle Crane",
                                                                                           "White Tern",
                                                                                           "Striated Heron")) %>%
  dplyr::select(eBird.English.Name.2024,
                eBird.Scientific.Name.2024,
                India.Checklist.Scientific.Name,
                BLI.Scientific.Name)

# Check to make sure all other columns are unchanged

SoIB_mapping_2024_other_col <- SoIB_mapping_2024 %>% dplyr::select(-c(eBird.Scientific.Name.2024,
                                                                             India.Checklist.Scientific.Name,
                                                                             BLI.Scientific.Name))

SoIB_mapping_2024_update_other_col <- SoIB_mapping_2024_update %>% dplyr::select(-c(eBird.Scientific.Name.2024,
                                                                             India.Checklist.Scientific.Name,
                                                                             BLI.Scientific.Name))

temp <- all.equal(SoIB_mapping_2024_other_col,SoIB_mapping_2024_update_other_col) # TRUE

write.csv(SoIB_mapping_2024_update, "00_data/SoIB_mapping_2024.csv", row.names = FALSE)

# Update taxonomy in the SoIB main file without categories

SoIB_main_wocats <- read.csv(file = "01_analyses_full/results/SoIB_main_wocats.csv")

SoIB_main_wocats_update <- SoIB_main_wocats %>% 
  mutate(eBird.Scientific.Name.2024 = case_when(eBird.Scientific.Name.2024 == "Anthropoides virgo" ~ "Grus virgo",
                                                eBird.Scientific.Name.2024 == "Gygis alba" ~ "Gygis candida",
                                                eBird.Scientific.Name.2024 == "Butorides striata" ~ "Butorides atricapilla",
                                                TRUE ~ eBird.Scientific.Name.2024),
         India.Checklist.Scientific.Name =  case_when(India.Checklist.Scientific.Name == "Anthropoides virgo" ~ "Grus virgo",
                                                      India.Checklist.Scientific.Name == "Gygis alba" ~ "Gygis candida",
                                                      India.Checklist.Scientific.Name == "Butorides striata" ~ "Butorides atricapilla",
                                                      TRUE ~ India.Checklist.Scientific.Name),
         BLI.Scientific.Name =  case_when(BLI.Scientific.Name == "Anthropoides virgo" ~ "Grus virgo",
                                          BLI.Scientific.Name == "Gygis alba" ~ "Gygis candida",
                                          BLI.Scientific.Name == "Butorides striata" ~ "Butorides atricapilla",
                                          TRUE ~ BLI.Scientific.Name))

# Check to see if changes are reflected

SoIB_main_wocats_subset <- SoIB_main_wocats %>% filter(eBird.Scientific.Name.2024 %in% c("Anthropoides virgo",
                                                                                           "Gygis alba",
                                                                                           "Butorides striata")) %>%
  dplyr::select(eBird.English.Name.2024,
                eBird.Scientific.Name.2024,
                India.Checklist.Scientific.Name,
                BLI.Scientific.Name)

SoIB_main_wocats_update_subset <- SoIB_main_wocats_update %>% filter(eBird.English.Name.2024 %in% c("Demoiselle Crane",
                                                                                                      "White Tern",
                                                                                                      "Striated Heron")) %>%
  dplyr::select(eBird.English.Name.2024,
                eBird.Scientific.Name.2024,
                India.Checklist.Scientific.Name,
                BLI.Scientific.Name)

# Check to make sure all other columns are unchanged

SoIB_main_wocats_other_col <- SoIB_main_wocats %>% dplyr::select(-c(eBird.Scientific.Name.2024,
                                                                      India.Checklist.Scientific.Name,
                                                                      BLI.Scientific.Name))

SoIB_main_wocats_update_other_col <- SoIB_main_wocats_update %>% dplyr::select(-c(eBird.Scientific.Name.2024,
                                                                                    India.Checklist.Scientific.Name,
                                                                                    BLI.Scientific.Name))

temp <- all.equal(SoIB_main_wocats_other_col,SoIB_main_wocats_update_other_col) # TRUE

write.csv(SoIB_main_wocats_update, "01_analyses_full/results/SoIB_main_wocats.csv", row.names = FALSE)
