
# This script updates the SoIB mapping file. 

india_checklist_2025 <- read.csv("00_data/india_checklist_v10.csv", header = T)
ebird_checklist_2025 <- read.csv("00_data/Clements_v2025-October-2025.csv", header = T)
tax_map <- read.csv("00_data/eBird_taxonomy_mapping.csv")
india_ebird_map_2025 <- read.csv("00_data/india_ebird_checklist_name_mapping_2025.csv", header = T)
india_bli_map_2025 <- read.csv("00_data/india_bli_checklist_name_mapping_2025.csv", header = T)


# The species list is derived from the India Checklist so will start from there

# Alongside names, also pull IUCN redlist, WPA Schedule, CITES Appendix and CMS Appendix from the 2025 checklist
map_2025 <- data.frame(India.Checklist.Common.Name = india_checklist_2025$English.Name,
                       India.Checklist.Scientific.Name = india_checklist_2025$Scientific.Name,
                       IUCN.Category = india_checklist_2025$IUCN.RedList,
                       WPA.Schedule = india_checklist_2025$WPA.Schedule,
                       CITES.Appendix = india_checklist_2025$CITES.Appendix,
                       CMS.Appendix = india_checklist_2025$CMS.Appendix,
                       Order = india_checklist_2025$Order)

# The Family column in the final xlsx file is derived from eBird checklist.
# So it will be pulled in later.

# Pull other columns from the 2024 mapping file

map_2024 <- read.csv("00_data/SoIB_mapping_2024.csv", header = T)

map_2025_col_from_map2024 <- map_2025 %>% left_join(map_2024 %>%
                                                      dplyr::select(c(SoIB.Past.Priority.Status,
                                                                      SoIB.Past.Long.Term.Status,
                                                                      SoIB.Past.Current.Status,
                                                                      SoIB.Past.Range.Status,
                                                                      Breeding.Activity.Period,
                                                                      Non.Breeding.Activity.Period,
                                                                      Diet.Guild,
                                                                      India.Endemic,
                                                                      Subcontinent.Endemic,
                                                                      Himalayas.Endemic,
                                                                      Endemic.Region,
                                                                      Habitat.Specialization,
                                                                      Migratory.Status.Within.India,
                                                                      Essential,
                                                                      Discard,
                                                                      Restricted.Islands,
                                                                      Onepercent.Estimates, # 1% Population threshold
                                                                      India.Checklist.Common.Name,
                                                                      India.Checklist.Scientific.Name)),
                                                    by = c("India.Checklist.Common.Name",
                                                           "India.Checklist.Scientific.Name"))

# Pull eBird English and Scientific Names from the India-eBird mapping file

map_2025_ebird_names <- map_2025_col_from_map2024 %>% left_join(india_ebird_map_2025 %>%
                                                                  dplyr::select(c(English.Name,
                                                                                  Scientific.Name,
                                                                                  eBird.English.Name.2025,
                                                                                  eBird.Scientific.Name.2025)),
                                                                by = join_by(India.Checklist.Common.Name == English.Name,
                                                                               India.Checklist.Scientific.Name == Scientific.Name))

which(is.na(map_2025_ebird_names$eBird.English.Name.2025))
which(is.na(map_2025_ebird_names$eBird.Scientific.Name.2025))

# Pull BLI English and Scientific Names from the India-BLI mapping file

map_2025_ebird_bli_names <- map_2025_ebird_names %>% left_join(india_bli_map_2025 %>%
                                                                  dplyr::select(c(English.Name,
                                                                                  Scientific.Name,
                                                                                  BLI.Common.Name.2025,
                                                                                  BLI.Scientific.Name.2025)),
                                                                by = join_by(India.Checklist.Common.Name == English.Name,
                                                                             India.Checklist.Scientific.Name == Scientific.Name))

which(is.na(map_2025_ebird_bli_names$BLI.Common.Name.2025))
which(is.na(map_2025_ebird_bli_names$BLI.Scientific.Name.2025))

names(map_2025_ebird_bli_names)

# Pull eBird code

map_2025_ebird_code <- map_2025_ebird_bli_names %>% left_join(ebird_checklist_2025 %>%
                                                                dplyr::select(c(species_code,
                                                                                English.name,
                                                                                scientific.name,
                                                                                family)),
                                                              by = join_by(eBird.English.Name.2025 == English.name,
                                                                           eBird.Scientific.Name.2025 == scientific.name))

which(is.na(map_2025_ebird_code$species_code))


# Reorder columns

map_2025_reorder_col <- map_2025_ebird_code %>%
  dplyr::select(c(eBird.English.Name.2025,
                  eBird.Scientific.Name.2025,
                  species_code,
                  Order,
                  family,
                  SoIB.Past.Priority.Status,
                  SoIB.Past.Long.Term.Status,
                  SoIB.Past.Current.Status,
                  SoIB.Past.Range.Status,
                  Breeding.Activity.Period,
                  Non.Breeding.Activity.Period,
                  Diet.Guild,
                  India.Endemic,
                  Subcontinent.Endemic,
                  Himalayas.Endemic,
                  Endemic.Region,
                  Habitat.Specialization,
                  Migratory.Status.Within.India,
                  Essential,
                  Discard,
                  Restricted.Islands,
                  India.Checklist.Common.Name,
                  India.Checklist.Scientific.Name,
                  BLI.Common.Name.2025,
                  BLI.Scientific.Name.2025,
                  IUCN.Category,
                  WPA.Schedule,
                  CITES.Appendix,
                  CMS.Appendix,
                  Onepercent.Estimates))

# Rename columns

rename_cols <- c(eBird.Code = "species_code",
                 BLI.Common.Name = "BLI.Common.Name.2025",
                 BLI.Scientific.Name = "BLI.Scientific.Name.2025",
                 Family = "family")

map_2025_rename_cols <- map_2025_reorder_col %>% 
  rename(all_of(rename_cols))


# Some checks

# Are any IUCN categories empty?

which(is.na(map_2025_rename_cols$IUCN.Category) | map_2025_rename_cols$IUCN.Category == "")
unique(map_2025_rename_cols$IUCN.Category)

# Are family and order names empty

which(is.na(map_2025_rename_cols$Family))

which(is.na(map_2025_rename_cols$Order))

# Check some species

which(str_detect(map_2025_rename_cols$eBird.English.Name.2025, regex("petrel", ignore_case =  TRUE)))
which(str_detect(map_2025_rename_cols$eBird.English.Name.2025, regex("spur", ignore_case = TRUE)))

# Are any BLI and eBird names NA or empty

tmp <- map_2025_rename_cols %>% 
  filter(if_any(c(eBird.English.Name.2025,
                  eBird.Scientific.Name.2025,
                  BLI.Common.Name,
                  BLI.Scientific.Name), is.na))


write.csv(map_2025_rename_cols, "00_data/SoIB_mapping_2025.csv", row.names = FALSE)
