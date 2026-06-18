# compile all SoIB_main files into single file to upload on website (and download from)

priority_update <- FALSE # The 2025 update does not update the priority categories

require(tidyverse)
require(glue)
require(writexl)
require(stringr)

load("00_data/maps_sf.RData") # for no. of cells in README
load("00_data/current_soib_migyears.RData")
load("00_data/analyses_metadata.RData")
source("00_scripts/00_functions.R")
source("00_scripts/20_functions.R")

# Load two newly created files which map the 2025 India checklist names with 
# 2025 eBird names and 2025 BLI names

india_ebird_map_2025 <- read.csv("00_data/india_ebird_checklist_name_mapping_2025.csv", header = T)
india_bli_map_2025 <- read.csv("00_data/india_bli_checklist_name_mapping_2025.csv", header = T)
ebird_checklist_2025 <- read.csv("00_data/Clements_v2025-October-2025.csv", header = T)
soib_mapping_2025 <- read.csv("00_data/SoIB_mapping_2025.csv", header = T)

# India checklist 2025

india_checklist_2025 <- read.csv("00_data/india_checklist_v10.csv", header = T)


interannual_update = TRUE
major_update = 2022
use_major_update = FALSE
# Use_major_update is FALSE because we are updating the trends and range sizes

# key states for each species
keystates <- read.csv("01_analyses_full/results/key_state_species_full.csv") %>% 
  arrange(ST_NM, India.Checklist.Common.Name) 

latest_map <- read.csv(paste("00_data/SoIB_mapping_",latest_soib_my,".csv",sep=""))
tax_map <- read.csv("00_data/eBird_taxonomy_mapping.csv")

# if (interannual_update == T)
# {
#   major_update_map = read.csv(paste("00_data/SoIB_mapping_",major_update,".csv",sep="")) %>%
#     dplyr::select(India.Checklist.Common.Name,eBird.English.Name.2022)
#   latest_map = read.csv(paste("00_data/SoIB_mapping_",latest_soib_my,".csv",sep=""))
#   tax_map = read.csv("00_data/eBird_taxonomy_mapping.csv")
#   
#   keystates = keystates %>%
#     left_join(major_update_map) %>%
#     left_join(tax_map) %>%
#     dplyr::select(ST_NM, eBird.English.Name.2024)
# }


# import ----------------------------------------------------------------------------

# importing all data and setting up
main_db0 <- map2(get_metadata()$SOIBMAIN.PATH, get_metadata()$MASK, 
                 ~ read_fn(.x) %>% bind_cols(tibble(MASK = .y))) %>% 
  list_rbind()

#save(main_db0, file = "main_db0.RData")

# Remove Cape petrel and Spur-winged lapwing because they are not wild in India.
# These species somehow found their way into the 2024 mapping file. This has been
# fixed in the updated mapping file.

main_db0 <- main_db0 %>%
  filter(! India.Checklist.Common.Name %in% c("Cape Petrel", "Spur-winged Lapwing"))

tmp <- as.data.frame(table(main_db0$India.Checklist.Common.Name, main_db0$MASK))

tmp2 <- tmp %>% filter(Freq == 2) %>% dplyr::select(Var1) %>% unique() # No species are repeated in a MASK

if (use_major_update) {
  analyses_metadata = analyses_metadata %>%
    mutate(SOIBMAIN.MAJOR.PATH = paste(RESULTS,"past/SoIB_main_",major_update+1,".csv",sep=""))
  
  main_db_major_update <- map2(analyses_metadata$SOIBMAIN.MAJOR.PATH, get_metadata()$MASK, 
                               ~ read_fn(.x) %>% bind_cols(tibble(MASK = .y))) %>% 
    list_rbind() %>%
    left_join(tax_map) %>%
    dplyr::select(eBird.English.Name.2024,MASK,longtermlci,longtermmean,
                  longtermrci,currentslopelci,currentslopemean,
                  currentsloperci,rangelci,rangemean,rangerci) %>%
    group_by(eBird.English.Name.2024,MASK) %>% slice(1)
  
  db_order = names(main_db0)
  
  main_db0 = main_db0 %>%
    dplyr::select(-longtermlci,-longtermmean,
                  -longtermrci,-currentslopelci,-currentslopemean,
                  -currentsloperci,-rangelci,-rangemean,-rangerci) %>%
    left_join(main_db_major_update) %>%
    relocate(all_of(db_order))
}


# red list values
redlist_proposed_spec <- c(
  "Northern Shoveler", "Baillon's Crake", "Terek Sandpiper", "Marsh Sandpiper", "Forest Wagtail",
  "Kentish Plover", "Spot-winged Starling", "Green-winged Teal", "Garganey", "Great Gray Shrike",
  "Blue Rock-Thrush", "Tibetan Sand-Plover", "Little Ringed Plover", "Indian Roller",
  "Common Sandpiper"
)

# to change after new redlist.csv is produced

# redlist <- read_csv("01_analyses_full/results/redlist.csv") %>%
#     left_join(major_update_map, by = c("Species" = "India.Checklist.Common.Name")) %>%
#     left_join(tax_map) %>%
#     dplyr::select(-Species, -eBird.English.Name.2022, -eBird.English.Name.2023)
  
redlist <- read_csv("01_analyses_full/results/redlist.csv") %>%
  left_join(latest_map %>%
              dplyr::select(c("India.Checklist.Common.Name", "eBird.English.Name.2024")), 
            by = c("Species" = "India.Checklist.Common.Name")) %>%
  dplyr::select(-Species)


redlist <- redlist %>% 
  filter(Years3GEN <= 14) %>% 
  dplyr::select("eBird.English.Name.2024", "3GEN Decline", "Criteria A Redlist Category Proposed") %>% 
  ### TEMP: correcting Near-threatened (already corrected in source script)
  mutate(`Criteria A Redlist Category Proposed` = case_when(
    `Criteria A Redlist Category Proposed` == "Near-threatened" ~ "Near Threatened",
    TRUE ~ `Criteria A Redlist Category Proposed`
  )) %>% 
  # only list final 15 proposed in report
  mutate(`Criteria A Redlist Category Proposed` = case_when(
    eBird.English.Name.2024 %in% redlist_proposed_spec ~ `Criteria A Redlist Category Proposed`, 
    TRUE ~ NA_character_
  )) %>% 
  magrittr::set_colnames(c("eBird.English.Name.2024", 
                           "Projected % Decline in 3 Generations",
                           "Regional Red List Category")) %>% 
  # these values only for country-level, and not for subnational
  mutate(MASK = "none") %>% 
  mutate(`Projected % Decline in 3 Generations` = str_remove(`Projected % Decline in 3 Generations`,
                                                             "%") %>% 
           as.numeric())

# process ---------------------------------------------------------------------------

main_db <- main_db0 %>% 
  # converting binary columns to logical
  mutate(across(c("India.Endemic", "Subcontinent.Endemic", "Himalayas.Endemic"),
                ~ case_when(. == "Yes" ~ TRUE, TRUE ~ FALSE)),
         Restricted.Islands = case_when(Restricted.Islands == 1 ~ TRUE, TRUE ~ FALSE),
         across(c("Long.Term.Analysis", "Current.Analysis", "Selected.SoIB"), 
                ~ case_when(. == "X" ~ TRUE, TRUE ~ FALSE))) %>% 
  # remove proj columns, other unnecessary columns
  mutate(across(c(starts_with("proj20"),
                  "Essential", "Discard", "eBird.Code"), ~ as.null(.))) %>% 
  # adding column whether species is key for current state
  is_curspec_key4state() %>% 
  # retain taxonomic order of species
  left_join(main_db0 %>% 
              distinct(India.Checklist.Common.Name) %>% 
              rownames_to_column("SPEC.ORDER") %>% 
              mutate(SPEC.ORDER = as.numeric(SPEC.ORDER))) %>% 
  group_by(MASK) %>% 
  arrange(MASK, SPEC.ORDER) %>% 
  # removing species that are not found in/relevant for mask
  # but for India, keeping all
  filter(MASK == "none" |
           (MASK != "none" & !is.na(SoIB.Major.Update.Priority.Status))) %>%
  ungroup() %>% 
  dplyr::select(-SPEC.ORDER) %>% 
  # round model estimates appropriately 
  round_model_estimates() %>%
  # percentage for Range Coverages (Grid already present)
  mutate(across(c("proprange25km2000","proprange25km.current","proprange25km.latestyear"),
                ~ round(. * 100))) %>% 
  mutate(across(c("mean5km","ci5km"),
                ~ round(.))) %>% 
  # join Red List columns
  left_join(redlist) %>% 
  # TEMPORARY FIX for subnational SoIB Priority Status (retain national Status)
  temp_priority_correction() %>%
  # remove "latest" priorities
  dplyr::select(-SoIB.Latest.Long.Term.Status,-SoIB.Latest.Current.Status,
                -SoIB.Latest.Range.Status,-SoIB.Latest.Priority.Status) %>%
  # move columns
  relocate(
    "India.Checklist.Common.Name","India.Checklist.Scientific.Name",
    "SoIB.Major.Update.Priority.Status","SoIB.Major.Update.Long.Term.Status",
    "SoIB.Major.Update.Current.Status","SoIB.Major.Update.Range.Status",
    "eBird.English.Name.2024","eBird.Scientific.Name.2024", 
    "BLI.Common.Name", "BLI.Scientific.Name","Order","Family",
    "Breeding.Activity.Period","Non.Breeding.Activity.Period","Diet.Guild",
    "Endemic.Region","India.Endemic","Subcontinent.Endemic","Himalayas.Endemic",
    "Habitat.Specialization","Migratory.Status.Within.India","Restricted.Islands",
    "IUCN.Category","WPA.Schedule","CITES.Appendix","CMS.Appendix","Onepercent.Estimates",
    "Selected.SoIB","Long.Term.Analysis","Current.Analysis",
    "longtermlci","longtermmean","longtermrci","currentslopelci","currentslopemean",
    "currentsloperci","rangelci","rangemean","rangerci",
    "KEY",
    "totalrange25km","proprange25km2000","proprange25km.current","proprange25km.latestyear",
    "mean5km","ci5km",
    "Projected % Decline in 3 Generations","Regional Red List Category",
    "SoIB.Past.Priority.Status","SoIB.Past.Long.Term.Status","SoIB.Past.Current.Status","SoIB.Past.Range.Status"
  ) %>% 
  # rename columns
  magrittr::set_colnames(c(
    "English Name","Scientific Name",
    "SoIB 2023 Priority Status","SoIB 2023 Long-term Trend Status",
    "SoIB 2023 Current Annual Trend Status","SoIB 2023 Distribution Range Size Status",
    "eBird English Name 2024","eBird Scientific Name 2024",
    "BLI English Name 2024","BLI Scientific Name 2024","Order","Family",
    "Breeding Activity Period","Non-breeding Activity Period","Diet Guild",
    "Endemicity","Endemic to India","Endemic to Subcontinent","Endemic to Himalaya",
    "Habitat Specialization","Migratory Status within India","Restricted to Islands",
    "IUCN Category","WPA Schedule","CITES Appendix","CMS Appendix","1% Population Threshold",
    "Selected for SoIB","Selected for Long-term Trend","Selected for Current Annual Trend",
    "Long-term Trend LCI","Long-term Trend Mean","Long-term Trend UCI",
    "Current Annual Trend LCI","Current Annual Trend Mean","Current Annual Trend UCI",
    "Distribution Range Size LCI","Distribution Range Size Mean","Distribution Range Size UCI",
    "State Where Species Key",
    "Number of Grids","Range Coverage (Pre-2000)","Range Coverage (Current)","Range Coverage (2024)",
    "Grid Coverage Mean", "Grid Coverage CI",
    "Projected % Decline in 3 Generations","Regional Red List Category",
    "SoIB 2020 Concern Status","SoIB 2020 Long-term Trend Status",
    "SoIB 2020 Current Annual Trend Status","SoIB 2020 Distribution Range Size Status",
    "MASK"
  )) %>% 
  # joining mask label
  join_mask_codes() %>% 
  dplyr::select(-c(MASK, MASK.CODE))


# converting certain character columns to factor
main_db <- main_db %>% 
  mutate(
    `SoIB 2023 Priority Status` = factor(
      `SoIB 2023 Priority Status`,
      levels = c("Low", "Moderate", "High")
    ),
    `SoIB 2023 Long-term Trend Status` = factor(
      `SoIB 2023 Long-term Trend Status`,
      levels = c("Rapid Decline", "Decline", "Insufficient Data", 
                 "Trend Inconclusive", "Stable", 
                 "Increase", "Rapid Increase")
    ),
    `SoIB 2023 Current Annual Trend Status` = factor(
      `SoIB 2023 Current Annual Trend Status`,
      levels = c("Rapid Decline", "Decline", "Insufficient Data", 
                 "Trend Inconclusive", "Stable", 
                 "Increase", "Rapid Increase")
    ),
    `SoIB 2023 Distribution Range Size Status` = factor(
      `SoIB 2023 Distribution Range Size Status`,
      levels = c("Very Restricted", "Restricted", "Historical", 
                 "Moderate", "Large", "Very Large")
    ),
    `SoIB 2020 Concern Status` = factor(
      `SoIB 2020 Concern Status`,
      levels = c("Low", "Moderate", "High")
    ),
    `SoIB 2020 Long-term Trend Status` = factor(
      `SoIB 2020 Long-term Trend Status`,
      levels = c("Strong Decline", "Moderate Decline", "Data Deficient",
                 "Uncertain", "Stable", 
                 "Moderate Increase", "Strong Increase")
    ),
    `SoIB 2020 Current Annual Trend Status` = factor(
      `SoIB 2020 Current Annual Trend Status`,
      levels = c("Strong Decline", "Moderate Decline", "Data Deficient",
                 "Uncertain", "Stable", 
                 "Moderate Increase", "Strong Increase")
    ),
    `SoIB 2020 Distribution Range Size Status` = factor(
      `SoIB 2020 Distribution Range Size Status`,
      levels = c("Very Restricted", "Restricted", "Data Deficient", 
                 "Moderate", "Large", "Very Large")
    ),
    `IUCN Category` = factor(
      `IUCN Category`,
      levels = c(
        # "Extinct", "Extinct in the Wild", 
        "Critically Endangered", "Endangered", 
        "Vulnerable", "Near Threatened", 
        "Least Concern", "Data Deficient", "Not recognized"
      )
    ),
    `Regional Red List Category` = factor(
      `Regional Red List Category`,
      levels = c(
        # "Extinct", "Extinct in the Wild", 
        # "Critically Endangered", 
        "Endangered", "Vulnerable", "Near Threatened"
        # "Least Concern", "Data Deficient", "Not Recognised"
      )
    ),
    `WPA Schedule` = factor(
      `WPA Schedule`,
      levels = c("Not scheduled", "Recent addition", 
                 "Schedule-II", "Schedule-I")
    ),
    `CITES Appendix` = factor(
      `CITES Appendix`,
      levels = c("Appendix II", "Appendix I")
    ),
    `CMS Appendix` = factor(
      `CMS Appendix`,
      levels = c("Appendix II", "Appendix I")
    )
  ) %>% 
  mutate(across(c("Order", "Family", "Breeding Activity Period",
                  "Non-breeding Activity Period", "Diet Guild",
                  "Endemicity", "Habitat Specialization",
                  "Migratory Status within India"),
                ~ as.factor(.)))


# At this point, the English Names and Scientific Names reflect the 2024 taxonomy
# from India Checklist. 
# But to maintain continuity on the website, we will revert to the 2023 names.

# Remove the Order and Family columns as hey will be pulled in later ----

main_db <- main_db %>%
  dplyr::select(c(-"Order",
                  -"Family"))


main_db <- unique(main_db) # There are some duplicate rows (Scaly thrush). This is an adhoc solution

# Join with ebird taxonomy file to pull ebird names from 2022
main_db_1 <- main_db %>% left_join(tax_map %>%
                                     dplyr::select(c("eBird.English.Name.2022", "eBird.English.Name.2024")),
                                   by = join_by(`eBird English Name 2024` == eBird.English.Name.2024))

# # Remove rows where 2022 checklist names are NA
# 
# main_db_1 <- main_db_1 %>% drop_na(eBird.English.Name.2022)

# Note 1: Pay close attention to species which have been lumped in 2025.

# For example, Striated Swallow and Red-rumped Swallow have been lumped into 
# one species - Eastern Red-rumped Swallow in 2025. But reverting to 2023 names 
# means that we show both Striated and Red-rumped swallow in the excel file

# Note 2: 

# There are new species in 2025 which have no corresponding name in 2023. These 
# will remain the 2024 names (example: Barnacle goose)

# removing striated heron

# main_db_2 <- main_db_1 %>% filter(is.na(eBird.English.Name.2022) | eBird.English.Name.2022 != "Striated Swallow")

# Use soib_mapping_2022 to pull the corresponding 2022 India Checklist names

soib_mapping_2022 <- read.csv("00_data/SoIB_mapping_2022.csv", header = T)

main_db_2 <- main_db_1 %>% left_join(soib_mapping_2022 %>%
                                       dplyr::select("eBird.English.Name.2022",
                                                     "eBird.Scientific.Name.2022",
                                                     "India.Checklist.Common.Name",
                                                     "India.Checklist.Scientific.Name"),
                                     by = "eBird.English.Name.2022")

# Rename the India checklist common and scientific name such that it is clear
# that they are from 2022

main_db_3 <- main_db_2 %>%
  rename(`India.Checklist.Common.Name.2022` = `India.Checklist.Common.Name`,
         `India.Checklist.Scientific.Name.2022` = `India.Checklist.Scientific.Name`)


# In this object: 
# English Name: India.Checklist.Common.Name.2024
# Scientific Name: India.Checklist.Scientific.Name.2024


# Now pull the India checklist common and scientific name from 2024 for species
# like Barnacle goose which did no exist in 2022 taxonomy

soib_mapping_2024 <- read.csv("00_data/SoIB_mapping_2024.csv", header = T)

main_db_4 <- main_db_3 %>% left_join(soib_mapping_2024 %>%
                                       dplyr::select("eBird.English.Name.2024",
                                                     "India.Checklist.Common.Name",
                                                     "India.Checklist.Scientific.Name"),
                                     by = join_by(`eBird English Name 2024` == "eBird.English.Name.2024"))

main_db_5 <- main_db_4 %>%
  rename(`India.Checklist.Common.Name.2024` = `India.Checklist.Common.Name`,
         `India.Checklist.Scientific.Name.2024` = `India.Checklist.Scientific.Name`)

# Species are sourced from the 2024 list but displayed using 2022 taxonomy names.
# Where a species has no 2022 equivalent, its 2024 name is used as a fallback

main_db_6 <- main_db_5 %>%
  mutate(`India.Checklist.Common.Name` = case_when(
    !is.na(`India.Checklist.Common.Name.2022`)  ~ `India.Checklist.Common.Name.2022`,
    TRUE ~ `India.Checklist.Common.Name.2024`),
    `India.Checklist.Scientific.Name` = case_when(
      !is.na(`India.Checklist.Scientific.Name.2022`)  ~ `India.Checklist.Scientific.Name.2022`,
      TRUE ~ `India.Checklist.Scientific.Name.2024`)) 

tmp <- main_db_6 %>% filter(is.na(India.Checklist.Scientific.Name))
tmp <- main_db_6 %>% filter(is.na(`IUCN Category`))
tmp <- main_db_6 %>% filter(is.na(`CMS Appendix`))


# Some species - Menetries's warbler, White-throated Rock-Thrush,
# Yellow-billed Grosbeak have no India.Checklist.Scientific Name, IUCN category, 
# CMS Appendix and CITES Appendix. Not sure why they are not in the 2024 mapping file.
# Use the ebird India mapping file 2025. This contains both India Checklist names
# from 2025 (what we need now) and eBird names from 2025 (what we need later).

main_db_7 <- main_db_6 %>%
  mutate(India.Checklist.Scientific.Name = case_when(
    India.Checklist.Common.Name == "Menetries's Warbler" ~ "Curruca mystacea",
    India.Checklist.Common.Name == "White-throated Rock-Thrush" ~ "Monticola gularis",
    India.Checklist.Common.Name == "Yellow-billed Grosbeak" ~ "Eophona migratoria",
    TRUE ~ India.Checklist.Scientific.Name   # fallback = copy original
  ))

main_db_7 <- main_db_7 %>%
  mutate(`IUCN Category` = case_when(
    India.Checklist.Common.Name == "Menetries's Warbler" ~ "Least Concern",
    India.Checklist.Common.Name == "White-throated Rock-Thrush" ~ "Least Concern",
    India.Checklist.Common.Name == "Yellow-billed Grosbeak" ~ "Least Concern",
    TRUE ~ `IUCN Category`   # fallback = copy original
  ))  

main_db_7 <- main_db_7 %>%
  mutate(`WPA Schedule` = case_when(
    India.Checklist.Common.Name == "Menetries's Warbler" ~ "Recent addition",
    India.Checklist.Common.Name == "White-throated Rock-Thrush" ~ "Recent addition",
    India.Checklist.Common.Name == "Yellow-billed Grosbeak" ~ "Recent addition",
    TRUE ~ `IUCN Category`   # fallback = copy original
  ))  
which(is.na(main_db_7$India.Checklist.Common.Name))
which(is.na(main_db_7$India.Checklist.Scientific.Name)) 

# Get eBird 2025 english names from the taxonomy file

main_db_8 <- main_db_7 %>%
  left_join(tax_map %>%
              dplyr::select(c(eBird.English.Name.2024, eBird.English.Name.2025)) %>% unique(),
                            by = join_by(`eBird English Name 2024` == eBird.English.Name.2024))


which(is.na(main_db_8$eBird.English.Name.2025))

# There are some species from 2024 which have no corresponding 2025 names in
# tax map. Pull these names directly from the eBird checklist 2025

species_2024_names <- main_db_8 %>%
  slice(which(is.na(main_db_8$eBird.English.Name.2025))) %>%
  pull(`eBird English Name 2024`) %>% unique()

lookup_2025 <- ebird_checklist_2025 %>%
  filter(English.name %in% species_2024_names) %>%
  dplyr::select(
    `eBird English Name 2024` = English.name,
    eBird.English.Name.2025 = English.name
  ) %>%
  distinct()

which(is.na(lookup_2025$eBird.English.Name.2025))

# All species english names from 2024 are in 2025. 

main_db_9 <- main_db_8 %>% 
  left_join(lookup_2025, 
            by = "eBird English Name 2024", 
            suffix = c("", ".from_checklist")) %>% 
  mutate(eBird.English.Name.2025 = 
            coalesce(eBird.English.Name.2025, eBird.English.Name.2025.from_checklist)) %>% 
  dplyr::select(-eBird.English.Name.2025.from_checklist)

which(is.na(main_db_9$eBird.English.Name.2025))

# Get eBird scientific names from 2025

main_db_10 <-  main_db_9 %>%
  left_join(ebird_checklist_2025 %>% 
              filter(category == "species") %>%
              dplyr::select(c(English.name,
                              scientific.name)),
            by = join_by(eBird.English.Name.2025 == English.name))

# Rename the scientific name

main_db_11 <- main_db_10 %>%
rename(
  eBird.Scientific.Name.2025 = scientific.name
) 

which(is.na(main_db_11$eBird.Scientific.Name.2025))

names(main_db_11)

# Get India Checklist names from 2025

main_db_12 <- main_db_11 %>%
  left_join(india_ebird_map_2025 %>%
              dplyr::select(c(English.Name,
                              Scientific.Name,
                              eBird.English.Name.2025)),
            by = "eBird.English.Name.2025")

which(is.na(main_db_12$English.Name))
which(is.na(main_db_12$Scientific.Name))

# Rename the India checklist English and scientific names

main_db_13 <- main_db_12 %>%
  rename(
    India.Checklist.Common.Name.2025 = English.Name,
    India.Checklist.Scientific.Name.2025 = Scientific.Name
  ) 

# Get BLI names from 2025

main_db_14 <- main_db_13 %>%
  left_join(india_bli_map_2025 %>%
              dplyr::select(c(BLI.Common.Name.2025,
                              BLI.Scientific.Name.2025,
                              English.Name)),
            by = join_by(India.Checklist.Common.Name.2025 == English.Name))

which(is.na(main_db_14$BLI.Common.Name.2025))
which(is.na(main_db_14$BLI.Scientific.Name.2025))

names(main_db_14)

# Get Order from India Checklist 2025

main_db_15 <- main_db_14 %>%
  left_join(india_checklist_2025 %>%
              dplyr::select(c(English.Name,
                              Scientific.Name,
                              Order)),
            by = join_by(India.Checklist.Common.Name.2025 == English.Name,
                         India.Checklist.Scientific.Name.2025 == Scientific.Name))

# Get Family from eBird Checklist 2025

main_db_16 <- main_db_15 %>%
  left_join(ebird_checklist_2025 %>%
              dplyr::select(c(English.name,
                              scientific.name,
                              family)),
            by = join_by(eBird.English.Name.2025 == English.name,
                         eBird.Scientific.Name.2025 == scientific.name))

main_db_17 <- main_db_16 %>%
  rename(`Family` = `family`)

names(main_db_17)

which(is.na(main_db_17$`IUCN Category`))

# Pull IUCN Category, WPA Schedule, CITES Appendix and CMS Appendix from the
# latest India Checklist

main_db_18 <- main_db_17 %>%
  left_join(india_checklist_2025 %>%
              dplyr::select(c(English.Name,
                              IUCN.RedList,
                              WPA.Schedule,
                              CITES.Appendix,
                              CMS.Appendix)),
            by = join_by(India.Checklist.Common.Name.2025 == English.Name))

which(is.na(main_db_18$IUCN.RedList)| main_db_18$IUCN.RedList == "")
which(is.na(main_db_18$WPA.Schedule) | main_db_18$WPA.Schedule == "")
which(is.na(main_db_18$CITES.Appendix) | main_db_18$CITES.Appendix == "")
which(is.na(main_db_18$CMS.Appendix) | main_db_18$CMS.Appendix == "")


# Reorganize


main_db_19 <- main_db_18 %>%
  dplyr::select(
    -"English Name",
    -"Scientific Name",
    -"eBird English Name 2024", 
    -"eBird Scientific Name 2024",
    -"BLI English Name 2024",
    -"BLI Scientific Name 2024",
    -"eBird.English.Name.2022",
    -"eBird.Scientific.Name.2022",
    -India.Checklist.Common.Name.2022,
    -India.Checklist.Scientific.Name.2022,
    -India.Checklist.Common.Name.2024,
    -India.Checklist.Scientific.Name.2024,
    -`IUCN Category`,
    -`WPA Schedule`,
    -`CMS Appendix`,
    -`CITES Appendix`
  ) %>%
  rename(
    `English Name` = `India.Checklist.Common.Name`,
    `Scientific Name` = `India.Checklist.Scientific.Name`,
    `IUCN Category` = IUCN.RedList,
    `WPA Schedule` = WPA.Schedule,
    `CITES Appendix` = CITES.Appendix,
    `CMS Appendix` = CMS.Appendix
    
  ) %>%
  relocate(`English Name`, `Scientific Name`)


main_db_20 <- main_db_19 %>%
  relocate(eBird.English.Name.2025,
           eBird.Scientific.Name.2025,
           BLI.Common.Name.2025,
           BLI.Scientific.Name.2025,
           India.Checklist.Common.Name.2025,
           India.Checklist.Scientific.Name.2025,
           Order,
           Family,
           .after = `SoIB 2023 Distribution Range Size Status`)

main_db_21 <- main_db_20 %>%
  relocate(`IUCN Category`,
           `WPA Schedule`,
           `CITES Appendix`,
           `CMS Appendix`,
           .after = `Restricted to Islands`)

# README ------------------------------------------------------------------

# info about data types
readme_datatype <- main_db_21 %>% 
  mutate(`Range Coverage CI (Current)` = 0) %>% 
  dplyr::select(-MASK.LABEL) %>% 
  reframe(across(everything(), ~ class(.))) %>% 
  pivot_longer(everything(), names_to = "Field", values_to = "Class")

# range of values
readme_range <- main_db_21 %>% 
  mutate(`Range Coverage CI (Current)` = NA) %>% 
  dplyr::select(-MASK.LABEL) %>% 
  reframe(across(!where(is.factor),
                 ~ range(na.omit(.)) %>% str_flatten_comma()),
          across(where(is.factor),
                 ~ c(first(levels(.)), 
                     last(levels(.))) %>% str_flatten_comma())) %>% 
  distinct() %>% 
  pivot_longer(everything(), names_to = "Field", values_to = "Range (min, max)") %>% 
  mutate(`Range (min, max)` = case_when(Field == "Range Coverage CI (Current)" ~ NA, 
                                        TRUE ~ `Range (min, max)`))

# which fields are only for national sheet?
readme_nat_excl <- main_db_21 %>% 
  mutate(NATIONAL = ifelse(MASK.LABEL == "India", TRUE, FALSE)) %>% 
  group_by(NATIONAL) %>% 
  reframe(across(everything(), ~ all(is.na(.)))) %>% 
  filter(NATIONAL == FALSE) %>% 
  dplyr::select(-NATIONAL) %>% 
  pivot_longer(everything(), names_to = "Field", values_to = "Exclusive to National")

readme <- tribble(
  ~ Field, ~ Meaning,
  
  "", "",
  "NOTE: Below is information about the superset of fields across all the sheets. Some fields are not applicable and hence are absent in subnational sheets (all except 'India'; see column 'Exclusive to National'). For example, SoIB 2023 Distribution Range Size Status assignment was done only at the national level.", "",
  "NOTE: India sheet contains all 1382 species in India Checklist v10.0 (https://indianbirds.in/india). Subnational sheets contain only those species whose corresponding subnational assessment was done. Note that while the primary source of species list is the v10.0 India checklist, the English Names and Scientific Names for species common to both v10 and v7.1 are retained from v7.1 for continuity with previous xlsx file. ", "",
  "", "",
  
  "English Name", "English name of species in India Checklist v7.1 (https://indianbirds.in/india)",
  "Scientific Name", "Scientific name of species in India Checklist v7.1 (https://indianbirds.in/india)",
  "SoIB 2023 Priority Status", "Conservation Priority Status of species from SoIB 2023 national-level assessment",
  "SoIB 2023 Long-term Trend Status", "Long-term Trend Status of species from SoIB 2023 assessment",
  "SoIB 2023 Current Annual Trend Status", "Current Annual Trend Status of species from SoIB 2023 assessment",
  "SoIB 2023 Distribution Range Size Status", "Distribution Range Size Status of species assigned from SoIB 2023 assessment",
  "eBird English Name 2025", "English name of species in eBird/Clements Checklist 2025 (https://www.birds.cornell.edu/clementschecklist/introduction/updateindex/october-2025/2025-citation-checklist-downloads/)",
  "eBird Scientific Name 2025", "Scientific name of species in eBird/Clements Checklist 2025 (https://www.birds.cornell.edu/clementschecklist/introduction/updateindex/october-2025/2025-citation-checklist-downloads/)",
  "BLI English Name 2025", "English name of species in HBW/BLI Checklist 2025 (https://datazone.birdlife.org/about-our-science/taxonomy)",
  "BLI Scientific Name 2025", "Scientific name of species in HBW/BLI Checklist 2025 https://datazone.birdlife.org/about-our-science/taxonomy)",
  "India Checklist Common Name 2025", "English name of species in India Checklist 2025 (https://indianbirds.in/india/)",
  "India Checklist Scientific Name 2025", "Scientific name of species in India Checklist 2025 (https://indianbirds.in/india/)",
  "Order", "Taxonomic Order to which species belongs from eBird checklist 2025",
  "Family", "Taxonomic Family to which species belongs from India checklist 2025",
  "Breeding Activity Period", "Breeding period of species, based on Wilman et al. 2014",
  "Non-breeding Activity Period", "Non-breeding period of species, based on Wilman et al. 2014",
  "Diet Guild", "Diet guild of species, based on Wilman et al. 2014",
  "Endemicity", "Endemicity of species adapted from India Checklist v10.0 (https://indianbirds.in/india)",
  "Endemic to India", "Whether species is endemic to India",
  "Endemic to Subcontinent", "Whether species is endemic to the Indian subcontinent",
  "Endemic to Himalaya", "Whether species is endemic to the Himalaya",
  "Habitat Specialization", "Habitat specialization of species, based on Wilman et al. 2014",
  "Migratory Status within India", "Migratory status of species within India, assigned based on multiple sources",
  "Restricted to Islands", "Whether species is restricted to the islands of India",
  "IUCN Category", "IUCN threat status category of species, based on India Checklist v10.0 (https://indianbirds.in/india)",
  "WPA Schedule", "WPA Schedule of species, based on India Checklist v10.0 (https://indianbirds.in/india)",
  "CITES Appendix", "CITES Appendix category of species, based on India Checklist v10.0 (https://indianbirds.in/india)",
  "CMS Appendix", "CMS Appendix category of species, based on India Checklist v10.0 (https://indianbirds.in/india)",
  "1% Population Threshold", "Wetlands International estimate of the 1% biogeographic population size (individuals) of a waterbird species",
  "Selected for SoIB", "Whether species was selected for SoIB 2023 analyses",
  "Selected for Long-term Trend", "Whether species was selected for Long-term Trend analysis in SoIB 2023",
  "Selected for Current Annual Trend", "Whether species was selected for Current Annual Trend analysis in SoIB 2023",
  "Long-term Trend LCI", "Lower limit of 95% confidence interval of modelled estimate of the change in abundance in 2024-25 relative to pre-2000 values (see p102 of SoIB 2023 report)",
  "Long-term Trend Mean", "Modelled estimate of the change in abundance in 2022-23 relative to pre-2000 values (see p102 of SoIB 2023 report)",
  "Long-term Trend UCI", "Upper limit of 95% confidence interval of modelled estimate of the change in abundance in 2024-25 relative to pre-2000 values (see p102 of SoIB 2023 report)",
  "Current Annual Trend LCI", "Lower limit of 95% confidence interval of modelled estimate of the mean annual change in abundance over the past 8 years (see p102 of SoIB 2023 report)",
  "Current Annual Trend Mean", "Modelled estimate of the mean annual change in abundance over the past 8 years (see p102 of SoIB 2023 report)",
  "Current Annual Trend UCI", "Upper limit of 95% confidence interval of modelled estimate of the mean annual change in abundance over the past 8 years (see p102 of SoIB 2023 report)",
  "Distribution Range Size LCI", "Lower limit of 95% confidence interval of modelled estimate of Distribution Range Size (see p102 of SoIB 2023 report)",
  "Distribution Range Size Mean", "Modelled estimate of Distribution Range Size (see p102 of SoIB 2023 report)",
  "Distribution Range Size UCI", "Upper limit of 95% confidence interval of modelled estimate of Distribution Range Size (see p102 of SoIB 2023 report)",
  "State Where Species Key", "Whether species is one of the key species for the current state (see p20 of SoIB 2023 report for details)",
  "Number of Grids", glue("Number of 25 km x 25 km grid cells from which the species reported over time (total {n_distinct(g1_in_sf$GRID.G1)})"),
  "Range Coverage (Pre-2000)", "Percentage of the 'Total Range' (see above) of the species which was sampled before the year 2000",
  "Range Coverage (Current)", "Average across 2015\u20132025 of percentage of the 'Total Range' (see above) which was sampled every year",
  "Range Coverage (2024)", "Percentage of the 'Total Range' (see above) which was sampled in the year 2024",
  "Grid Coverage Mean", "Average across all 25 km x 25 km cells with the species, of percentage of sampled 5 km x 5 km subcells within each 25 km x 25 km cell (a maximum of 25)",
  "Grid Coverage CI", "95% confidence interval across all 25 km x 25 km cells with the species, of percentage of sampled 5 km x 5 km subcells within each 25 km x 25 km cell (a maximum of 25)",
  "Projected % Decline in 3 Generations", "Decline in three generations of species, projected from SoIB 2023 analysis",
  "Regional Red List Category", "Regional Red List threat status category proposed from SoIB 2023 analysis based on IUCN criterion A (decline in three generations)",
  "SoIB 2020 Concern Status", "Conservation Concern Status of species from SoIB 2020 assessment",
  "SoIB 2020 Long-term Trend Status", "Long-term Trend Status of species from SoIB 2020 assessment",
  "SoIB 2020 Current Annual Trend Status", "Current Annual Trend Status of species from SoIB 2020 assessment",
  "SoIB 2020 Distribution Range Size Status", "Distribution Range Size Status of species from SoIB 2020 assessment"
  
) %>% 
  left_join(readme_datatype, by = "Field") %>% 
  left_join(readme_nat_excl, by = "Field") %>% 
  left_join(readme_range, by = "Field") %>% 
  relocate(Meaning, .after = last_col()) %>% 
  mutate(`Range (min, max)` = case_when(Class == "character" ~ "", 
                                        # converting logical ranges (0, 1) to TRUE/FALSE
                                        Class == "logical" ~ "TRUE, FALSE",
                                        TRUE ~ `Range (min, max)`)) %>% 
  mutate(across(c(Class, `Range (min, max)`), 
                ~ replace_na(., ""))) %>% 
  rename(`Field Name` = Field,
         Description = Meaning)

# for website table
write_xlsx(x = readme[-(1:4), c("Field Name", "Description")],
           path = "20_website/SoIB_2025_update_main_readme_forweb.xlsx")


# main_db_21_india <- main_db_21 %>%
#   filter(MASK.LABEL == "India" & !is.na(`Long-term Trend Mean`))



# files_df <- tibble(
#   file = list.files(
#     "02_graphs/01_single/01_none/long-term trends/",
#     pattern = "_in_LTT_trend\\.png$",
#     full.names = FALSE
#   )
# ) %>%
#   mutate(
#     species_clean = file %>%
#       str_remove("_in_LTT_trend\\.png$") %>%
#       str_replace_all("-", " ")
#   )
# 
# species_missing_files <- main_db_21_india %>%
#   mutate(species_clean = str_replace_all(`English Name`, "-", " ")) %>%
#   anti_join(
#     files_df,
#     by = "species_clean"
#   )
# 
# 
# species_missing_files_1 <- species_missing_files %>%
#   dplyr::select(c(`English Name`, India.Checklist.Common.Name.2025))
# writing -----------------------------------------------------------------

# ordering sheets by mask
main_db_split <- main_db_21 %>% 
  # get named list of dfs
  split(.$MASK.LABEL)

split_order <- data.frame(MASK.LABEL = names(main_db_split)) %>% 
  rownames_to_column("ID") %>% 
  left_join(get_metadata() %>% join_mask_codes()) %>% 
  arrange(MASK.ORDERED) %>%
  pull(ID) %>% 
  as.numeric()

main_db_split <- main_db_split[split_order] %>% 
  # removing empty/NA columns
  # removing the column of mask name
  map(~ .x %>% 
        dplyr::select(-where(~ all(is.na(.)))) %>% 
        dplyr::select(-MASK.LABEL))

c(list(README = readme), main_db_split) %>% 
  write_xlsx(path = "20_website/02_SoIB_2025_main_v0.xlsx")


# writing individually for archive

if (!dir.exists("20_website/archive/")) {
  dir.create("20_website/archive/")
}

map2(main_db_split, names(main_db_split), ~ {
  write_csv(.x, glue("20_website/archive/SoIB 2023 {.y}.csv"))
})

# Create separate CSV files for each workbook sheet

# create one CSV per MASK value

library(stringr)

out_dir <- "20_website/zenodo_files"

if (!dir.exists(out_dir)) {
  dir.create(out_dir)
}

for (m in unique(main_db_21$MASK.LABEL)) {
  
  out <- main_db_21[main_db_21$MASK.LABEL == m, ]
  out <- out %>% 
    # removing empty/NA columns
    # removing the column of mask name
          dplyr::select(-where(~ all(is.na(.)))) %>% 
          dplyr::select(-MASK.LABEL)
  
  safe_mask <- str_replace_all(m, "\\s+", "_")
  
  write.csv(
    out,
    file = file.path(out_dir, str_c("SoIB_2025_", safe_mask, "_v0.csv")),
    row.names = FALSE
  )
}

for (m in unique(main_db_21$MASK)) {
  
  out <- main_db_21[main_db_21$MASK == m, ]
  out$MASK <- NULL
  
  write.csv(
    out,
    file = paste0(m, ".csv"),
    row.names = FALSE
  )
}



# ebird_checklist_2025_tmp <- ebird_checklist_2025 %>%
#   filter(English.name %in%
#            (
#              main_db_6 %>%
#                filter(is.na(India.Checklist.Scientific.Name)) %>%
#                pull(`English Name`)
#            )) %>%
#   dplyr::select(English.name)
# # These are the corresponding eBird 2025 names for the 3 species we are
# # interested in
# 
# india_checklist_scientific_names <- ebird_checklist_2025_tmp %>%
#   left_join(india_ebird_map_2025 %>%
#               dplyr::select(c(eBird.English.Name.2025, 
#                               Scientific.Name,
#                               WPA.Schedule,
#                               IUCN.RedList,
#                               CMS.Appendix,
#                               CITES.Appendix)),
#             by = join_by(English.name == eBird.English.Name.2025))
# 
# 
# 
# main_db_7 <- main_db_6 %>%
#   left_join(india_checklist_scientific_names,
#             by = join_by(`eBird English Name 2024` == English.name))
# 
# 
# main_db_8 <- main_db_7 %>%
#   mutate(`India.Checklist.Scientific.Name` = case_when(
#       !is.na(`India.Checklist.Scientific.Name`)  ~ `India.Checklist.Scientific.Name`,
#       TRUE ~ Scientific.Name),
#       `IUCN Category` = case_when(
#         !is.na(`IUCN Category`)  ~ `IUCN Category`,
#         TRUE ~ IUCN.RedList),
#       `WPA Schedule` = case_when(
#         !is.na(`WPA Schedule`)  ~ `WPA Schedule`,
#         TRUE ~ WPA.Schedule),
#       `CMS Appendix` = case_when(
#         !is.na(`CMS Appendix`)  ~ `CMS Appendix`,
#         TRUE ~ CMS.Appendix),
#       `CITES Appendix` = case_when(
#         !is.na(`CITES Appendix`)  ~ `CITES Appendix`,
#         TRUE ~ CITES.Appendix)
#       )
#   
# which(is.na(main_db_8$India.Checklist.Common.Name))
# which(is.na(main_db_8$India.Checklist.Scientific.Name))




#-----


# main_db_7 <- main_db_6 %>% 
#   left_join(tax_map %>%
#               dplyr::select(c(eBird.English.Name.2024,
#                               eBird.English.Name.2025)) %>% unique(),
#             by = join_by(`eBird English Name 2024` == eBird.English.Name.2024))


# Check if any eBird 2025 English names are not pulled from the taxonomy mapping

# tmp <- main_db_7 %>%
#   slice(which(is.na(eBird.English.Name.2025)))

# Pull 
main_db_7 <- main_db_6 %>%
  left_join(india_ebird_map_2025 %>%
              dplyr::select(c(English.Name, # 2025 India common name
                              Scientific.Name, # 2025 India scientific name
                              IUCN.RedList,
                              WPA.Schedule,
                              CMS.Appendix,
                              CITES.Appendix,
                              eBird.English.Name.2025,
                              eBird.Scientific.Name.2025)),
            by = join_by(`English Name` == English.Name))

which(is.na(main_db_7$eBird.English.Name.2025))

tmp <- main_db_7 %>% slice(which(is.na(main_db_7$eBird.English.Name.2025)))


species <- main_db_7 %>% slice(which(is.na(main_db_7$eBird.English.Name.2025))) %>%
  pull(`English Name`) %>% unique()

# Get the eBird common name 2025 for these species from the taxonomy file




# This step is not a problem because 

main_db_8 <- main_db_7 %>%
  mutate(`India.Checklist.Scientific.Name` = case_when(
    !is.na(`India.Checklist.Scientific.Name`)  ~ `India.Checklist.Scientific.Name`,
    TRUE ~ Scientific.Name),
    `IUCN Category` = case_when(
      !is.na(`IUCN Category`)  ~ `IUCN Category`,
      TRUE ~ IUCN.RedList),
    `WPA Schedule` = case_when(
      !is.na(`WPA Schedule`)  ~ `WPA Schedule`,
      TRUE ~ WPA.Schedule),
    `CMS Appendix` = case_when(
      !is.na(`CMS Appendix`)  ~ `CMS Appendix`,
      TRUE ~ CMS.Appendix),
    `CITES Appendix` = case_when(
      !is.na(`CITES Appendix`)  ~ `CITES Appendix`,
      TRUE ~ CITES.Appendix)
    )

main_db_9 <- main_db_8 %>%
  dplyr::select(c(
    -`English Name`,
    -`Scientific Name`,
    -"Scientific.Name",
    -"IUCN.RedList",
    -"WPA.Schedule",
    -"CMS.Appendix",
    -"CITES.Appendix",
    -`eBird English Name 2024`,
    -`eBird Scientific Name 2024`,
    -`BLI English Name 2024`,
    -`BLI Scientific Name 2024`,
    -eBird.English.Name.2022,
    -eBird.Scientific.Name.2022,
    -India.Checklist.Common.Name.2022,
    -India.Checklist.Scientific.Name.2022,
    -India.Checklist.Common.Name.2024,
    -India.Checklist.Scientific.Name.2024)) %>%
  rename(
        `English Name` = `India.Checklist.Common.Name`,
        `Scientific Name` = `India.Checklist.Scientific.Name`
      ) %>%
      relocate(`English Name`, `Scientific Name`)
  

tmp <- main_db_9 %>% filter(if_any(c(`English Name`, `Scientific Name`), is.na))


which(is.na(tmp$`IUCN Category`))

which(is.na(tmp$`WPA Schedule`))

which(is.na(main_db_9$eBird.English.Name.2025))

which(is.na(main_db_9$eBird.Scientific.Name.2025))


# Pull India checklist names, BLI names and rearrange

# Use eBird 2025 names to pull India checklist names

main_db_10 <- main_db_9 %>%
  left_join(india_ebird_map_2025 %>%
              dplyr::select(c(English.Name, eBird.English.Name.2025)),
            by = "eBird.English.Name.2025")

which(is.na(main_db_10$English.Name))

tmp <- main_db_10 %>% slice(which(is.na(main_db_10$English.Name)))

main_db_11 <- main_db_10 %>%
  left_join(india_ebird_map_2025 %>%
              dplyr::select(c(Scientific.Name, eBird.Scientific.Name.2025)),
            by = "eBird.Scientific.Name.2025")

which(is.na(main_db_11$Scientific.Name))

# If any, pull the English names directly from the eBird 2025 checklist. 
# But first, we have to make sure that the English names have not changed 
# between 2024 and 2025. Only then can we 

tmp2 <- tmp %>%
  left_join(ebird_checklist_2025 %>%
              dplyr::select(English.name, scientific.name, sort_v2024),
            by = join_by(`eBird English Name 2024`== English.name)) %>% 
  filter(is.na(sort_v2024))

# If NA, all eBird english names are present in the 2025 checklist. So pull
# the scientific names and English names

main_db_8 <- main_db_7 %>%
  left_join(ebird_checklist_2025 %>%
              dplyr::select(English.name),
            by = join_by(`eBird English Name 2024` == English.name))


india_checklist_2025 <- read.csv("00_data/india_checklist_v10.csv", header = T)

main_db_7 <- main_db_6 %>%
  left_join(india_checklist_2025 %>%
              dplyr::select(c(English.Name,
                              Scientific.Name,
                              IUCN.RedList,
                              CMS.Appendix,
                              CITES.Appendix)),
            by = join_by(India.Checklist.Common.Name == English.Name), # This assumes that the common names do not change in the India Checklist
            keep = TRUE)

main_db_7 <- main_db_6 %>%
  left_join(india_checklist_2025 %>%
              dplyr::select(c(English.Name,
                              Scientific.Name,
                              IUCN.RedList,
                              CMS.Appendix,
                              CITES.Appendix)),
            by = join_by(India.Checklist.Common.Name == English.Name))


# Rename columns from the 2025 checklist as such

main_db_7 <- main_db_7 %>%
  rename(India.Checklist.Scientific.Name.2025 = Scientific.Name,
         India.Checklist.Common.Name.2025 = English.Name)

tmp <- which(is.na(main_db_7$India.Checklist.Scientific.Name.2025))


main_db_8 <- main_db_7 %>%
  mutate(India.Checklist.Scientific.Name = case_when(is.na(India.Checklist.Scientific.Name) ~ Scientific.Name,
                                                     TRUE ~ India.Checklist.Scientific.Name),
         `IUCN Category` = case_when(is.na(`IUCN Category`) ~ IUCN.RedList,
                                     TRUE ~ `IUCN Category`),
         `CMS Appendix` = case_when(is.na(`CMS Appendix`) ~ CMS.Appendix,
                                    TRUE ~ `CMS Appendix`),
         `CITES Appendix` = case_when(is.na(`CITES Appendix`) ~ CITES.Appendix,
                                    TRUE ~ `CITES Appendix`))

# Some checks  
tmp <- main_db_8 %>% filter(is.na(India.Checklist.Scientific.Name))

tmp <- main_db_8 %>% filter(is.na(India.Checklist.Common.Name))

tmp <- main_db_8 %>% filter(is.na(`IUCN Category`))
  
tmp <- main_db_8 %>% filter(is.na(`CMS Appendix`))

tmp <- main_db_8 %>% filter(is.na(`CITES Appendix`))

# Remove some columns and move India.Checklist.Common.Name and 
# India.Checklist.Scientific.Name to the front of the table

# main_db_8 <- main_db_7 %>%
#   dplyr::select(
#     -"English Name",
#     -"Scientific Name",
#     -"eBird English Name 2024", 
#     -"eBird Scientific Name 2024",
#     -"BLI English Name 2024",
#     -"BLI Scientific Name 2024",
#     -"eBird.English.Name.2022",
#     -"eBird.Scientific.Name.2022"
#   ) %>%
#   rename(
#     `English Name` = `India.Checklist.Common.Name.2022`,
#     `Scientific Name` = `India.Checklist.Scientific.Name.2022`
#   ) %>%
#   relocate(`English Name`, `Scientific Name`)

  
# main_db_6 <- main_db_5 %>%
#   mutate(`English Name` = case_when(
#     !is.na(`India.Checklist.Common.Name.2022`)  ~ `India.Checklist.Common.Name.2022`,
#     TRUE ~ `India.Checklist.Common.Name.2024`),
#     `Scientific Name` = case_when(
#       !is.na(`India.Checklist.Scientific.Name.2022`)  ~ `India.Checklist.Scientific.Name.2022`,
#       TRUE ~ `India.Checklist.Scientific.Name.2024`))




# Use taxonomy mapping to pull eBird names from the 2025 taxonomy

main_db_9 <- main_db_8 %>% 
  left_join(tax_map %>%
              dplyr::select(c(eBird.English.Name.2024,
                              eBird.English.Name.2025)) %>% unique(),
            by = join_by(`eBird English Name 2024` == eBird.English.Name.2024))

# main_db_9 <- main_db_8 %>% 
#   left_join(tax_map %>%
#               dplyr::select(c(eBird.English.Name.2024,
#                               eBird.English.Name.2025)) %>% unique(),
#                             by = join_by(`English Name` == eBird.English.Name.2024))
# 
# which(is.na(main_db_4$`English Name`))

# Use the 2025 eBird names to pull 2025 India Checklist names and eBird scientific
# names

main_db_5 <- main_db_4 %>% 
  left_join(india_ebird_map_2025 %>%
              dplyr::select(c(eBird.English.Name.2025,
                              eBird.Scientific.Name.2025,
                              English.Name,
                              Scientific.Name
                              )),
            by = "eBird.English.Name.2025")

# Pull the BLI names

main_db_6 <- main_db_5 %>% 
  left_join(india_bli_map_2025 %>%
              dplyr::select(c(BLI.Common.Name.2025,
                              BLI.Scientific.Name.2025,
                              English.Name,
                              Scientific.Name
              )),
            by = c("English.Name", "Scientific.Name"))

# Rename the English and Scientific Names as India Checklist 2025 names

main_db_7 <- main_db_6 %>%
  rename(`India.Checklist.Common.Name.2025` = `English.Name`,
         `India.Checklist.Scientific.Name.2025` = `Scientific.Name`)

# Remove the 2024 names and replace them with 2025 names

main_db_8 <- main_db_7 %>%
  dplyr::select(
    -"English Name",
    -"Scientific Name",
    -"eBird English Name 2024", 
    -"eBird Scientific Name 2024",
    -"BLI English Name 2024",
    -"BLI Scientific Name 2024",
    -"eBird.English.Name.2022",
    -"eBird.Scientific.Name.2022"
  ) %>%
  rename(
    `English Name` = `India.Checklist.Common.Name.2022`,
    `Scientific Name` = `India.Checklist.Scientific.Name.2022`
  ) %>%
  relocate(`English Name`, `Scientific Name`)

# Pull Order from India Checklist 2025

main_db_9 <- main_db_8 %>%
  left_join(india_checklist_2025 %>%
              dplyr::select(c(English.Name,
                              Scientific.Name,
                              Order)),
            by = join_by(India.Checklist.Common.Name.2025 == English.Name,
                         India.Checklist.Scientific.Name.2025 == Scientific.Name))

# Pull Order from India Checklist 2025

main_db_10 <- main_db_9 %>%
  left_join(ebird_checklist_2025 %>%
              dplyr::select(c(English.name,
                              scientific.name,
                              family)),
            by = join_by(eBird.English.Name.2025 == English.name,
                         eBird.Scientific.Name.2025 == scientific.name))

main_db_11 <- main_db_10 %>%
  rename(`Family` = `family`)

# Reorder some columns

main_db_12 <- main_db_11 %>%
  dplyr::select(c(`English Name`,
                  `Scientific Name`,
                  `SoIB 2023 Priority Status`,
                  `SoIB 2023 Long-term Trend Status`,
                  `SoIB 2023 Current Annual Trend Status`,
                  `SoIB 2023 Distribution Range Size Status`,
                  eBird.English.Name.2025,
                  eBird.Scientific.Name.2025,
                  BLI.Common.Name.2025,
                  BLI.Scientific.Name.2025,
                  India.Checklist.Common.Name.2025,
                  India.Checklist.Scientific.Name.2025,
                  Order,
                  Family,
                  `Breeding Activity Period`,
                  `Non-breeding Activity Period`,
                  `Diet Guild`,
                  Endemicity,
                  `Endemic to India`,
                  `Endemic to India`,
                  `Endemic to Subcontinent`,
                  `Endemic to Himalaya`,
                  `Habitat Specialization`,
                  `Migratory Status within India`,
                  `Restricted to Islands`,
                  `IUCN Category`,
                  `WPA Schedule`,
                  `CITES Appendix`,
                  `CMS Appendix`,
                  `1% Population Threshold`,
                  `Selected for SoIB`,
                  `Selected for Long-term Trend`,
                  `Selected for Current Annual Trend`,
                  `Long-term Trend LCI`,
                  `Long-term Trend Mean`,
                  `Long-term Trend UCI`,
                  `Current Annual Trend LCI`,
                  `Current Annual Trend Mean`,
                  `Current Annual Trend UCI`,
                  `Distribution Range Size LCI`,
                  `Distribution Range Size Mean`,
                  `Distribution Range Size UCI`,
                  `State Where Species Key`,
                  `Number of Grids`,
                  `Range Coverage (Pre-2000)`,
                  `Range Coverage (Current)`,
                  `Range Coverage (2024)`,
                  `Grid Coverage Mean`,
                  `Grid Coverage CI`,
                  `Projected % Decline in 3 Generations`,
                  `Regional Red List Category`,
                  `SoIB 2020 Concern Status`,
                  `SoIB 2020 Long-term Trend Status`,
                  `SoIB 2020 Current Annual Trend Status`,
                  `SoIB 2020 Distribution Range Size Status`,
                  MASK.LABEL))
  
# README ------------------------------------------------------------------

# info about data types
readme_datatype <- main_db_12 %>% 
  mutate(`Range Coverage CI (Current)` = 0) %>% 
  dplyr::select(-MASK.LABEL) %>% 
  reframe(across(everything(), ~ class(.))) %>% 
  pivot_longer(everything(), names_to = "Field", values_to = "Class")

# range of values
readme_range <- main_db_12 %>% 
  mutate(`Range Coverage CI (Current)` = NA) %>% 
  dplyr::select(-MASK.LABEL) %>% 
  reframe(across(!where(is.factor),
                 ~ range(na.omit(.)) %>% str_flatten_comma()),
          across(where(is.factor),
                 ~ c(first(levels(.)), 
                     last(levels(.))) %>% str_flatten_comma())) %>% 
  distinct() %>% 
  pivot_longer(everything(), names_to = "Field", values_to = "Range (min, max)") %>% 
  mutate(`Range (min, max)` = case_when(Field == "Range Coverage CI (Current)" ~ NA, 
                                        TRUE ~ `Range (min, max)`))

# which fields are only for national sheet?
readme_nat_excl <- main_db_12 %>% 
  mutate(NATIONAL = ifelse(MASK.LABEL == "India", TRUE, FALSE)) %>% 
  group_by(NATIONAL) %>% 
  reframe(across(everything(), ~ all(is.na(.)))) %>% 
  filter(NATIONAL == FALSE) %>% 
  dplyr::select(-NATIONAL) %>% 
  pivot_longer(everything(), names_to = "Field", values_to = "Exclusive to National")

readme <- tribble(
  ~ Field, ~ Meaning,
  
  "", "",
  "NOTE: Below is information about the superset of fields across all the sheets. Some fields are not applicable and hence are absent in subnational sheets (all except 'India'; see column 'Exclusive to National'). For example, SoIB 2023 Distribution Range Size Status assignment was done only at the national level.", "",
  "NOTE: India sheet contains all 1357 species in India Checklist v7.1 (https://indianbirds.in/india). Subnational sheets contain only those species whose corresponding subnational assessment was done. Note that while the primary source of species list is the v10 India checklist, the English Names and Scientific Names are for species common to both v10 and v7.1 are retained from v7.1 for continuity with previous xlsx file. ", "",
  "", "",
  
  "English Name", "English name of species in India Checklist v7.1 (https://indianbirds.in/india)",
  "Scientific Name", "Scientific name of species in India Checklist v7.1 (https://indianbirds.in/india)",
  "SoIB 2023 Priority Status", "Conservation Priority Status of species from SoIB 2023 national-level assessment",
  "SoIB 2023 Long-term Trend Status", "Long-term Trend Status of species from SoIB 2023 assessment",
  "SoIB 2023 Current Annual Trend Status", "Current Annual Trend Status of species from SoIB 2023 assessment",
  "SoIB 2023 Distribution Range Size Status", "Distribution Range Size Status of species assigned from SoIB 2023 assessment",
  "eBird English Name 2024", "English name of species in eBird/Clements Checklist 2024",
  "eBird Scientific Name 2024", "Scientific name of species in eBird/Clements Checklist 2024",
  "BLI English Name 2024", "English name of species in HBW/BLI Checklist 2024",
  "BLI Scientific Name 2024", "Scientific name of species in HBW/BLI Checklist 2024",
  "Order", "Taxonomic Order to which species belongs",
  "Family", "Taxonomic Family to which species belongs",
  "Breeding Activity Period", "Breeding period of species, based on Wilman et al. 2014",
  "Non-breeding Activity Period", "Non-breeding period of species, based on Wilman et al. 2014",
  "Diet Guild", "Diet guild of species, based on Wilman et al. 2014",
  "Endemicity", "Endemicity of species adapted from India Checklist v7.1 (https://indianbirds.in/india)",
  "Endemic to India", "Whether species is endemic to India",
  "Endemic to Subcontinent", "Whether species is endemic to the Indian subcontinent",
  "Endemic to Himalaya", "Whether species is endemic to the Himalaya",
  "Habitat Specialization", "Habitat specialization of species, based on Wilman et al. 2014",
  "Migratory Status within India", "Migratory status of species within India, assigned based on multiple sources",
  "Restricted to Islands", "Whether species is restricted to the islands of India",
  "IUCN Category", "IUCN threat status category of species, based on India Checklist v10 (https://indianbirds.in/india)",
  "WPA Schedule", "WPA Schedule of species, based on India Checklist v10 (https://indianbirds.in/india)",
  "CITES Appendix", "CITES Appendix category of species, based on India Checklist v10 (https://indianbirds.in/india)",
  "CMS Appendix", "CMS Appendix category of species, based on India Checklist v10 (https://indianbirds.in/india)",
  "1% Population Threshold", "Wetlands International estimate of the 1% biogeographic population size (individuals) of a waterbird species",
  "Selected for SoIB", "Whether species was selected for SoIB 2023 analyses",
  "Selected for Long-term Trend", "Whether species was selected for Long-term Trend analysis in SoIB 2023",
  "Selected for Current Annual Trend", "Whether species was selected for Current Annual Trend analysis in SoIB 2023",
  "Long-term Trend LCI", "Lower limit of 95% confidence interval of modelled estimate of the change in abundance in 2022-23 relative to pre-2000 values (see p102 of SoIB 2023 report)",
  "Long-term Trend Mean", "Modelled estimate of the change in abundance in 2022-23 relative to pre-2000 values (see p102 of SoIB 2023 report)",
  "Long-term Trend UCI", "Upper limit of 95% confidence interval of modelled estimate of the change in abundance in 2022-23 relative to pre-2000 values (see p102 of SoIB 2023 report)",
  "Current Annual Trend LCI", "Lower limit of 95% confidence interval of modelled estimate of the mean annual change in abundance over the past 8 years (see p102 of SoIB 2023 report)",
  "Current Annual Trend Mean", "Modelled estimate of the mean annual change in abundance over the past 8 years (see p102 of SoIB 2023 report)",
  "Current Annual Trend UCI", "Upper limit of 95% confidence interval of modelled estimate of the mean annual change in abundance over the past 8 years (see p102 of SoIB 2023 report)",
  "Distribution Range Size LCI", "Lower limit of 95% confidence interval of modelled estimate of Distribution Range Size (see p102 of SoIB 2023 report)",
  "Distribution Range Size Mean", "Modelled estimate of Distribution Range Size (see p102 of SoIB 2023 report)",
  "Distribution Range Size UCI", "Upper limit of 95% confidence interval of modelled estimate of Distribution Range Size (see p102 of SoIB 2023 report)",
  "State Where Species Key", "Whether species is one of the key species for the current state (see p20 of SoIB 2023 report for details)",
  "Number of Grids", glue("Number of 25 km x 25 km grid cells from which the species reported over time (total {n_distinct(g1_in_sf$GRID.G1)})"),
  "Range Coverage (Pre-2000)", "Percentage of the 'Total Range' (see above) of the species which was sampled before the year 2000",
  "Range Coverage (Current)", "Average across 2015\u20132025 of percentage of the 'Total Range' (see above) which was sampled every year",
  "Range Coverage CI (Current)", "(COMING SOON...) 95% confidence interval across 2015\u20132023 of percentage of the 'Total Range' (see above) which was sampled every year",
  "Range Coverage (2024)", "Percentage of the 'Total Range' (see above) which was sampled in the year 2024",
  "Grid Coverage Mean", "Average across all 25 km x 25 km cells with the species, of percentage of sampled 5 km x 5 km subcells within each 25 km x 25 km cell (a maximum of 25)",
  "Grid Coverage CI", "95% confidence interval across all 25 km x 25 km cells with the species, of percentage of sampled 5 km x 5 km subcells within each 25 km x 25 km cell (a maximum of 25)",
  "Projected % Decline in 3 Generations", "Decline in three generations of species, projected from SoIB 2023 analysis",
  "Regional Red List Category", "Regional Red List threat status category proposed from SoIB 2023 analysis based on IUCN criterion A (decline in three generations)",
  "SoIB 2020 Concern Status", "Conservation Concern Status of species from SoIB 2020 assessment",
  "SoIB 2020 Long-term Trend Status", "Long-term Trend Status of species from SoIB 2020 assessment",
  "SoIB 2020 Current Annual Trend Status", "Current Annual Trend Status of species from SoIB 2020 assessment",
  "SoIB 2020 Distribution Range Size Status", "Distribution Range Size Status of species from SoIB 2020 assessment"
  
) %>% 
  left_join(readme_datatype, by = "Field") %>% 
  left_join(readme_nat_excl, by = "Field") %>% 
  left_join(readme_range, by = "Field") %>% 
  relocate(Meaning, .after = last_col()) %>% 
  mutate(`Range (min, max)` = case_when(Class == "character" ~ "", 
                                        # converting logical ranges (0, 1) to TRUE/FALSE
                                        Class == "logical" ~ "TRUE, FALSE",
                                        TRUE ~ `Range (min, max)`)) %>% 
  mutate(across(c(Class, `Range (min, max)`), 
                ~ replace_na(., ""))) %>% 
  rename(`Field Name` = Field,
         Description = Meaning)

# for website table
write_xlsx(x = readme[-(1:4), c("Field Name", "Description")],
           path = "20_website/SoIB_2025_update_main_readme_forweb.xlsx")


# writing -----------------------------------------------------------------

# At this point, the English Names and Scientific Names reflect the 2025 taxonomy
# from India Checklist. 
# But to maintain continuity on the website, we will revert to the 2023 names.

main_db_1 <- main_db %>% left_join(tax_map %>%
                                   dplyr::select(c("eBird.English.Name.2022", "eBird.English.Name.2024")),
                                   by = join_by(`eBird English Name 2024` == eBird.English.Name.2024))



# Note 1: Pay close attention to species which have been lumped in 2025.

# For example, Striated Swallow and Red-rumped Swallow have been lumped into 
# one species - Eastern Red-rumped Swallow in 2025. But reverting to 2023 names 
# means that we show both Striated and Red-rumped swallow.

# Note 2: 

# There are new species in 2025 which have no corresponding name in 2023. These 
# will remain the 2024 names (example: Barnacle goose)

# removing striated heron

# main_db_2 <- main_db_1 %>% filter(is.na(eBird.English.Name.2022) | eBird.English.Name.2022 != "Striated Swallow")

# Use soib_mapping_2022 to pull the corresponding 2022 India Checklist names

soib_mapping_2022 <- read.csv("00_data/SoIB_mapping_2022.csv", header = T)

main_db_2 <- main_db_1 %>% left_join(soib_mapping_2022 %>%
                                       dplyr::select("eBird.English.Name.2022",
                                                     "India.Checklist.Common.Name",
                                                     "India.Checklist.Scientific.Name"),
                                     by = "eBird.English.Name.2022")

# Rename the India checklist common and scientific name such that it is clear
# that they are from 2023

main_db_3 <- main_db_2 %>%
  rename(`India.Checklist.Common.Name.2022` = `India.Checklist.Common.Name`,
         `India.Checklist.Scientific.Name.2022` = `India.Checklist.Scientific.Name`)


# Now pull the India checklist common and scientific name from 2025 for species
# like Barnacle goose which do no exist in 2023

soib_mapping_2024 <- read.csv("00_data/SoIB_mapping_2024.csv", header = T)

main_db_4 <- main_db_3 %>% left_join(SoIB_mapping_2024 %>%
                                       dplyr::select("eBird.English.Name.2024",
                                                     "India.Checklist.Common.Name",
                                                     "India.Checklist.Scientific.Name"),
                                     by = join_by(`eBird English Name 2024` == "eBird.English.Name.2024"))

main_db_5 <- main_db_4 %>%
  rename(`India.Checklist.Common.Name.2024` = `India.Checklist.Common.Name`,
         `India.Checklist.Scientific.Name.2024` = `India.Checklist.Scientific.Name`)


# At this 


main_db_6 <- main_db_5 %>%
  mutate(`India.Checklist.Common.Name` = case_when(
    !is.na(`India.Checklist.Common.Name.2022`)  ~ `India.Checklist.Common.Name.2022`,
    TRUE ~ `India.Checklist.Common.Name.2024`),
    `India.Checklist.Scientific.Name` = case_when(
      !is.na(`India.Checklist.Scientific.Name.2022`)  ~ `India.Checklist.Scientific.Name.2022`,
      TRUE ~ `India.Checklist.Scientific.Name.2024`))
    

    
main_db_7 <- main_db_6 %>%
  dplyr::select(
    -"English Name",
    -"Scientific Name",
    -"eBird.English.Name.2022",
    -"India.Checklist.Common.Name.2022",
    -"India.Checklist.Scientific.Name.2022",
    -"India.Checklist.Common.Name.2024",        
    -"India.Checklist.Scientific.Name.2024" 
  ) %>%
  rename(
    `English Name` = `India.Checklist.Common.Name`,
    `Scientific Name` = `India.Checklist.Scientific.Name`
  ) %>%
  relocate(`English Name`, `Scientific Name`)

# SoIB_mapping_2022 <- read.csv("00_data/SoIB_mapping_2022.csv", header = T)
# 
# main_db_3 <- main_db_2 %>% left_join(SoIB_mapping_2022 %>%
#                                        dplyr::select("eBird.English.Name.2022",
#                                                      "India.Checklist.Common.Name",
#                                                      "India.Checklist.Scientific.Name"),
#                                      by = "eBird.English.Name.2022")
# 
# 
# main_db_4 <- main_db_3 %>%
#   dplyr::select(
#     -`English Name`,
#     -`Scientific Name`,
#     -`eBird.English.Name.2022`
#   ) %>%
#   rename(
#     `English Name` = `India.Checklist.Common.Name`,
#     `Scientific Name` = `India.Checklist.Scientific.Name`
#   ) %>%
#   relocate(`English Name`, `Scientific Name`)


# ordering sheets by mask
main_db_split <- main_db_7 %>% 
  # get named list of dfs
  split(.$MASK.LABEL)

split_order <- data.frame(MASK.LABEL = names(main_db_split)) %>% 
  rownames_to_column("ID") %>% 
  left_join(get_metadata() %>% join_mask_codes()) %>% 
  arrange(MASK.ORDERED) %>%
  pull(ID) %>% 
  as.numeric()

main_db_split <- main_db_split[split_order] %>% 
  # removing empty/NA columns
  # removing the column of mask name
  map(~ .x %>% 
        dplyr::select(-where(~ all(is.na(.)))) %>% 
        dplyr::select(-MASK.LABEL))

c(list(README = readme), main_db_split) %>% 
  write_xlsx(path = "20_website/SoIB_2025_update_main.xlsx")


# writing individually for archive

if (!dir.exists("20_website/archive/")) {
  dir.create("20_website/archive/")
}

map2(main_db_split, names(main_db_split), ~ {
  write_csv(.x, glue("20_website/archive/SoIB 2023 {.y}.csv"))
})
