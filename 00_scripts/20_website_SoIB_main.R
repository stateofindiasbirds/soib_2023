# compile all SoIB_main files into single file to upload on website (and download from)

require(tidyverse)
require(glue)
require(writexl)

load("00_data/maps_sf.RData") # for no. of cells in README
load("00_data/current_soib_migyears.RData")
load("00_data/analyses_metadata.RData")
source("00_scripts/00_functions.R")
source("00_scripts/20_functions.R")

interannual_update = TRUE
major_update = 2022
use_major_update = TRUE

# key states for each species
keystates <- read.csv("01_analyses_full/results/key_state_species_full.csv") %>% 
  arrange(ST_NM, India.Checklist.Common.Name) 

if (interannual_update == T)
{
  major_update_map = read.csv(paste("00_data/SoIB_mapping_",major_update,".csv",sep="")) %>%
    dplyr::select(India.Checklist.Common.Name,eBird.English.Name.2022)
  latest_map = read.csv(paste("00_data/SoIB_mapping_",latest_soib_my,".csv",sep=""))
  tax_map = read.csv("00_data/eBird_taxonomy_mapping.csv")
  
  keystates = keystates %>%
    left_join(major_update_map) %>%
    left_join(tax_map) %>%
    dplyr::select(ST_NM, eBird.English.Name.2024)
}


# import ----------------------------------------------------------------------------

# importing all data and setting up
main_db0 <- map2(get_metadata()$SOIBMAIN.PATH, get_metadata()$MASK, 
                 ~ read_fn(.x) %>% bind_cols(tibble(MASK = .y))) %>% 
  list_rbind()

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
  "Kentish Plover", "Spot-winged Starling", "Common Teal", "Garganey", "Great Grey Shrike",
  "Blue Rock Thrush", "Tibetan Sand Plover", "Little Ringed Plover", "Indian Roller",
  "Common Sandpiper"
)

# to change after new redlist.csv is produced
redlist <- read_csv("01_analyses_full/results/redlist.csv") %>%
  left_join(major_update_map, by = c("Species" = "India.Checklist.Common.Name")) %>%
  left_join(tax_map) %>%
  dplyr::select(-Species, -eBird.English.Name.2022, -eBird.English.Name.2023)

redlist <- redlist %>% 
  filter(`3GEN` <= 14) %>% 
  dplyr::select("eBird.English.Name.2024", "3GEN Decline", "Redlist Category Proposed") %>% 
  ### TEMP: correcting Near-threatened (already corrected in source script)
  mutate(`Redlist Category Proposed` = case_when(
    `Redlist Category Proposed` == "Near-threatened" ~ "Near Threatened",
    TRUE ~ `Redlist Category Proposed`
  )) %>% 
  # only list final 15 proposed in report
  mutate(`Redlist Category Proposed` = case_when(
    eBird.English.Name.2024 %in% redlist_proposed_spec ~ `Redlist Category Proposed`, 
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
  # percentage for Range Coverages (Grid already percent)
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
    "eBird English Name","eBird Scientific Name",
    "BLI English Name","BLI Scientific Name","Order","Family",
    "Breeding Activity Period","Non-breeding Activity Period","Diet Guild",
    "Endemicity","Endemic to India","Endemic to Subcontinent","Endemic to Himalaya",
    "Habitat Specialization","Migratory Status within India","Restricted to Islands",
    "IUCN Category","WPA Schedule","CITES Appendix","CMS Appendix","1% Population Threshold",
    "Selected for SoIB","Selected for Long-term Trend","Selected for Current Annual Trend",
    "Long-term Trend LCI","Long-term Trend Mean","Long-term Trend UCI",
    "Current Annual Trend LCI","Current Annual Trend Mean","Current Annual Trend UCI",
    "Distribution Range Size LCI","Distribution Range Size Mean","Distribution Range Size UCI",
    "State Where Species Key",
    "Number of Grids","Range Coverage (Pre-2000)","Range Coverage (Current)","Range Coverage (2022)",
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
        "Least Concern", "Data Deficient", "Not Recognised"
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
      levels = c("Not protected", "Recent addition", 
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


# README ------------------------------------------------------------------

# info about data types
readme_datatype <- main_db %>% 
  mutate(`Range Coverage CI (Current)` = 0) %>% 
  dplyr::select(-MASK.LABEL) %>% 
  reframe(across(everything(), ~ class(.))) %>% 
  pivot_longer(everything(), names_to = "Field", values_to = "Class")

# range of values
readme_range <- main_db %>% 
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
readme_nat_excl <- main_db %>% 
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
  "NOTE: India sheet contains all 1357 species in India Checklist v7.1 (https://indianbirds.in/india). Subnational sheets contain only those species whose corresponding subnational assessment was done.", "",
  "", "",
  
  "English Name", "English name of species in India Checklist v7.1 (https://indianbirds.in/india)",
  "Scientific Name", "Scientific name of species in India Checklist v7.1 (https://indianbirds.in/india)",
  "SoIB 2023 Priority Status", "Conservation Priority Status of species from SoIB 2023 national-level assessment",
  "SoIB 2023 Long-term Trend Status", "Long-term Trend Status of species from SoIB 2023 assessment",
  "SoIB 2023 Current Annual Trend Status", "Current Annual Trend Status of species from SoIB 2023 assessment",
  "SoIB 2023 Distribution Range Size Status", "Distribution Range Size Status of species assigned from SoIB 2023 assessment",
  "eBird English Name", "English name of species in eBird/Clements Checklist 2024",
  "eBird Scientific Name", "Scientific name of species in eBird/Clements Checklist 2024",
  "BLI English Name", "English name of species in HBW/BLI Checklist 2025",
  "BLI Scientific Name", "Scientific name of species in HBW/BLI Checklist 2025",
  "Order", "Taxonomic Order to which species belongs",
  "Family", "Taxonomic Family to which species belongs",
  "Breeding Activity Period", "Breeding period of species, based on Wilman et al. 2014",
  "Non-breeding Activity Period", "Non-breeding period of species, based on Wilman et al. 2014",
  "Diet Guild", "Diet guild of species, based on Wilman et al. 2014",
  "Endemicity", "Endemicity of species adapted from India Checklist v9.2 (https://indianbirds.in/india)",
  "Endemic to India", "Whether species is endemic to India",
  "Endemic to Subcontinent", "Whether species is endemic to the Indian subcontinent",
  "Endemic to Himalaya", "Whether species is endemic to the Himalaya",
  "Habitat Specialization", "Habitat specialization of species, based on Wilman et al. 2014",
  "Migratory Status within India", "Migratory status of species within India, assigned based on multiple sources",
  "Restricted to Islands", "Whether species is restricted to the islands of India",
  "IUCN Category", "IUCN threat status category of species, based on India Checklist v9.2 (https://indianbirds.in/india)",
  "WPA Schedule", "WPA Schedule of species, based on India Checklist v9.2 (https://indianbirds.in/india)",
  "CITES Appendix", "CITES Appendix category of species, based on India Checklist v9.2 (https://indianbirds.in/india)",
  "CMS Appendix", "CMS Appendix category of species, based on India Checklist v9.2 (https://indianbirds.in/india)",
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
  "Range Coverage (Current)", "Average across 2015\u20132023 of percentage of the 'Total Range' (see above) which was sampled every year",
  "Range Coverage CI (Current)", "(COMING SOON...) 95% confidence interval across 2015\u20132023 of percentage of the 'Total Range' (see above) which was sampled every year",
  "Range Coverage (2022)", "Percentage of the 'Total Range' (see above) which was sampled in the year 2022",
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
           path = "20_website/SoIB_2025_main_readme_forweb_2025.xlsx")


# writing -----------------------------------------------------------------

# ordering sheets by mask
main_db_split <- main_db %>% 
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
  write_xlsx(path = "20_website/SoIB_2023_main_2025.xlsx")


# writing individually for archive

if (!dir.exists("20_website/archive/")) {
  dir.create("20_website/archive/")
}

map2(main_db_split, names(main_db_split), ~ {
  write_csv(.x, glue("20_website/archive/SoIB 2023 {.y}.csv"))
})
