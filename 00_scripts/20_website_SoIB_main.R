# compile all SoIB_main files into single file to upload on website (and download from)

require(tidyverse)
require(glue)
require(writexl)

load("00_data/analyses_metadata.RData")
source("00_scripts/20_functions.R")

# key states for each species
keystates <- read.csv("01_analyses_full/results/key_state_species_full.csv") %>% 
  arrange(ST_NM, India.Checklist.Common.Name) 


# import ----------------------------------------------------------------------------

# importing all data and setting up
main_db0 <- map2(analyses_metadata$SOIBMAIN.PATH, analyses_metadata$MASK, 
                 ~ read_fn(.x) %>% bind_cols(tibble(MASK = .y))) %>% 
  list_rbind()

# red list values
redlist <- read_csv("01_analyses_full/results/redlist.csv") %>% 
  filter(`3GEN` <= 14) %>% 
  dplyr::select("Species", "3GEN Decline", "Redlist Category Proposed") %>% 
  magrittr::set_colnames(c("India.Checklist.Common.Name", 
                           "Projected Decline in 3 Generations",
                           "Regional Red List Category")) %>% 
  # these values only for country-level, and not for subnational
  mutate(MASK = "none")

# process ---------------------------------------------------------------------------

main_db <- main_db0 %>% 
  # remove proj columns
  mutate(across(starts_with("proj20"), ~ as.null(.))) %>% 
  # remove other unnecessary columns
  mutate(across(c("Essential", "Discard", "eBird.Code"), 
                ~ as.null(.))) %>% 
  # adding column whether species is key for current state
  is_curspec_key4state() %>% 
  # retain taxonomic order of species
  left_join(main_db0 %>% 
              distinct(India.Checklist.Common.Name) %>% 
              rownames_to_column("SPEC.ORDER") %>% 
              mutate(SPEC.ORDER = as.numeric(SPEC.ORDER))) %>% 
  group_by(MASK) %>% 
  arrange(MASK, SPEC.ORDER) %>% 
  ungroup() %>% 
  dplyr::select(-SPEC.ORDER) %>% 
  # join Red List columns
  left_join(redlist) %>% 
  # move columns
  relocate(
    "India.Checklist.Common.Name","India.Checklist.Scientific.Name",
    "SOIBv2.Priority.Status","SOIBv2.Long.Term.Status","SOIBv2.Current.Status","SOIBv2.Range.Status",
    "SOIB.Long.Term.Status","SOIB.Current.Status","SOIB.Range.Status",
    "eBird.English.Name.2022","eBird.Scientific.Name.2022", 
    "BLI.Common.Name", "BLI.Scientific.Name","Order","Family",
    "Breeding.Activity.Period","Non.Breeding.Activity.Period","Diet.Guild",
    "Endemic.Region","India.Endemic","Subcontinent.Endemic","Himalayas.Endemic",
    "Habitat.Specialization","Migratory.Status.Within.India","Restricted.Islands",
    "IUCN.Category","WPA.Schedule","CITES.Appendix","CMS.Appendix","Onepercent.Estimates",
    "Selected.SOIB","Long.Term.Analysis","Current.Analysis",
    "longtermlci","longtermmean","longtermrci","currentslopelci","currentslopemean",
    "currentsloperci","rangelci","rangemean","rangerci","SOIB.Concern.Status","KEY",
    "totalrange25km","proprange25km2000","proprange25km.current","proprange25km2022",
    "mean5km","ci5km","Projected Decline in 3 Generations","Regional Red List Category"
  ) %>% 
  # rename columns
  magrittr::set_colnames(c(
    "English Name","Scientific Name",
    "SoIB 2023 Priority Status","SoIB 2023 Long-term Trend Status","SoIB 2023 Current Annual Trend Status",
    "SoIB 2023 Distribution Range Size Status",
    "SoIB 2020 Long-term Trend Status","SoIB 2020 Current Annual Trend Status",
    "SoIB 2020 Distribution Range Size Status",
    "eBird English Name 2022","eBird Scientific Name 2022",
    "BLI English Name 2022","BLI Scientific Name 2022","Order","Family",
    "Breeding Activity Period","Non-breeding Activity Period","Diet Guild",
    "Endemicity","Endemic to India","Endemic to Subcontinent","Endemic to Himalaya",
    "Habitat Specialization","Migratory Status within India","Restricted to Islands",
    "IUCN Category","WPA Schedule","CITES Appendix","CMS Appendix","1% Population Threshold",
    "Selected for SoIB","Selected for Long-term Trend","Selected for Current Annual Trend",
    "Long-term Trend LCI","Long-term Trend Mean","Long-term Trend UCI",
    "Current Annual Trend LCI","Current Annual Trend Mean","Current Annual Trend UCI",
    "Distribution Range Size LCI","Distribution Range Size Mean","Distribution Range Size UCI",
    "SoIB 2020 Concern Status","Key Species for State",
    "Total Range","Range Coverage (Pre-2000)","Range Coverage (Current)","Range Coverage (2022)",
    "Grid Coverage Mean", "Grid Coverage CI",
    "Projected Decline in 3 Generations","Regional Red List Category",
    "MASK"
    )) %>% 
  # joining mask label
  join_mask_codes() %>% 
  dplyr::select(-c(MASK, MASK.CODE)) 


# README ------------------------------------------------------------------

readme <- tribble(
  ~ Field, ~ Meaning,
  
  "English Name", "English name of species in India Checklist v7.1 (https://indianbirds.in/india)",
  "Scientific Name", "Scientific name of species in India Checklist v7.1 (https://indianbirds.in/india)",
  "SoIB 2023 Priority Status", "Conservation Priority Status of species from SoIB 2023 assessment",
  "SoIB 2023 Long-term Trend Status", "Long-term Trend Status of species from SoIB 2023 assessment",
  "SoIB 2023 Current Annual Trend Status", "Current Annual Trend Status of species from SoIB 2023 assessment",
  "SoIB 2023 Distribution Range Size Status", "Distribution Range Size Status of species assigned from SoIB 2023 assessment",
  "SoIB 2020 Long-term Trend Status", "Long-term Trend Status of species from SoIB 2020 assessment",
  "SoIB 2020 Current Annual Trend Status", "Current Annual Trend Status of species from SoIB 2020 assessment",
  "SoIB 2020 Distribution Range Size Status", "Distribution Range Size Status of species from SoIB 2020 assessment",
  "eBird English Name 2022", "English name of species in eBird/Clements Checklist 2022",
  "eBird Scientific Name 2022", "Scientific name of species in eBird/Clements Checklist 2022",
  "BLI English Name 2022", "English name of species in HBW/BLI Checklist 2022",
  "BLI Scientific Name 2022", "Scientific name of species in HBW/BLI Checklist 2022",
  "Order", "Taxonomic Order to which species belongs",
  "Family", "Taxonomic Family to which species belongs",
  "Breeding Activity Period", "Breeding period of species, based on ___",
  "Non-breeding Activity Period", "Non-breeding period of species, based on ___",
  "Diet Guild", "Diet guild of species, based on ___",
  "Endemicity", "Endemicity of species, based on ___",
  "Endemic to India", "Whether species is endemic to India",
  "Endemic to Subcontinent", "Whether species is endemic to the Indian subcontinent",
  "Endemic to Himalaya", "Whether species is endemic to the Himalaya",
  "Habitat Specialization", "Habitat specialization of species, based on ___",
  "Migratory Status within India", "Migratory status of species within India, assigned based on ___",
  "Restricted to Islands", "Whether species is restricted to the islands of India",
  "IUCN Category", "IUCN threat status category of species based on India Checklist v7.1 (https://indianbirds.in/india)",
  "WPA Schedule", "IUCN threat status category of species based on India Checklist v7.1 (https://indianbirds.in/india)",
  "CITES Appendix", "IUCN threat status category of species based on India Checklist v7.1 (https://indianbirds.in/india)",
  "CMS Appendix", "IUCN threat status category of species based on India Checklist v7.1 (https://indianbirds.in/india)",
  "1% Population Threshold", "___",
  "Selected for SoIB", "Whether species was selected for SoIB 2023 analyses",
  "Selected for Long-term Trend", "Whether species was selected for Long-term Trend analysis in SoIB 2023",
  "Selected for Current Annual Trend", "Whether species was selected for Current Annual Trend analysis in SoIB 2023",
  "Long-term Trend LCI", "Modelled estimate of Long-term Trend (lower limit of 95% confidence interval)",
  "Long-term Trend Mean", "Modelled estimate of Long-term Trend (mean)",
  "Long-term Trend UCI", "Modelled estimate of Long-term Trend (upper limit of 95% confidence interval)",
  "Current Annual Trend LCI", "Modelled estimate of Current Annual Trend (lower limit of 95% confidence interval)",
  "Current Annual Trend Mean", "Modelled estimate of Current Annual Trend (mean)",
  "Current Annual Trend UCI", "Modelled estimate of Current Annual Trend (upper limit of 95% confidence interval)",
  "Distribution Range Size LCI", "Modelled estimate of Distribution Range Size (lower limit of 95% confidence interval)",
  "Distribution Range Size Mean", "Modelled estimate of Distribution Range Size (mean)",
  "Distribution Range Size UCI", "Modelled estimate of Distribution Range Size (upper limit of 95% confidence interval)",
  "SoIB 2020 Concern Status", "Conservation Concern Status of species from SoIB 2020 assessment",
  "Key Species for State", "Whether species is one of the key species for the current state (see ___)",
  "Total Range", "Number of 25 km x 25 km grid cells from which the species reported over time",
  "Range Coverage (Pre-2000)", "Percentage of the 'Total Range' (see above) of the species which was sampled before the year 2000",
  "Range Coverage (Current)", "Percentage of the 'Total Range' (see above) which was sampled during 2015\u20132023",
  "Range Coverage (2022)", "Percentage of the 'Total Range' (see above) which was sampled in the year 2022",
  "Grid Coverage Mean", "Mean estimate of coverage of 5 km x 5 km subcells within each 25 km x 25 km cell averaged across all cells",
  "Grid Coverage CI", "95% confidence interval of coverage of 5 km x 5 km subcells within each 25 km x 25 km cell averaged across all cells",
  "Projected Decline in 3 Generations", "Decline in 3 generations of species, projected from SoIB 2023 analysis",
  "Regional Red List Category", "Regional Red List threat status category proposed from SoIB 2023 analysis"
  
) %>% 
  # dummy, to be removed later
  mutate(MASK.LABEL = "")

# writing -----------------------------------------------------------------

# ordering sheets by mask
main_db_split <- main_db %>% 
  # get named list of dfs
  split(.$MASK.LABEL)

split_order <- data.frame(MASK.LABEL = names(main_db_split)) %>% 
  rownames_to_column("ID") %>% 
  left_join(analyses_metadata %>% join_mask_codes()) %>% 
  arrange(MASK.ORDERED) %>%
  pull(ID) %>% 
  as.numeric()

c(list(README = readme), 
  main_db_split[split_order]) %>% 
  # removing the column of mask name
  map(~ dplyr::select(.x, -MASK.LABEL)) %>% 
  write_xlsx(path = "20_website/SoIB_2023_main.xlsx")
