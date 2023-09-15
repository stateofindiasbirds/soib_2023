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


# process ---------------------------------------------------------------------------

main_db <- main_db0 %>% 
  # remove proj columns
  mutate(across(starts_with("proj20"), ~ as.null(.))) %>% 
  # remove other unnecessary columns
  mutate(across(c("Essential", "Discard", "eBird.Code", contains("5km")), 
                ~ as.null(.))) %>% 
  # adding column whether current state is key for particular species
  is_state_keyforspec() %>% 
  # retain taxonomic order of species
  left_join(main_db0 %>% 
              distinct(India.Checklist.Common.Name) %>% 
              rownames_to_column("SPEC.ORDER") %>% 
              mutate(SPEC.ORDER = as.numeric(SPEC.ORDER))) %>% 
  group_by(MASK) %>% 
  arrange(MASK, SPEC.ORDER) %>% 
  ungroup() %>% 
  dplyr::select(-SPEC.ORDER) %>% 
  # SoIB 2020 comparisons not applicable for habs/masks
  mutate(across(c("SOIB.Concern.Status",
                  "SOIB.Long.Term.Status","SOIB.Current.Status","SOIB.Range.Status"),
                ~ case_when(MASK != "none" ~ NA, TRUE ~ .))) %>% 
  # move columns
  relocate(
    "India.Checklist.Common.Name","India.Checklist.Scientific.Name",
    "SOIBv2.Priority.Status","SOIBv2.Long.Term.Status","SOIBv2.Current.Status","SOIBv2.Range.Status",
    "SOIB.Concern.Status","SOIB.Long.Term.Status","SOIB.Current.Status","SOIB.Range.Status",
    "eBird.English.Name.2022","eBird.Scientific.Name.2022", 
    "BLI.Common.Name", "BLI.Scientific.Name","Order","Family",
    "Breeding.Activity.Period","Non.Breeding.Activity.Period","Diet.Guild",
    "Endemic.Region","India.Endemic","Subcontinent.Endemic","Himalayas.Endemic",
    "Habitat.Specialization","Migratory.Status.Within.India","Restricted.Islands",
    "IUCN.Category","WPA.Schedule","CITES.Appendix","CMS.Appendix","Onepercent.Estimates",
    "Selected.SOIB","Long.Term.Analysis","Current.Analysis",
    "longtermlci","longtermmean","longtermrci","currentslopelci","currentslopemean",
    "currentsloperci","rangelci","rangemean","rangerci", "KEY"
  ) %>% 
  # string encoding issue  
  mutate(across(c("eBird.Scientific.Name.2022", "BLI.Scientific.Name"),
                ~ str_replace_all(., "�", " "))) %>% 
  # rename columns
  magrittr::set_colnames(c(
    "Common Name","Scientific Name",
    "SoIB 2023 Priority Status","SoIB 2023 Long-term Trend Status","SoIB 2023 Current Annual Trend Status",
    "SoIB 2023 Distribution Range Size Status",
    "SoIB 2020 Concern Status","SoIB 2020 Long-term Trend Status","SoIB 2020 Current Annual Trend Status",
    "SoIB 2020 Distribution Range Size Status",
    "eBird English Name 2022","eBird Scientific Name 2022",
    "BLI Common Name","BLI Scientific Name","Order","Family",
    "Breeding Activity Period","Non-breeding Activity Period","Diet Guild",
    "Endemicity","Endemic to India","Endemic to Subcontinent","Endemic to Himalaya",
    "Habitat Specialization","Migratory Status within India","Restricted to Islands",
    "IUCN Category","WPA Schedule","CITES Appendix","CMS Appendix","1% Estimate",
    "Selected for SoIB 2023","Selected for Long-term Trend","Selected for Current Annual Trend",
    "Long-term Trend LCI","Long-term Trend Mean","Long-term Trend RCI",
    "Current Annual Trend LCI","Current Annual Trend Mean","Current Annual Trend RCI",
    "Distribution Range Size LCI","Distribution Range Size Mean","Distribution Range Size RCI",
    "Key Species for States", "MASK"
    )) %>% 
  # joining mask label
  join_mask_codes() %>% 
  dplyr::select(-c(MASK, MASK.CODE)) 


# writing

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

main_db_split[split_order] %>% 
  # removing the column of mask name
  map(~ dplyr::select(.x, -MASK.LABEL)) %>% 
  write_xlsx(path = "20_website/SoIB_2023_main.xlsx")
