# compile all SoIB_main files into single file to upload on website (and download from)

require(tidyverse)
require(glue)
require(writexl)

load("00_data/analyses_metadata.RData")
source("00_scripts/20_functions.R")


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
  mutate(across(c("Essential", "Discard", "BLI.Common.Name", "BLI.Scientific.Name",
                  "eBird.Code","Onepercent.Estimates",
                  contains("5km")), 
                ~ as.null(.))) %>% 
  # move columns
  relocate(
    "India.Checklist.Common.Name",
    "Selected.SOIB","Long.Term.Analysis","Current.Analysis",
    "SOIBv2.Priority.Status","SOIBv2.Long.Term.Status","SOIBv2.Current.Status","SOIBv2.Range.Status",
    "SOIB.Concern.Status","SOIB.Long.Term.Status","SOIB.Current.Status","SOIB.Range.Status",
    "India.Checklist.Scientific.Name","eBird.English.Name.2022","eBird.Scientific.Name.2022",
    "Order","Family","Breeding.Activity.Period","Non.Breeding.Activity.Period","Diet.Guild",
    "India.Endemic","Subcontinent.Endemic","Himalayas.Endemic","Endemic.Region",
    "Habitat.Specialization","Migratory.Status.Within.India","Restricted.Islands",
    "IUCN.Category","WPA.Schedule","CITES.Appendix","CMS.Appendix",
    "longtermlci","longtermmean","longtermrci","currentslopelci","currentslopemean",
    "currentsloperci","rangelci","rangemean","rangerci"
  ) %>% 
  # string encoding issue with 
  mutate(eBird.Scientific.Name.2022 = str_replace_all(eBird.Scientific.Name.2022, "�", " ")) %>% 
  # rename columns
  magrittr::set_colnames(c(
    "Common Name",
    "Selected for SoIB 2023","Selected for LTT","Selected for CAT",
    "SoIB 2023 Priority Status","SoIB 2023 LTT Status","SoIB 2023 CAT Status","SoIB 2023 Range Status",
    "SoIB 2020 Concern Status","SoIB 2020 LTT Status","SoIB 2020 CAT Status","SoIB 2020 Range Status",
    "Scientific Name","eBird English Name 2022","eBird Scientific Name 2022",
    "Order","Family","Breeding Activity Period","Non-breeding Activity Period","Diet Guild",
    "Endemic to India","Endemic to Subcontinent","Endemic to Himalaya","Endemic Region",
    "Habitat Specialisation","Migratory Status within India","Restricted to Islands",
    "IUCN Category","WPA Schedule","CITES Appendix","CMS Appendix",
    "Long-term Trend LCI","Long-term Trend Mean","Long-term Trend RCI",
    "Current Annual Trend LCI","Current Annual Trend Mean","Current Annual Trend RCI",
    "Range Size LCI","Range Size Mean","Range Size RCI",
    "MASK"
    )) %>% 
  # joining mask label
  join_mask_codes() %>% 
  dplyr::select(-c(MASK, MASK.CODE))


# writing

main_db %>% 
  # get named list of dfs
  split(.$MASK.LABEL) %>% 
  write_xlsx(path = "20_website/SoIB_2023_main.xlsx")
