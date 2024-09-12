# create and save metadata for the individual case studies.

### (run only when there are changes to paths. else loaded directly in each Step.) ###

require(tidyverse)
require(glue)

case_study_list <- c("hornbills", "spiti", "vembanad", "eaglenest", "nannaj")
case_study_metrics <- c("Encounter rate (per km)", "Density (count per ha)", "Count",
                        NA_character_, "Average count per point")

sysmon_metadata <- data.frame(
  # bustards plot comes in Species Accounts but plotting follows sysmon logic
  CASE = c("bustards", case_study_list),
  METRIC = c("Population estimate", case_study_metrics)
) %>% 
  mutate(CASE.NO = 1:n() - 1,
         FOLDER.ROOT = "10_sys-mon/",
         FOLDER.DATA = glue("{FOLDER.ROOT}data/"),
         FOLDER.OUT = glue("{FOLDER.ROOT}graphs/"),
         CASE.ORDER = glue("{str_pad(CASE.NO, width = 2, pad = 0)}_{CASE}")) %>% 
  mutate(PATH.DATA = glue("{FOLDER.DATA}{CASE.ORDER}.csv"),
         PATH.DATA.EXTRA = case_when(
           CASE == "spiti" ~ glue("{FOLDER.DATA}{CASE.ORDER}_hab.csv"),
           CASE == "vembanad" ~ glue("{FOLDER.DATA}{CASE.ORDER}_total.csv")
         )) %>% 
  mutate(PATH.OUT = glue("{FOLDER.OUT}{CASE.ORDER}.png"),
         PATH.OUT.EXTRA = case_when(
           CASE == "spiti" ~ glue("{FOLDER.OUT}{CASE.ORDER}_hab.png"),
           CASE == "vembanad" ~ glue("{FOLDER.OUT}{CASE.ORDER}_total.png")
         )) %>% 
  # eaglenest plots already exist
  mutate(across(c(everything(), -CASE, -CASE.NO, -CASE.ORDER), 
                ~ ifelse(CASE == "eaglenest", NA_real_, .))) %>% 
  arrange(CASE.ORDER) %>% 
  mutate(across(c(CASE, CASE.ORDER), ~ fct_inorder(.))) %>% 
  relocate(CASE.ORDER, CASE.NO, CASE)


walk2(sysmon_metadata$FOLDER.DATA, sysmon_metadata$FOLDER.OUT, ~ {
  
  if (!dir.exists(.x)) {dir.create(.x, recursive = TRUE)}
  
  if (!dir.exists(.y)) {dir.create(.y, recursive = TRUE)}
  
})

# for later reference
save(sysmon_metadata, file = "00_data/sysmon_metadata.RData")
