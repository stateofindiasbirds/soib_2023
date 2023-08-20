require(tidyverse)
require(glue)

# create and save metadata for processing analyses for various masks.
# (run only when there are changes to paths. else loaded directly in each Step.)

states_list <- c(
  "Goa", "Manipur", "Odisha", "Dadra and Nagar Haveli", "Andaman and Nicobar Islands",
  "Bihar", "Chandigarh", "Karnataka", "Lakshadweep", "Rajasthan", "Arunachal Pradesh",
  "Gujarat", "Haryana", "Mizoram", "Uttar Pradesh", "Uttarakhand", "Himachal Pradesh",
  "Daman and Diu", "Sikkim", "Delhi", "Tripura", "Punjab", "Madhya Pradesh", "Jharkhand",
  "Nagaland", "Telangana", "West Bengal", "Chhattisgarh", "Jammu and Kashmir", "Assam",       
  "Maharashtra", "Meghalaya", "Ladakh", "Kerala", "Tamil Nadu", "Andhra Pradesh", "Puducherry"
)

analyses_metadata <- data.frame(MASK = c("none", 
                                         "woodland", "cropland", "ONEland", 
                                         "PA",
                                         states_list)) %>% 
  # whether it is for country, state or habitat masks
  mutate(MASK.TYPE = case_when(
    MASK == "none" ~ "country", 
    MASK %in% c("woodland", "cropland", "ONEland") ~ "habitat",
    MASK == "PA" ~ "conservation_area", # PAs, CCAs, IBAs, etc.
    TRUE ~ "state"
  )) %>% 
  mutate(FOLDER = case_when(
    MASK == "none" ~ "01_analyses_full/", 
    MASK %in% c("woodland", "cropland", "ONEland", "PA") ~ glue("01_analyses_mask-{MASK}/"),
    TRUE ~ glue("01_analyses_states/{MASK}/")
  )) %>% 
  mutate(FULLSPECLIST.PATH = glue("{FOLDER}fullspecieslist.csv"),
         LOCS.PATH = glue("{FOLDER}sub_samp_locs.csv"),
         SPECLISTDATA.PATH = glue("{FOLDER}specieslists.RData"),
         DATA.PATH = glue("{FOLDER}dataforanalyses.RData"),
         RAND.GROUP.IDS.PATH = glue("{FOLDER}randomgroupids.RData"),
         
         SIMDATA.PATHONLY = glue("{FOLDER}dataforsim/"),
         TRENDS.PATHONLY = glue("{FOLDER}trends/"),
         # occu model run only for full country; presence only for country and states
         OCCU.PRES.PATHONLY = case_when(
           MASK.TYPE %in% c("country", "state") ~ glue("{FOLDER}occupancy-presence/"),
           TRUE ~ "01_analyses_full/occupancy-presence/"
           ),
         OCCU.MOD.PATHONLY = "01_analyses_full/occupancy-model/",
         RESULTS = glue("{FOLDER}results/"),
         
         OCCU.OUTPATH = glue("{RESULTS}occupancy/"),
         
         TRENDS.OUTPATH = glue("{RESULTS}trends.csv"),
         LTTSENS.PATH = glue("{RESULTS}longterm_sensitivity.RData"),
         CURSENS.PATH = glue("{RESULTS}current_sensitivity.csv"),
         SOIBMAIN.WOCATS.PATH = glue("{RESULTS}SoIB_main_wocats.csv"),
         SOIBMAIN.PATH = glue("{RESULTS}SoIB_main.csv"),
         SUMMARY.PATH = glue("{RESULTS}SoIB_summaries.xlsx")) %>% 
  # ordering masks especially when creating folders
  mutate(MASK = factor(MASK, levels = c("none", 
                                        "woodland", "cropland", "ONEland", 
                                        "PA",
                                        sort(states_list)))) %>% 
  mutate(MASK.ORDERED = glue("{str_pad(as.numeric(MASK), width = 2, pad = 0)}_{MASK}")) %>% 
  # paths for different plot outputs
  mutate(WEB.PLOTS.FOLDER = "20_website/graphs/",
         PLOT.SINGLE.FOLDER = glue("02_graphs/01_single/{MASK.ORDERED}/"),
         PLOT.MULTI.FOLDER = "02_graphs/02_multispecies/",
         PLOT.COMPOSITE.FOLDER = "02_graphs/03_composite/") %>% 
  mutate(MAP.FOLDER = case_when(
    # range maps only produced for full country and states, so we don't want folder for masks
    MASK.TYPE %in% c("country", "state") ~ glue("02_graphs/10_rangemaps/{MASK.ORDERED}/"),
    TRUE ~ NA_character_
  ), 
         WEB.MAP.FOLDER = "20_website/maps/",
         # for states, we take full-country and just filter appropriately
         MAP.DATA.OCC.PATH = glue("01_analyses_full/data_rangemap_toplot.csv"),
         MAP.DATA.VAG.PATH = glue("01_analyses_full/data_rangemap_vagrants.csv"))


# ensuring folders are created if they don't already exist
walk2(analyses_metadata$FOLDER, analyses_metadata$RESULTS, ~ {
  
  if (!dir.exists(.x)) {dir.create(.x, recursive = TRUE)}
  
  if (!dir.exists(.y)) {dir.create(.y, recursive = TRUE)}
  
})

walk(analyses_metadata$OCCU.OUTPATH, ~ {
  if (!dir.exists(.x)) {dir.create(.x, recursive = TRUE)}
})

walk2(analyses_metadata$WEB.PLOTS.FOLDER, analyses_metadata$PLOT.SINGLE.FOLDER, ~ {
  
  if (!dir.exists(.x)) {dir.create(.x, recursive = TRUE)}
  
  if (!dir.exists(.y)) {dir.create(.y, recursive = TRUE)}
  
})

# subfolder for trends
if (!dir.exists(glue("{unique(analyses_metadata$WEB.PLOTS.FOLDER)}trends/"))) {
  dir.create(glue("{unique(analyses_metadata$WEB.PLOTS.FOLDER)}trends/"), recursive = TRUE)
}


walk2(analyses_metadata$PLOT.MULTI.FOLDER, analyses_metadata$PLOT.COMPOSITE.FOLDER, ~ {
  
  if (!dir.exists(.x)) {dir.create(.x, recursive = TRUE)}
  
  if (!dir.exists(.y)) {dir.create(.y, recursive = TRUE)}
  
})

walk2(analyses_metadata$MAP.FOLDER, analyses_metadata$WEB.MAP.FOLDER, ~ {
  
  # value is NA for hab/CA masks, so don't try to dir.___()
  if (!is.na(.x)) {
    if (!dir.exists(.x)) {dir.create(.x, recursive = TRUE)}
  }
  
  if (!dir.exists(.y)) {dir.create(.y, recursive = TRUE)}
  
})

# for later reference
save(analyses_metadata, file = "00_data/analyses_metadata.RData")