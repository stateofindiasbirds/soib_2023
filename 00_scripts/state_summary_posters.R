require(tidyverse)
require(sf)
require(officer)

load("00_data/dataforanalyses_extra.RData")
load("01_analyses_full/specieslists.RData")
load("00_data/maps_sf.RData")

analyses_metadata = get_metadata() %>% filter(MASK.TYPE == "state")


dataM <- read.delim("00_data/dataforMyna.txt", 
                    sep = "\t", header = T, quote = "", stringsAsFactors = F,
                    na.strings = c(""," ",NA)) %>% 
  rename(ST_NM = STATE,
         DISTRICT = COUNTY) %>% 
  mutate(OBSERVATION.DATE = as.Date(OBSERVATION.DATE), 
         month = month(OBSERVATION.DATE),
         cyear = year(OBSERVATION.DATE)) %>%
  mutate(year = ifelse(month > 5, cyear, cyear-1)) %>% # from June to May
  mutate(group.id = ifelse(is.na(GROUP.IDENTIFIER), 
                           SAMPLING.EVENT.IDENTIFIER, GROUP.IDENTIFIER)) 


main = read.csv("01_analyses_full/results/SoIB_main.csv")
india_checklist_map = main %>% dplyr::select(India.Checklist.Common.Name,eBird.English.Name.2024)
district_mapping = read.csv("00_data/eBird_district_mapping.csv") %>%
  distinct()
photos = read.csv("00_data/BOW_image_list.csv")

state_birds = read.csv("00_data/state_birds.csv")
top4 = read.csv("01_analyses_full/results/key_state_species_top4.csv") %>%
  left_join(india_checklist_map) %>%
  dplyr::select(ST_NM,eBird.English.Name.2024) %>%
  rename(COMMON.NAME = eBird.English.Name.2024)
  
key_species = read.csv("01_analyses_full/results/key_state_species_full.csv")


n_species = dataM %>%
  filter(CATEGORY %in% c("species","issf")) %>%
  group_by(ST_NM) %>%
  reframe(n = n_distinct(COMMON.NAME))

state_summary = states_sf %>%
  st_drop_geometry() %>%
  mutate(AREA = round(AREA)) %>%
  rename(ST_NM = STATE.NAME) %>%
  left_join(n_species)


# key districts for top 4

key_districts = data0 %>%
  filter(ALL.SPECIES.REPORTED == 1, !is.na(DISTRICT), year > (soib_year_info("latest_year") - 5)) %>%
  group_by(ST_NM,DISTRICT) %>%
  mutate(lists = n_distinct(group.id)) %>% ungroup() %>%
  semi_join(top4) %>%
  filter(lists > 20) %>%
  group_by(ST_NM,COMMON.NAME,DISTRICT) %>%
  reframe(freq = n_distinct(group.id)/max(lists)) %>%
  group_by(ST_NM,COMMON.NAME) %>%
  arrange(desc(freq)) %>% slice(1:2) %>%
  arrange(ST_NM,COMMON.NAME,DISTRICT,desc(freq)) %>%
  ungroup()
  
  
  

# key habitats for top 20    

habitats = main %>% 
  dplyr::select(eBird.English.Name.2024,Habitat.Specialization,
                India.Checklist.Common.Name) %>%
  mutate(Habitat.Specialization = 
           case_when(Habitat.Specialization == "Forest" ~ "Woodland",
                     Habitat.Specialization == "Forest & Plantation" ~ "Woodland",
                     Habitat.Specialization == "Grassland" ~ "Open Landscape",
                     Habitat.Specialization == "Grassland & Scrub" ~ "Open Landscape",
                     Habitat.Specialization == "Alpine & Cold Desert" ~ "Open Landscape",
                     Habitat.Specialization == "Open Habitat" ~ "Open Landscape",
                     TRUE ~ Habitat.Specialization))

key_habitats_1 = key_species %>%
  left_join(habitats) %>%
  filter(!Habitat.Specialization %in% c("Non-specialized")) %>%
  dplyr::select(-India.Checklist.Common.Name) %>%
  group_by(ST_NM,Habitat.Specialization) %>%
  rename(COMMON.NAME = eBird.English.Name.2024) %>%
  mutate(n = n_distinct(COMMON.NAME)) %>%
  filter(prop.range == 1)

key_habitats = key_species %>%
  left_join(habitats) %>%
  filter(!Habitat.Specialization %in% c("Non-specialized")) %>%
  dplyr::select(-India.Checklist.Common.Name) %>%
  group_by(ST_NM,Habitat.Specialization) %>%
  rename(COMMON.NAME = eBird.English.Name.2024) %>%
  mutate(n = n_distinct(COMMON.NAME)) %>%
  group_by(ST_NM) %>%
  arrange(ST_NM,desc(n),Habitat.Specialization,desc(prop.range)) %>% ungroup() %>%
  group_by(ST_NM,n,Habitat.Specialization) %>%
  slice(1:4) %>%
  bind_rows(key_habitats_1) %>%
  arrange(ST_NM,desc(n),Habitat.Specialization,desc(prop.range)) %>% ungroup() %>%
  distinct()

key_habitats$Habitat.Specialization = factor(key_habitats$Habitat.Specialization,
                                             levels = c("Woodland","Wetland","Open Landscape"))




# important districts

main_priority = main %>%
  dplyr::select(eBird.English.Name.2024,SoIB.Latest.Priority.Status) %>%
  rename(COMMON.NAME = eBird.English.Name.2024)

top_districts = data0 %>%
  filter(!is.na(DISTRICT)) %>%
  left_join(main_priority) %>%
  dplyr::select(ST_NM,DISTRICT,COMMON.NAME,SoIB.Latest.Priority.Status) %>%
  filter(SoIB.Latest.Priority.Status == "High") %>%
  group_by(ST_NM,DISTRICT) %>%
  reframe(n.high = n_distinct(COMMON.NAME)) %>%
  group_by(ST_NM) %>% arrange(desc(n.high)) %>% slice(1:3)


low_birding_districts_part = data0 %>%
  filter(ALL.SPECIES.REPORTED == 1, !is.na(DISTRICT)) %>%
  dplyr::select(ST_NM,DISTRICT,group.id) %>%
  group_by(ST_NM,DISTRICT) %>%
  reframe(n.lists = n_distinct(group.id))

low_birding_districts = dists_sf %>%
  st_drop_geometry() %>%
  left_join(district_mapping) %>%
  rename(ST_NM = STATE.NAME) %>%
  mutate(DISTRICT = case_when(ST_NM == "Chhattisgarh" & DISTRICT.NAME == "Raigad" ~ "Raigarh",
                              ST_NM == "Maharashtra" & DISTRICT.NAME == "Raigad" ~ "Raigad",
                              ST_NM == "Uttarakhand" & DISTRICT.NAME == "Almora\n" ~ "Almora",
                              TRUE ~ DISTRICT)) %>%
  dplyr::select(ST_NM,DISTRICT) %>%
  left_join(low_birding_districts_part) %>%
  mutate(n.lists = case_when(is.na(n.lists) ~ 0, TRUE ~ n.lists)) %>%
  group_by(ST_NM) %>% arrange(n.lists) %>% slice(1:3)
  

# produce  doc

text_style0 = fp_text(font.size = 20, bold = T)
text_style1 = fp_text(font.size = 12, bold = T)
text_style2 = fp_text(font.size = 8, bold = T, italic = T)
par_style0 = fp_par(text.align = "left", padding.bottom = 10, padding.top = 10, line_spacing = 2)
par_style1 = fp_par(text.align = "left", padding.bottom = 10, padding.top = 10)

SoIB_state_summary = read_docx()
SoIB_state_summary = body_add_par(SoIB_state_summary, "SoIB State Summary info", style = "centered")

trend_cats = tibble('Trend Status' = c("Rapid Decline","Decline","Stable","Increase",
                                           "Rapid Increase","Trend Inconclusive",
                                           "Insufficient Data"))

for (i in analyses_metadata$MASK)
{
  SoIB_state_summary = body_add_fpar(SoIB_state_summary, 
                                     fpar(ftext(i, prop = text_style0), fp_p = par_style0))
  
  
  st_main = read.csv(analyses_metadata$SOIBMAIN.PATH[analyses_metadata$MASK == i])

  SoIB_state_summary = body_add_par(SoIB_state_summary, 
                                    paste("Area = ",state_summary$AREA[state_summary$ST_NM == i], sep = ""), 
                                    style = "Normal")
  SoIB_state_summary = body_add_par(SoIB_state_summary, 
                                    paste("Species = ",state_summary$n[state_summary$ST_NM == i], sep = ""), 
                                    style = "Normal")
  
  # link to folder with state and PA map
  
  SoIB_state_summary = body_add_fpar(SoIB_state_summary, 
                                     fpar(ftext("Folder with state maps with PAs", 
                                                prop = text_style1), 
                                          fp_p = par_style1))
  SoIB_state_summary = body_add_par(SoIB_state_summary, 
                                    "https://drive.google.com/drive/folders/1Acnywq7xFLctYdNpG2o41WWXZ5xVgXZ-?usp=sharing", 
                                    style = "Normal")

  # key  species
  
  SoIB_state_summary = body_add_fpar(SoIB_state_summary, 
                                     fpar(ftext("Key species and districts", 
                                                prop = text_style1), 
                                          fp_p = par_style1))
  
  delim.key.list = key_districts %>% filter(ST_NM == i) %>%
    rename(eBird.English.Name.2024 = COMMON.NAME) %>%
    left_join(india_checklist_map) %>%
    group_by(India.Checklist.Common.Name) %>%
    reframe(DISTRICT = toString(DISTRICT)) %>%
    ungroup()
  
  delim.key.list = top4 %>%
    filter(ST_NM == i) %>%
    left_join(photos) %>%
    rename(eBird.English.Name.2024 = COMMON.NAME) %>%
    left_join(india_checklist_map) %>%
    dplyr::select(India.Checklist.Common.Name,URL) %>%
    left_join(delim.key.list) %>%
    dplyr::select(India.Checklist.Common.Name,DISTRICT,URL)
    

  SoIB_state_summary = body_add_table(SoIB_state_summary, 
                                      value = delim.key.list, 
                                      style = "table_template")
  
  # state bird
  
  st_bird = state_birds$COMMON.NAME[state_birds$ST_NM == i]
  
  if (!is.na(st_bird))
  {
    st_bird_India = india_checklist_map$India.Checklist.Common.Name[
      india_checklist_map$eBird.English.Name.2024 == st_bird]
  }

  
  SoIB_state_summary = body_add_fpar(SoIB_state_summary, 
                                     fpar(ftext("State Bird", 
                                                prop = text_style1), 
                                          fp_p = par_style1))
  
  if (!is.na(st_bird))
  {
    SoIB_state_summary = body_add_par(SoIB_state_summary, 
                                      st_bird_India, 
                                      style = "Normal")
    SoIB_state_summary = body_add_par(SoIB_state_summary, 
                                      photos$URL[photos$COMMON.NAME == st_bird], 
                                      style = "Normal")
    SoIB_state_summary = body_add_fpar(SoIB_state_summary, 
                                       fpar(ftext("National Trend and Status", 
                                                  prop = text_style2), 
                                            fp_p = par_style1))
    SoIB_state_summary = body_add_par(SoIB_state_summary, 
                                      paste("Long-term Trend = ",
                                            main$SoIB.Latest.Long.Term.Status[main$eBird.English.Name.2024 == st_bird], sep = ""), 
                                      style = "Normal")
    SoIB_state_summary = body_add_par(SoIB_state_summary, 
                                      paste("Current Trend = ",
                                            main$SoIB.Latest.Current.Status[main$eBird.English.Name.2024 == st_bird], sep = ""), 
                                      style = "Normal")
    SoIB_state_summary = body_add_par(SoIB_state_summary, 
                                      paste("National Conservation Priority = ",
                                            main$SoIB.Latest.Priority.Status[main$eBird.English.Name.2024 == st_bird], sep = ""), 
                                      style = "Normal")
    SoIB_state_summary = body_add_par(SoIB_state_summary, 
                                      paste("IUCN = ",
                                            main$IUCN.Category[main$eBird.English.Name.2024 == st_bird], sep = ""), 
                                      style = "Normal")
    SoIB_state_summary = body_add_par(SoIB_state_summary, 
                                      paste("WPA = ",
                                            main$WPA.Schedule[main$eBird.English.Name.2024 == st_bird], sep = ""), 
                                      style = "Normal")
    
    SoIB_state_summary = body_add_fpar(SoIB_state_summary, 
                                       fpar(ftext("State Trend and Status", 
                                                  prop = text_style2), 
                                            fp_p = par_style1))
    SoIB_state_summary = body_add_par(SoIB_state_summary, 
                                      paste("Long-term Trend = ",
                                            st_main$SoIB.Latest.Long.Term.Status[st_main$eBird.English.Name.2024 == st_bird], sep = ""), 
                                      style = "Normal")
    SoIB_state_summary = body_add_par(SoIB_state_summary, 
                                      paste("Current Trend = ",
                                            st_main$SoIB.Latest.Current.Status[st_main$eBird.English.Name.2024 == st_bird], sep = ""), 
                                      style = "Normal")
    
    SoIB_state_summary = body_add_fpar(SoIB_state_summary, 
                                       fpar(ftext("Map Link", 
                                                  prop = text_style2), 
                                            fp_p = par_style1))
    SoIB_state_summary = body_add_par(SoIB_state_summary,
                                      paste("https://stateofindiasbirds.in/species/",
                                            state_birds$abb[state_birds$COMMON.NAME == st_bird &
                                                              state_birds$ST_NM == i],
                                            "-",
                                            main$eBird.Code[main$eBird.English.Name.2024 == st_bird],
                                            "/",sep = ""),
                                      style = "Normal")
  }
  else
  {
    SoIB_state_summary = body_add_par(SoIB_state_summary, 
                                      "No state bird", 
                                      style = "Normal")
  }
    
  
  # high priority map
  
  SoIB_state_summary = body_add_fpar(SoIB_state_summary, 
                                     fpar(ftext("Folder with High Priority maps", 
                                                prop = text_style1), 
                                          fp_p = par_style1))
  SoIB_state_summary = body_add_par(SoIB_state_summary, 
                                    "https://drive.google.com/drive/folders/1c_ue5fbzY4BGOwD3okhgYht_Ihx9u45W?usp=sharing", 
                                    style = "Normal")
  
  
  # major habitats
  
  delim.habitat.list = key_habitats %>%
    filter(ST_NM == i) %>%
    rename(eBird.English.Name.2024 = COMMON.NAME) %>%
    left_join(india_checklist_map) %>%
    group_by(Habitat.Specialization) %>%
    reframe(India.Checklist.Common.Name = toString(India.Checklist.Common.Name)) %>%
    ungroup()
  
  SoIB_state_summary = body_add_fpar(SoIB_state_summary, 
                                     fpar(ftext("Major habitats", 
                                                prop = text_style1), 
                                          fp_p = par_style1))
  SoIB_state_summary = body_add_table(SoIB_state_summary, 
                                      value = delim.habitat.list, 
                                      style = "table_template")
  
  
  # trend summary
  
  st_trend_list = union(st_main$eBird.English.Name.2024[st_main$Long.Term.Analysis == "X"],
                        st_main$eBird.English.Name.2024[st_main$Current.Analysis == "X"])
  
  st_inc_list = st_main %>%
    filter(Selected.SoIB == "X") %>%
    pull(eBird.English.Name.2024)
  
  flag = "state level - 150 or more species assessed in state"
  
  if (length(st_trend_list) < 150)
  {
    st_main = main
    flag = "country level - less than 150 species assessed in state"
  }
    
  ltt = st_main %>%
    filter(eBird.English.Name.2022 %in% st_inc_list) %>%
    group_by(SoIB.Latest.Long.Term.Status) %>% 
    reframe('Long-term' = n()) %>%
    rename('Trend Status' = SoIB.Latest.Long.Term.Status)
  
  cat = st_main %>%
    filter(eBird.English.Name.2022 %in% st_inc_list) %>%
    group_by(SoIB.Latest.Current.Status) %>% 
    reframe('Current Annual' = n()) %>%
    rename('Trend Status' = SoIB.Latest.Current.Status)
  
  trend_summary = trend_cats %>%
    left_join(ltt) %>%
    left_join(cat)
  
  SoIB_state_summary = body_add_fpar(SoIB_state_summary, 
                                     fpar(ftext("Trend summary",
                                                prop = text_style1), 
                                          fp_p = par_style1))
  SoIB_state_summary = body_add_fpar(SoIB_state_summary, 
                                     fpar(ftext(flag,
                                                prop = text_style1), 
                                          fp_p = par_style1))
  SoIB_state_summary = body_add_table(SoIB_state_summary, 
                                      value = trend_summary, 
                                      style = "table_template")
  
  
  # districts with most high priority species
  
  SoIB_state_summary = body_add_fpar(SoIB_state_summary, 
                                     fpar(ftext("Districts with most High Priority species", 
                                                prop = text_style1), 
                                          fp_p = par_style1))
  SoIB_state_summary = body_add_table(SoIB_state_summary, 
                                      value = top_districts %>% ungroup() %>%
                                        filter(ST_NM == i) %>%
                                        dplyr::select(-ST_NM), 
                                      style = "table_template")
  
  # districts with least birding
  
  SoIB_state_summary = body_add_fpar(SoIB_state_summary, 
                                     fpar(ftext("Districts with fewest bird lists", 
                                                prop = text_style1), 
                                          fp_p = par_style1))
  SoIB_state_summary = body_add_table(SoIB_state_summary, 
                                      value = low_birding_districts %>% ungroup() %>%
                                        filter(ST_NM == i) %>%
                                        dplyr::select(-ST_NM), 
                                      style = "table_template")
  
  
}

print(SoIB_state_summary, target = "01_analyses_states/SoIB_state_summary.docx")
