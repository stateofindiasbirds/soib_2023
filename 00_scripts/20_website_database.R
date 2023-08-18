require(tidyverse)
require(glue)

load("00_data/analyses_metadata.RData")
source("00_scripts/20_functions.R")

# key states for each species
keystates <- read.csv("01_analyses_full/results/key_state_species_full.csv") %>% 
  arrange(India.Checklist.Common.Name, ST_NM) %>% 
  group_by(India.Checklist.Common.Name) %>% 
  summarise(key_states = str_flatten_comma(ST_NM))

# import ----------------------------------------------------------------------------

# importing all data and setting up
web_db0 <- map2(analyses_metadata$SOIBMAIN.PATH, analyses_metadata$MASK, 
              ~ read_fn(.x) %>% bind_cols(tibble(MASK = .y))) %>% 
  list_rbind() %>% 
  # filtering for SoIB species
  filter(Selected.SOIB == "X") %>%
  dplyr::select(-c("eBird.English.Name.2022", "eBird.Scientific.Name.2022", "Order", "Family",
                   starts_with("SOIB."), contains("Breeding.Activity"), "Diet.Guild",
                   starts_with("BLI."), ends_with(".Appendix"), "Onepercent.Estimates", 
                   contains("range25km"), "mean5km", "ci5km",
                   starts_with("proj20"))) %>% 
  # joining MASK.TYPE
  left_join(analyses_metadata %>% distinct(MASK, MASK.TYPE)) %>% 
  # changing "country" mask type to "national"
  mutate(MASK.TYPE = if_else(MASK.TYPE == "country", "national", MASK.TYPE)) %>% 
  relocate(India.Checklist.Common.Name, MASK) %>% 
  arrange(India.Checklist.Common.Name, MASK) %>% 
  mutate(ID = "", post_excerpt = "", post_date = "", downloadlink = "",
         wp_page_template = "", pinged = "", primary_assessment = "",
         post_author = "amithkumar.4",
         post_status = "publish",
         post_format = "standard", 
         comment_status = "closed", ping_status = "closed",
         post_parent = 0, menu_order = 0)


# creation of fields ----------------------------------------------------------------

web_db <- web_db0 %>% 
  # join key states for each species
  left_join(keystates) %>% 
  rename(`long-term_trend` = longtermmean,
         current_annual_change = currentslopemean,
         distribution_range_size = rangemean,
         current_status = SOIBv2.Current.Status,
         distribution_status = SOIBv2.Range.Status,
         iucn_status = IUCN.Category,
         long_term_status = SOIBv2.Long.Term.Status,
         migratory_status = Migratory.Status.Within.India,
         status_of_conservation_concern = SOIBv2.Priority.Status,
         wlpa_schedule = WPA.Schedule,
         habitat_specialization = Habitat.Specialization,
         endemicity = Endemic.Region,
         custom_url = eBird.Code) %>% 
  mutate(across(c("long-term_trend", "current_annual_change"), ~ round(., 2))) %>% 
  str_c_CI(., longtermlci, longtermrci, new_name = "long-term_trend_ci") %>% 
  str_c_CI(., currentslopelci, currentsloperci, new_name = "current_annual_change_ci") %>% 
  str_c_CI(., rangelci, rangerci, new_name = "distribution_range_size_ci_units_of_10000_sqkm") %>% 
  join_mask_codes()


# creation of fields within species (diff. masks) -----------------------------------

web_db <- web_db %>% 
  # setup for some long strings
  mutate(URL_base = "https://wordpress-1024190-3615983.cloudwaysapps.com/",
         # prefix for uploads
         URL_pre_uploads = glue("{URL_base}wp-content/uploads/"),
         # converting species name to enter in URLs
         URL_species = str_replace_all(India.Checklist.Common.Name, 
                                       c(" " = "-", "'" = "_")), 
         URL_suf_rangemap = "_rangemap.png",
         URL_suf_trend = "_trend.png") %>% 
  # some long strings
  mutate(featured_image = glue("{URL_pre_uploads}{URL_species}_{MASK.CODE}{URL_suf_rangemap}"),
         map_filename = glue("{URL_pre_uploads}{URL_species}_{MASK.CODE}{URL_suf_rangemap}"),
         map_filename_originals = glue("{URL_pre_uploads}originals/{URL_species}_{MASK.CODE}{URL_suf_rangemap}"),
         graph_filename = glue("{URL_pre_uploads}trends/{URL_species}_{MASK.CODE}{URL_suf_trend}"),
         graph_filename_originals = glue("{URL_pre_uploads}originals/trends/{URL_species}_{MASK.CODE}{URL_suf_trend}")) %>% 
  mutate(full_url_2 = case_when(MASK.TYPE == "national" ~ glue("{custom_url}"),
                                  TRUE ~ glue("{MASK.CODE}-{custom_url}")),
         post_category = MASK.LABEL,
         post_content = MASK.LABEL,
         all_trends = MASK.LABEL,
         habitats = if_else(MASK.TYPE == "habitat", MASK.LABEL, NA_character_),
         conservation_areas = if_else(MASK.TYPE == "conservation_area", MASK.LABEL, NA_character_))

# get list of all masks for each species
web_db <- web_db %>% 
  group_by(India.Checklist.Common.Name, MASK.TYPE) %>% 
  # HTML string, mask codes and mask labels (for states) of all masks of current mask type
  summarise(trends_addn = str_flatten(glue("{MASK.CODE}-{custom_url}"), collapse = ",")) %>% 
  pivot_wider(names_from = MASK.TYPE, 
              values_from = trends_addn, 
              names_glue = "{MASK.TYPE}_{.value}") %>% 
  ungroup() %>% 
  left_join(web_db) %>% 
  mutate(post_name = if_else(MASK.TYPE == "national", national_trends_addn, full_url_2))

# national trend values as separate columns
web_db <- web_db %>% 
  filter(MASK.TYPE == "national") %>% 
  group_by(India.Checklist.Common.Name) %>% 
  reframe(`long-term_trend_in` = `long-term_trend`,
          `long-term_trend_ci_in` = `long-term_trend_ci`,
          current_annual_change_in = current_annual_change,
          current_annual_change_ci_in = current_annual_change_ci,
          distribution_range_size_in = distribution_range_size,
          distribution_range_size_ci_units_of_10000_sqkm_in = distribution_range_size_ci_units_of_10000_sqkm,
          migratory_status_in = migratory_status,
          habitat_specialization_in = habitat_specialization,
          endemicity_in = endemicity) %>% 
  # some species have mask trends but not national, so right join not left
  right_join(web_db, relationship = "many-to-many") %>% 
  mutate(across(ends_with("_in"), ~ ifelse(MASK.TYPE == "national", "", .)))



# convert to website format ---------------------------------------------------------

web_db <- web_db %>% 
  rename(post_title = India.Checklist.Common.Name, 
         scientific_name = India.Checklist.Scientific.Name) %>% 
  dplyr::select(post_title, ID, post_content, post_excerpt, post_date, post_name, post_author,
                post_status, featured_image, wp_page_template, post_format, comment_status,
                ping_status, pinged, post_parent, menu_order, scientific_name, 
                `long-term_trend`, `long-term_trend_ci`, current_annual_change, current_annual_change_ci,
                distribution_range_size, distribution_range_size_ci_units_of_10000_sqkm,
                downloadlink, map_filename, map_filename_originals, graph_filename, graph_filename_originals,
                current_status, distribution_status, iucn_status, long_term_status,
                migratory_status, status_of_conservation_concern, wlpa_schedule,
                primary_assessment, habitat_specialization, endemicity, custom_url, 
                `long-term_trend_in`, `long-term_trend_ci_in`, current_annual_change_in, current_annual_change_ci_in, 
                distribution_range_size_in, distribution_range_size_ci_units_of_10000_sqkm_in,
                migratory_status_in, habitat_specialization_in, endemicity_in,
                national_trends_addn, habitat_trends_addn, state_trends_addn, full_url_2, 
                habitats, conservation_areas, conservation_area_trends_addn, key_states, all_trends, post_category) %>% 
  # converting all NAs to blanks
  mutate(across(everything(), ~ ifelse(is.na(.), "", .)))

write_csv(web_db, file = "20_website/website_database.csv")
