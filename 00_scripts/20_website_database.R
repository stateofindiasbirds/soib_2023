require(tidyverse)
require(glue)
require(scales)

source("00_scripts/00_functions.R")
source("00_scripts/20_functions.R")

# key states for each species
keystates <- read.csv("01_analyses_full/results/key_state_species_full.csv") %>% 
  arrange(India.Checklist.Common.Name, ST_NM) %>% 
  group_by(India.Checklist.Common.Name) %>% 
  summarise(key_states = str_flatten_comma(ST_NM))

# import ----------------------------------------------------------------------------

# importing all data and setting up
web_db0 <- map2(get_metadata()$SOIBMAIN.PATH, get_metadata()$MASK, 
              ~ read_fn(.x) %>% bind_cols(tibble(MASK = .y))) %>% 
  list_rbind() %>% 
  # updating with latest IUCN Status
  get_latest_IUCN_status("India.Checklist.Common.Name", "IUCN.Category") %>% 
  mutate(India.Checklist.Common.Name = fct_inorder(India.Checklist.Common.Name)) %>% 
  # filtering for SoIB species
  filter(Selected.SoIB == "X") %>%
  # whether species is new to latest SoIB
  mutate(new_to_soib = case_when(is.na(SoIB.Past.Priority.Status) & !is.na(SoIB.Latest.Priority.Status) ~ TRUE,
                                 TRUE ~ FALSE)) %>% 
  dplyr::select(-c("eBird.English.Name.2023", "eBird.Scientific.Name.2023", "Order", "Family",
                   starts_with("SoIB."), contains("Breeding.Activity"), "Diet.Guild",
                   starts_with("BLI."), ends_with(".Appendix"), "Onepercent.Estimates", 
                   contains("range25km"), "mean5km", "ci5km",
                   starts_with("proj20"))) %>% 
  # joining MASK.TYPE
  left_join(get_metadata() %>% distinct(MASK, MASK.TYPE)) %>% 
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

# taxonomic order to arrange species
tax_order <- levels(web_db0$India.Checklist.Common.Name)

# creation of fields ----------------------------------------------------------------

web_db <- web_db0 %>% 
  # TEMPORARY FIX for subnational SoIB Priority Status (retain national Status)
  temp_priority_correction() %>% 
  # join key states for each species
  left_join(keystates, by = "India.Checklist.Common.Name") %>% 
  rename(`long-term_trend` = longtermmean,
         current_annual_change = currentslopemean,
         distribution_range_size = rangemean,
         current_status = SoIB.Latest.Current.Status,
         distribution_status = SoIB.Latest.Range.Status,
         iucn_status = IUCN.Category,
         long_term_status = SoIB.Latest.Long.Term.Status,
         migratory_status = Migratory.Status.Within.India,
         status_of_conservation_concern = SoIB.Latest.Priority.Status,
         wlpa_schedule = WPA.Schedule,
         habitat_specialization = Habitat.Specialization,
         endemicity = Endemic.Region,
         custom_url = eBird.Code) %>% 
  mutate(across(c("long-term_trend", "current_annual_change"), ~ round(., 2))) %>% 
  # adding commas to large values of range size
  mutate(across(c("distribution_range_size", "rangelci", "rangerci"), ~ label_comma()(.))) %>% 
  # on website, we want a filter to show only species which have trend graph (LTT or CAT)
  # trend graphs not plotted for Insufficient Data
  mutate(only_estimated_trend = case_when(
    long_term_status == "Insufficient Data" & current_status == "Insufficient Data" ~ "No",
    TRUE ~ "Yes"
    )) %>% 
  str_c_CI(., longtermlci, longtermrci, new_name = "long-term_trend_ci") %>% 
  str_c_CI(., currentslopelci, currentsloperci, new_name = "current_annual_change_ci") %>% 
  str_c_CI(., rangelci, rangerci, new_name = "distribution_range_size_ci_units_of_10000_sqkm") %>% 
  join_mask_codes() %>% 
  # change mask labels from acronym for website
  mutate(MASK.LABEL = case_when(MASK.LABEL == "PAs" ~ "Protected Areas",
                                MASK.LABEL == "ONEs" ~ "Open Natural Ecosystems",
                                TRUE ~ MASK.LABEL))


# creation of fields within species (diff. masks) -----------------------------------

web_db <- web_db %>% 
  # setup for some long strings
  mutate(URL_base = "https://wordpress-1024190-3615983.cloudwaysapps.com/",
         # prefix for uploads
         URL_pre_uploads = glue("{URL_base}wp-content/uploads/"),
         # subfolder structure for SoIB 2023 original images
         URL_orig_substr = "originals/2023/",
         # converting species name to enter in URLs
         URL_species = str_replace_all(India.Checklist.Common.Name, 
                                       c(" " = "-", "'" = "_")), 
         URL_suf_rangemap = "_map_2023.jpg",
         URL_suf_trend_LTT = "_LTT_trend.png",
         URL_suf_trend_CAT = "_CAT_trend.png") %>% 
  # some long strings
  mutate(featured_image = glue("{URL_pre_uploads}{URL_species}_{MASK.CODE}{URL_suf_rangemap}"),
         map_filename = glue("{URL_pre_uploads}maps/{URL_species}_{MASK.CODE}{URL_suf_rangemap}"),
         map_filename_originals = glue("{URL_pre_uploads}{URL_orig_substr}{URL_species}_{MASK.CODE}{URL_suf_rangemap}"),
         # no trend plot if both LTT and CAT absent
         # graph in species card:
         graph_filename = case_when(
           !is.na(`long-term_trend`) ~ glue("{URL_pre_uploads}trends/{URL_species}_{MASK.CODE}{URL_suf_trend_LTT}"),
           !is.na(current_annual_change) ~ glue("{URL_pre_uploads}trends/{URL_species}_{MASK.CODE}{URL_suf_trend_CAT}"),
           TRUE ~ NA_character_
         ),
         # originals for download:
         graph_filename_originals_LTT = case_when(
           !is.na(`long-term_trend`) ~ glue("{URL_pre_uploads}{URL_orig_substr}trends/{URL_species}_{MASK.CODE}{URL_suf_trend_LTT}"),
           TRUE ~ NA_character_
         ),
         graph_filename_originals_CAT = case_when(
           !is.na(current_annual_change) ~ glue("{URL_pre_uploads}{URL_orig_substr}trends/{URL_species}_{MASK.CODE}{URL_suf_trend_CAT}"),
           TRUE ~ NA_character_
         )) %>%
  mutate(post_name = case_when(MASK.TYPE == "national" ~ glue("{custom_url}"),
                               TRUE ~ glue("{MASK.CODE}-{custom_url}")),
         post_category = MASK.LABEL,
         post_content = MASK.LABEL,
         all_trends = MASK.LABEL,
         habitats = if_else(MASK.TYPE == "habitat", MASK.LABEL, "None"),
         conservation_areas = if_else(MASK.TYPE == "conservation_area", MASK.LABEL, "None")) %>% 
  # no maps for habitats/CAs, so show India map
  mutate(across(c(featured_image, starts_with("map_filename")), 
                ~ case_when(!MASK.TYPE %in% c("habitat", "conservation_area") ~ .,
                            TRUE ~ str_replace(., glue("_{MASK.CODE}_"), "_in_")))) %>%
  # _addn columns need to contain info about whether or not that species-mask combo has trend 
  mutate(custom_url_estnot = case_when(
    !is.na(`long-term_trend`) | !is.na(current_annual_change) ~ glue("est-{custom_url}"),
    TRUE ~ glue("not-{custom_url}")
  ))

web_db <- web_db %>% 
  # get list of all masks for each species
  # HTML string, mask codes and mask labels (for states) of all masks of current mask type
  group_by(India.Checklist.Common.Name, MASK.TYPE) %>% 
  summarise(trends_addn = str_flatten(glue("{MASK.CODE}-{custom_url_estnot}"), collapse = ",")) %>% 
  pivot_wider(names_from = MASK.TYPE, 
              values_from = trends_addn, 
              names_glue = "{MASK.TYPE}_{.value}") %>% 
  ungroup() %>% 
  left_join(web_db) %>% 
  mutate(full_url_2 = if_else(MASK.TYPE == "national", national_trends_addn, post_name)) 

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
  right_join(web_db, relationship = "many-to-many") 



# convert to website format ---------------------------------------------------------

web_db <- web_db %>% 
  rename(post_title = India.Checklist.Common.Name, 
         scientific_name = India.Checklist.Scientific.Name) %>% 
  dplyr::select(post_title, ID, post_content, post_excerpt, post_date, post_name, post_author,
                post_status, featured_image, wp_page_template, post_format, comment_status,
                ping_status, pinged, post_parent, menu_order, scientific_name, 
                `long-term_trend`, `long-term_trend_ci`, current_annual_change, current_annual_change_ci,
                distribution_range_size, distribution_range_size_ci_units_of_10000_sqkm,
                downloadlink, map_filename, map_filename_originals, graph_filename, 
                graph_filename_originals_LTT, graph_filename_originals_CAT,
                current_status, distribution_status, iucn_status, long_term_status,
                migratory_status, status_of_conservation_concern, wlpa_schedule,
                primary_assessment, habitat_specialization, endemicity, custom_url, custom_url_estnot, 
                `long-term_trend_in`, `long-term_trend_ci_in`, current_annual_change_in, current_annual_change_ci_in, 
                distribution_range_size_in, distribution_range_size_ci_units_of_10000_sqkm_in,
                migratory_status_in, habitat_specialization_in, endemicity_in,
                national_trends_addn, habitat_trends_addn, state_trends_addn, full_url_2, 
                habitats, conservation_areas, conservation_area_trends_addn, key_states, all_trends, 
                post_category, only_estimated_trend, new_to_soib) %>% 
  # converting all NAs to blanks
  mutate(across(everything(), ~ ifelse(is.na(.), "", .))) %>% 
  # sort taxonomically
  mutate(post_title = factor(post_title, levels = tax_order)) %>% 
  arrange(post_title)
  

write_csv(web_db, file = "20_website/website_database.csv")


# add photographer credits and remove NAs

library(tidyverse)

web_db = read.csv("20_website/website_database_2024.csv")
credits = read.csv("20_website/photo_credits.csv")

web_db = web_db %>% left_join(credits)
web_db$featured_image <- paste(web_db$scientific_name,".jpg",sep="")
web_db$featured_image <- gsub(" ", "_", web_db$featured_image)
web_db[is.na(web_db)] <- ""

web_db = data.frame(lapply(web_db, function(x) gsub("https://wordpress-1024190-3615983.cloudwaysapps.com/", "https://stateofindiasbirds.in/", x)), stringsAsFactors = FALSE)

write_csv(web_db, file = "20_website/website_database_credits_2024.csv")
