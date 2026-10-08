# This script generates the website database file used to populate the SoIB website. 
# Please keep the following in mind:
#
# 1. Set the priority_update flag correctly. This flag controls whether priority
#    categories should be updated from those established in the last Major update.
#    Priority categories are only revised when there is clear evidence of genuine
#    change in a species' on-the-ground status. Trends and maps, however, are
#    refreshed whenever new data is available, ensuring users always have access
#    to the latest insights.
#
# 2. In the code, trend graphs are generated only when the trend is not classified
#    as "Insufficient data". If the new trend is "Insufficient data" for full
#    country but the previous trend was not, the 2023 report would have a trend graph 
#    while the current update would not — potentially confusing users. 
#    To handle this gracefully, the most recently available graph is displayed in such cases.
#    Note that these operations are not applicable if a state/mask trend is 
#    insuffficient in 2025 and there is a valid trend in 2023. In this case, the
#    insufficient data will be displayed

require(tidyverse)
require(glue)
require(scales)
require(rlang)

source("00_scripts/00_functions.R")
source("00_scripts/20_functions.R")
priority_update <- FALSE # The 2025 update does not update the priority categories

# key states for each species
keystates <- read.csv("01_analyses_full/results/key_state_species_full.csv") %>% 
  arrange(India.Checklist.Common.Name, ST_NM) %>% 
  group_by(India.Checklist.Common.Name) %>% 
  summarise(key_states = str_flatten_comma(ST_NM))

# The current website database
previous_database <- read.csv("20_website/website_database_2025_iucn_update.csv", header = T)


# import ----------------------------------------------------------------------------

web_db0 <- map2(get_metadata()$SOIBMAIN.PATH, get_metadata()$MASK, 
                ~ read_fn(.x) %>% bind_cols(tibble(MASK = .y))) %>% 
  list_rbind() %>% 
  # updating with latest IUCN Status 
  get_latest_IUCN_status("India.Checklist.Common.Name", "IUCN.Category") %>% 
  mutate(India.Checklist.Common.Name = fct_inorder(India.Checklist.Common.Name)) %>% 
  # filtering for SoIB species
  filter(Selected.SoIB == "X") %>% 
  # whether species is new to latest SoIB
  mutate(new_to_soib_2025 = case_when(is.na(SoIB.Major.Update.Priority.Status) & !is.na(SoIB.Latest.Priority.Status) ~ TRUE,
                                      TRUE ~ FALSE)) %>% 
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

all_names <- names(web_db0)



# Before going forward, check whether there are any species for which there is 
# "insufficient data" for latest current/long term trends but had sufficient data
# in the last Major Update (that is, 2023), in a certain mask. This is possible 
# because the determination of "insufficient data" for a mask is not made solely
# on the basis of data volume. It also made on the basis of how the 
# data is spatially distribution in a grid, among other things.

# For such species, trend graphs from the last Major Update are used. 

web_db0_tmp <- web_db0 %>% 
  dplyr::select(c("India.Checklist.Common.Name",
                  "MASK",
                  "SoIB.Latest.Long.Term.Status",
                  "SoIB.Latest.Current.Status",
                  "SoIB.Major.Update.Long.Term.Status",
                  "SoIB.Major.Update.Current.Status"))

# Latest LTT is insufficient data while last Major Update is not, for full country.
use_2023_ltt_species <- web_db0_tmp %>% 
  filter(
    (SoIB.Latest.Long.Term.Status == "Insufficient Data" & 
       SoIB.Major.Update.Long.Term.Status != "Insufficient Data")
  ) %>%
  dplyr::select(c("India.Checklist.Common.Name",
                  "MASK",
                  "SoIB.Latest.Long.Term.Status",
                  "SoIB.Major.Update.Long.Term.Status"))

# Latest CAT is insufficient data while last Major Update is not.
use_2023_cat_species <- web_db0_tmp %>% 
  filter(
    (SoIB.Latest.Current.Status == "Insufficient Data" & 
       SoIB.Major.Update.Current.Status != "Insufficient Data")
  ) %>%
  dplyr::select(c("India.Checklist.Common.Name",
                  "MASK",
                  "SoIB.Major.Update.Current.Status",
                  "SoIB.Latest.Current.Status"))

# Create a new column to flag the species and MASK combination for which there 
# is insufficient data now and had trend estimates from the last major update,
# for full country.

web_db_flag <- web_db0 %>% 
  left_join(
    use_2023_ltt_species %>%
      dplyr::select(c("India.Checklist.Common.Name", "MASK")) %>%
      mutate(flag_fix_ltt = TRUE),
    by = c("India.Checklist.Common.Name", "MASK")
  ) %>%
  mutate(flag_fix_ltt = replace_na(flag_fix_ltt, FALSE))

web_db_flag <- web_db_flag %>% 
  left_join(
    use_2023_cat_species %>%
      dplyr::select(c("India.Checklist.Common.Name", "MASK")) %>%
      mutate(flag_fix_cat = TRUE),
    by = c("India.Checklist.Common.Name", "MASK")
  ) %>%
  mutate(flag_fix_cat = replace_na(flag_fix_cat, FALSE))

# Depending on the priority update value, different columns are dropped
# If priority update is TRUE, we are interested in latest priorities so we drop
# the major update priority columns. If priority update is FALSE, vice-versa

cols_to_drop <- if (priority_update) {
  c(
    "eBird.English.Name.2024", "eBird.Scientific.Name.2024", "Order", "Family",
    grep("Breeding.Activity", all_names, value = TRUE),
    "Diet.Guild",
    "SoIB.Major.Update.Long.Term.Status",
    "SoIB.Major.Update.Current.Status",
    "SoIB.Major.Update.Range.Status",
    "SoIB.Major.Update.Priority.Status",
    grep("^BLI\\.", all_names, value = TRUE),
    grep("\\.Appendix$", all_names, value = TRUE),
    "Onepercent.Estimates",
    grep("range25km", all_names, value = TRUE),
    "mean5km", "ci5km",
    grep("^proj20", all_names, value = TRUE)
  )
} else {
  c(
    "eBird.English.Name.2024", "eBird.Scientific.Name.2024", "Order", "Family",
    grep("Breeding.Activity", all_names, value = TRUE),
    "Diet.Guild",
    "SoIB.Latest.Long.Term.Status",
    "SoIB.Latest.Current.Status",
    "SoIB.Latest.Range.Status",
    "SoIB.Latest.Priority.Status",
    grep("^BLI\\.", all_names, value = TRUE),
    grep("\\.Appendix$", all_names, value = TRUE),
    "Onepercent.Estimates",
    grep("range25km", all_names, value = TRUE),
    "mean5km", "ci5km",
    grep("^proj20", all_names, value = TRUE)
  )
}

# Drop the columns
web_db0 <- web_db_flag %>%
  dplyr::select(-all_of(cols_to_drop))

#names(web_db0_test)

# taxonomic order to arrange species
tax_order <- levels(web_db0$India.Checklist.Common.Name) 


# creation of fields ----------------------------------------------------------------

# Rename columns. If priority update is FALSE, the last Major update will be
# assigned to long_term_status and current_status. 
# Otherwise, latest status will be assigned to long_term_status and current_status.

if (priority_update) {
  rename_cols <- exprs(`long-term_trend` = longtermmean,
                       current_annual_change = currentslopemean,
                       distribution_range_size = rangemean,
                       iucn_status = IUCN.Category,
                       migratory_status = Migratory.Status.Within.India,
                       wlpa_schedule = WPA.Schedule,
                       habitat_specialization = Habitat.Specialization,
                       endemicity = Endemic.Region,
                       custom_url = eBird.Code,
                       current_status = SoIB.Latest.Current.Status,
                       distribution_status = SoIB.Latest.Range.Status,
                       long_term_status = SoIB.Latest.Long.Term.Status,
                       status_of_conservation_concern = SoIB.Latest.Priority.Status
  )
  
} else {
  rename_cols <- exprs(`long-term_trend` = longtermmean,
                       current_annual_change = currentslopemean,
                       distribution_range_size = rangemean,
                       iucn_status = IUCN.Category,
                       migratory_status = Migratory.Status.Within.India,
                       wlpa_schedule = WPA.Schedule,
                       habitat_specialization = Habitat.Specialization,
                       endemicity = Endemic.Region,
                       custom_url = eBird.Code,
                       current_status = SoIB.Major.Update.Current.Status,
                       distribution_status = SoIB.Major.Update.Range.Status,
                       long_term_status = SoIB.Major.Update.Long.Term.Status,
                       status_of_conservation_concern = SoIB.Major.Update.Priority.Status
  )
}

web_db <- web_db0 %>% 
  # TEMPORARY FIX for subnational SoIB Priority Status (retain national Status)
  temp_priority_correction() %>% 
  # join key states for each species
  left_join(keystates, by = "India.Checklist.Common.Name") %>% 
  rename(!!!rename_cols) %>%
  mutate(across(c("long-term_trend", "current_annual_change"), ~ round(., 2))) %>% 
  # adding commas to large values of range size
  mutate(across(c("distribution_range_size", "rangelci", "rangerci"), ~ label_comma()(as.numeric(.)))) %>% 
  # on website, we want a filter to show only species which have trend graph (LTT or CAT)
  # trend graphs not plotted for Insufficient Data
  mutate(only_conclusive_trend = case_when(
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


# Create URLs for the trend and map images

web_db_2 <- web_db %>%
  # setup for some long strings
  mutate(URL_base = "https://stateofindiasbirds.in/wp-content/uploads/",
         # prefix for uploads
         #URL_pre_uploads = glue("{URL_base}wp-content/uploads/"),
         # For graphs
         # subfolder structure for SoIB 2025 original images
         URL_graph_year_ltt = ifelse(flag_fix_ltt == TRUE, "2023", "2025"), # For distribution map,
         URL_graph_year_cat = ifelse(flag_fix_cat == TRUE, "2023", "2025"),
         # converting species name to enter in URLs
         URL_species = str_replace_all(India.Checklist.Common.Name, 
                                       c(" " = "-", "'" = "_")),
         URL_scientific_name = str_replace_all(India.Checklist.Scientific.Name, 
                                               c(" " = "_")),
         feature_image_extension = ".jpg",
         URL_suf_rangemap = "_map_2025.jpg",
         URL_suf_trend_LTT = "_LTT_trend.png",
         URL_suf_trend_CAT = "_CAT_trend.png") %>% 
  # some long strings
  mutate(featured_image = glue("{URL_base}{URL_scientific_name}{feature_image_extension}"),
         map_filename = glue("{URL_base}maps/2025/display/{URL_species}_{MASK.CODE}{URL_suf_rangemap}"),
         map_filename_originals = glue("{URL_base}maps/2025/original/{URL_species}_{MASK.CODE}{URL_suf_rangemap}"),
         
         # originals for download:
         graph_filename_originals = case_when(
           long_term_status != "Insufficient Data" ~ glue("{URL_base}trends/{URL_graph_year_ltt}/original/{URL_species}_{MASK.CODE}{URL_suf_trend_LTT}"),
           TRUE ~ NA_character_
         ),
         graph_filename_originals_cat = case_when(
           current_status != "Insufficient Data" ~ glue("{URL_base}trends/{URL_graph_year_cat}/original/{URL_species}_{MASK.CODE}{URL_suf_trend_CAT}"),
           TRUE ~ NA_character_
         ),
         graph_filename = coalesce(
           graph_filename_originals,
           graph_filename_originals_cat
         )) %>%
  mutate(graph_filename = str_replace(graph_filename, "original", "display")) %>%
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
    long_term_status != "Insufficient Data" | current_status != "Insufficient Data" ~ glue("est-{custom_url}"),
    TRUE ~ glue("not-{custom_url}")
  ))

# Check how many species have 2023 trends instead of 2025, for full country

tmp <- web_db_2 %>%
  filter(str_detect(graph_filename, "2023") & 
           MASK == "none")


web_db_3 <- web_db_2 %>% 
  # get list of all masks for each species
  # HTML string, mask codes and mask labels (for states) of all masks of current mask type
  group_by(India.Checklist.Common.Name, MASK.TYPE) %>% 
  summarise(trends_addn = str_flatten(glue("{MASK.CODE}-{custom_url_estnot}"), collapse = ",")) %>% 
  pivot_wider(names_from = MASK.TYPE, 
              values_from = trends_addn, 
              names_glue = "{MASK.TYPE}_{.value}") %>% 
  ungroup() %>% 
  left_join(web_db_2) %>% 
  mutate(full_url_2 = if_else(MASK.TYPE == "national", national_trends_addn, post_name)) 

# national trend values as separate columns
web_db_4 <- web_db_3 %>% 
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
  right_join(web_db_3, relationship = "many-to-many") 



# convert to website format ---------------------------------------------------------

web_db_5 <- web_db_4 %>% 
  rename(post_title = India.Checklist.Common.Name, 
         scientific_name = India.Checklist.Scientific.Name) %>% 
  dplyr::select(post_title, ID, post_content, post_excerpt, post_date, post_name, post_author,
                post_status, featured_image, wp_page_template, post_format, comment_status,
                ping_status, pinged, post_parent, menu_order, scientific_name, 
                `long-term_trend`, `long-term_trend_ci`, current_annual_change, current_annual_change_ci,
                distribution_range_size, distribution_range_size_ci_units_of_10000_sqkm,
                downloadlink, map_filename, map_filename_originals, graph_filename, 
                graph_filename_originals, graph_filename_originals_cat,
                current_status, distribution_status, iucn_status, long_term_status,
                migratory_status, status_of_conservation_concern, wlpa_schedule,
                primary_assessment, habitat_specialization, endemicity, custom_url, custom_url_estnot, 
                `long-term_trend_in`, `long-term_trend_ci_in`, current_annual_change_in, current_annual_change_ci_in, 
                distribution_range_size_in, distribution_range_size_ci_units_of_10000_sqkm_in,
                migratory_status_in, habitat_specialization_in, endemicity_in,
                national_trends_addn, habitat_trends_addn, state_trends_addn, full_url_2, 
                habitats, conservation_areas, conservation_area_trends_addn, key_states, all_trends, 
                post_category, only_conclusive_trend, new_to_soib_2025, flag_fix_ltt, flag_fix_cat, MASK.CODE) %>% 
  # converting all NAs to blanks
  mutate(across(everything(), ~ ifelse(is.na(.), "", .))) %>% 
  # sort taxonomically
  mutate(post_title = factor(post_title, levels = tax_order)) %>% 
  arrange(post_title)


# Some columns from the previous website database have to be pulled into this
# update. These are new_to_soib_2023, custom_URL and post_name. 
# custom_URL will be renamed to old_URL_2020 and post_name to old_URL_2023.
# However, the species names have changed which makes matching the two databases
# difficult. 

# Introduce a column of new names in the previous database

previous_database_tmp <- previous_database %>%
  rename(old_URL_2020 = custom_url,
         old_URL_2023 = post_name,
  )

# The post_name in the older database represents India.Checklist.Common.Name 
# from 2022. So this cannot be combined with the current file directly. 
# In the older database, use the SoIB mapping 2022 file to get the eBird 2022 names. 
# Then use the taxonomy file to pull the eBird 2024 names. 
# Then use the 2024 mapping file to pull India.Checklist.Common.Name from
# 2024 (post_title in the updated database)

map_2022 <- read.csv("00_data/SoIB_mapping_2022.csv")
map_2024 <- read.csv("00_data/SoIB_mapping_2024.csv")

taxmap <- read.csv("00_data/eBird_taxonomy_mapping.csv")

# Get eBird.English.Name.2022 using the 2022 mapping file
previous_database_tmp_2  <- previous_database_tmp %>% 
  left_join(map_2022 %>% dplyr::select(c(eBird.English.Name.2022, India.Checklist.Common.Name)),
            by = join_by(post_title == India.Checklist.Common.Name))

# Oriental Dwarf Kingfisher has been split in 2022 into Black-backed and 
# Rufous-backed. So more rows are generated in the final output

map_2022_fix <- map_2022 %>% filter(eBird.English.Name.2022 != 
                                      "Rufous-backed Dwarf-Kingfisher")


previous_database_tmp_2  <- previous_database_tmp %>% 
  left_join(map_2022_fix %>% dplyr::select(c(eBird.English.Name.2022, India.Checklist.Common.Name)),
            by = join_by(post_title == India.Checklist.Common.Name))


# Get eBird.English.Name.2024 from the taxonomy mapping file
previous_database_tmp_3 <- previous_database_tmp_2 %>%
  left_join(taxmap %>% dplyr::select(c(eBird.English.Name.2022, eBird.English.Name.2024)),
            by = "eBird.English.Name.2022")


# Get India.Checklist.Common.Name 2024 using the 2024 mapping file
previous_database_tmp_4 <- previous_database_tmp_3 %>%
  left_join(map_2024 %>% dplyr::select(c(eBird.English.Name.2024, India.Checklist.Common.Name)),
            by = "eBird.English.Name.2024")

# At this stage, we have India.Checklist.Common.Name from both 2022 and 2024 in
# one object. 
# In this object, post_name = India.Checklist.Common.Name from 2022
# India.Checklist.Common.Name = India.Checklist.Common.Name from 2024

# This object now contains names from 2024 so that it can be joined with the 
# current database to pull the required columns

previous_database_tmp_5 <- previous_database_tmp_4 %>%
  dplyr::select(-all_of(c("eBird.English.Name.2022", "eBird.English.Name.2024")))

# Are there any empty or NA India.Checklist.Common.Name

which(is.na(previous_database_tmp_5$India.Checklist.Common.Name) | previous_database_tmp_5$India.Checklist.Common.Name == "")


web_db_6 <- web_db_5 %>%
  left_join(previous_database_tmp_5 %>%
              dplyr::select(c("old_URL_2020",
                              "old_URL_2023",
                              "new_to_soib_2023",
                              "India.Checklist.Common.Name",
                              "post_content")), 
            by = join_by(post_title == India.Checklist.Common.Name, post_content))

# At this point, the English Names and Scientific Names reflect the 2024 taxonomy
# from India Checklist. 
# But to maintain continuity on the website, we will revert to the 2023 names.
# The map and trend URLs will still bear the 2024 names except in cases where
# we have to revert to the 2023 names

# 2024 India - (2024 India, 2024 eBird - SoIB mapping 2024) - (2024 ebird, 2022 ebird - taxonomy mapping) - (2022 ebird, 2022 India - SoIB mapping 2022)


web_db_7 <- web_db_6 %>% left_join(map_2024 %>%
                                     dplyr::select(c("eBird.English.Name.2024", "India.Checklist.Common.Name")),
                                   by = join_by(post_title == India.Checklist.Common.Name))

web_db_8 <- web_db_7 %>% left_join(taxmap %>%
                                     dplyr::select(c("eBird.English.Name.2022", "eBird.English.Name.2024")),
                                   by = "eBird.English.Name.2024")



# Note 1: Pay close attention to species which have been lumped in 2025.

# For example, Striated Swallow and Red-rumped Swallow have been lumped into 
# one species - Eastern Red-rumped Swallow in 2025. But reverting to 2023 names 
# means that we show both Striated and Red-rumped swallow. Strangely, there is 
# no Striated Swallow. 

# Removing striated swallow

web_db_9 <- web_db_8 %>% filter(is.na(eBird.English.Name.2022) | eBird.English.Name.2022 != "Striated Swallow")

# Use soib_mapping_2022 to pull the corresponding 2022 India Checklist names

web_db_10 <- web_db_9 %>% left_join(map_2022 %>%
                                      dplyr::select("eBird.English.Name.2022",
                                                    "India.Checklist.Common.Name",
                                                    "India.Checklist.Scientific.Name"),
                                    by = "eBird.English.Name.2022")

# Are there any NAs in the pulled India.Checklist.Common.Name, "India.Checklist.Scientific.Name",

which(is.na(web_db_10$India.Checklist.Common.Name) | is.na(web_db_10$India.Checklist.Scientific.Name))

web_db_11 <- web_db_10 %>%
  dplyr::select(
    -"eBird.English.Name.2022",
    -"eBird.English.Name.2024"
  ) %>%
  rename(
    India.Checklist.Common.Name.2022 = India.Checklist.Common.Name,
    India.Checklist.Scientific.Name.2022 = India.Checklist.Scientific.Name
  ) 


web_db_12 <- web_db_11 %>% 
  mutate(URL_base = "https://stateofindiasbirds.in/wp-content/uploads/",
         # setup for some long strings
         # prefix for uploads
         #URL_pre_uploads = glue("{URL_base}wp-content/uploads/"),
         # For graphs
         # subfolder structure for SoIB 2025 original images
         URL_graph_year_ltt = ifelse(flag_fix_ltt == TRUE, "2023", "2025"), # For distribution map,
         URL_graph_year_cat = ifelse(flag_fix_cat == TRUE, "2023", "2025"),
         # converting species name to enter in URLs
         URL_species_map = str_replace_all(post_title, 
                                           c(" " = "-", "'" = "_")),
         URL_species_ltt = ifelse(flag_fix_ltt == TRUE, str_replace_all(India.Checklist.Common.Name.2022, 
                                                                        c(" " = "-", "'" = "_")), str_replace_all(post_title, 
                                                                                                                  c(" " = "-", "'" = "_"))),
         URL_species_cat = ifelse(flag_fix_cat == TRUE, str_replace_all(India.Checklist.Common.Name.2022,
                                                                        c(" " = "-", "'" = "_")), str_replace_all(post_title, 
                                                                                                                  c(" " = "-", "'" = "_"))),
         URL_scientific_name = str_replace_all(India.Checklist.Scientific.Name.2022, 
                                               c(" " = "_")), # This is used to 
         feature_image_extension = ".jpg",
         URL_suf_rangemap = "_map_2025.jpg",
         URL_suf_trend_LTT = "_LTT_trend.png",
         URL_suf_trend_CAT = "_CAT_trend.png") %>% 
  # some long strings
  mutate(featured_image = glue("{URL_base}{URL_scientific_name}{feature_image_extension}"),
         map_filename = glue("{URL_base}maps/2025/display/{URL_species_map}_{MASK.CODE}{URL_suf_rangemap}"),
         map_filename_originals = glue("{URL_base}maps/2025/original/{URL_species_map}_{MASK.CODE}{URL_suf_rangemap}"),
         
         # originals for download:
         graph_filename_originals = case_when(
           long_term_status != "Insufficient Data" ~ glue("{URL_base}trends/{URL_graph_year_ltt}/original/{URL_species_ltt}_{MASK.CODE}{URL_suf_trend_LTT}"),
           TRUE ~ NA_character_
         ),
         graph_filename_originals_cat = case_when(
           current_status != "Insufficient Data" ~ glue("{URL_base}trends/{URL_graph_year_cat}/original/{URL_species_cat}_{MASK.CODE}{URL_suf_trend_CAT}"),
           TRUE ~ NA_character_
         ),
         graph_filename = coalesce(
           graph_filename_originals,
           graph_filename_originals_cat
         )) %>%
  mutate(graph_filename = str_replace(graph_filename, "original", "display"))


tmp <- web_db_12 %>%
  filter(str_detect(graph_filename, "display"))


web_db_13 <- web_db_12 %>%
  dplyr::select(
    -"URL_base",
    -"URL_graph_year_ltt",
    -"URL_graph_year_cat",
    -"URL_species_map",
    -"URL_species_ltt",
    -"URL_species_cat",
    -"URL_scientific_name",
    -"feature_image_extension",                          
    -"URL_suf_rangemap",
    -"URL_suf_trend_LTT",
    -"URL_suf_trend_CAT",
    -"post_title",
    -"scientific_name",
    -"MASK.CODE",
    -"flag_fix_ltt",
    -"flag_fix_cat"  
  ) %>%
  rename(
    post_title = India.Checklist.Common.Name.2022,
    scientific_name = India.Checklist.Scientific.Name.2022
  ) %>%
  relocate(post_title) %>%
  relocate(scientific_name, .after=menu_order)


# Checks: Rows where graph plots are from 2023

tmp <- web_db_13 %>%
  filter(str_detect(graph_filename, "2023"))

# Revert to 2023 only if the graph plots are for full country. All the others
# will re-labelled NA

web_db_14 <- web_db_13 %>%
  mutate(
    graph_filename = case_when(
      str_detect(as.character(graph_filename), "2023") ~ NA_character_,
      TRUE ~ as.character(graph_filename)
    ),
    graph_filename_originals = case_when(
      str_detect(as.character(graph_filename_originals), "2023") ~ NA_character_,
      TRUE ~ as.character(graph_filename_originals)
    ),
    graph_filename_originals_cat = case_when(
      str_detect(as.character(graph_filename_originals_cat), "2023") ~ NA_character_,
      TRUE ~ as.character(graph_filename_originals_cat)
  )
  )
  
tmp <- web_db_14 %>%
  filter(str_detect(graph_filename, "2023"))

# No species will have 2023 graphs displayed.

# Add photographer credits. Some changes in 2025 ----

credits <- read.csv("20_website/photo_credits.csv")

credits_update <- credits %>% 
  mutate(photo_credit = dplyr::replace_when(photo_credit, 
                                            scientific_name == "Sylviparus modestus" ~ "Bharat Tiwari",
                                            scientific_name == "Periparus ater" ~ "Jaswinder Singh",
                                            scientific_name == "Tadorna ferruginea" ~ "Bhaarat Vyas",
                                            scientific_name == "Mareca strepera" ~ "Arpit Bansal",
                                            scientific_name == "Haliaeetus ichthyaetus" ~ "Parthasarathi Chakrabarti",
                                            scientific_name == "Leptoptilos dubius" ~ "Parthasarathi Chakrabarti",
                                            scientific_name == "Ducula badia" ~ "Abhishek Das")) 

write.csv(credits_update, file = "20_website/photo_credits_update.csv", row.names = FALSE)


web_db_15 <- web_db_14 %>% left_join(credits_update)

web_db_15$update <- "2025 Update"

write.csv(web_db_15, file = "20_website/website_database_2025_update.csv", 
          row.names = FALSE)



