require(tidyverse)
require(glue)

load("00_data/analyses_metadata.RData")


# import ----------------------------------------------------------------------------

# importing all data and setting up
web_db <- map2(analyses_metadata$SOIBMAIN.PATH, analyses_metadata$MASK, 
              ~ read_fn(.x) %>% bind_cols(tibble(MASK = .y))) %>% 
  list_rbind() %>% 
  dplyr::select(-c("eBird.English.Name.2022", "eBird.Scientific.Name.2022", "Order", "Family",
                   starts_with("SOIB."), contains("Breeding.Activity"), "Diet.Guild",
                   starts_with("BLI."), ends_with(".Appendix"), "Onepercent.Estimates", 
                   contains("range25km"), "mean5km", "ci5km",
                   starts_with("proj20"))) %>% 
  # joining MASK.TYPE
  left_join(analyses_metadata %>% distinct(MASK, MASK.TYPE)) %>% 
  relocate(India.Checklist.Common.Name, MASK) %>% 
  arrange(India.Checklist.Common.Name, MASK) %>% 
  mutate(ID = "", post_content = "", post_excerpt = "", post_date = "", post_name = "",
         wp_page_template = "", pinged = "", primary_assessment = "",
         post_author = "amithkumar.4",
         post_status = "publish",
         post_format = "standard", 
         comment_status = "closed", ping_status = "closed",
         post_parent = 0, menu_order = 0)


# creation of fields ----------------------------------------------------------------

web_db2 <- web_db %>% 
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
  rowwise() %>% # else str_c_CI() takes entire column and returns NA
  mutate(`long-term_trend_ci` = str_c_CI(longtermlci, longtermrci),
         current_annual_change_ci = str_c_CI(currentslopelci, currentsloperci),
         distribution_range_size_ci_units_of_10000_sqkm = str_c_CI(rangelci, rangerci)) %>% 
  # some long strings
  mutate(featured_image = glue("{URL_pre_uploads}{URL_species}{URL_suf_rangemap}"),
         downloadlink = glue("{URL_pre_uploads}{URL_species}_Infosheets.jpg"),
         map_filename = glue("{URL_pre_uploads}{URL_species}{URL_suf_rangemap}"),
         map_filename_originals = glue("{URL_pre_uploads}originals/{URL_species}{URL_suf_rangemap}"),
         graph_filename = glue("{URL_pre_uploads}{URL_species}{URL_suf_trend}"),
         graph_filename_originals = glue("{URL_pre_uploads}originals/{URL_species}{URL_suf_trend}"))

# https://wordpress-1024190-3615983.cloudwaysapps.com/wp-content/uploads/Green-Sandpiper_terrain.jpg
# https://wordpress-1024190-3615983.cloudwaysapps.com/birds/ka/grnsan/
# https://wordpress-1024190-3615983.cloudwaysapps.com/wp-content/uploads/karnataka.png

# <a href="https://wordpress-1024190-3615983.cloudwaysapps.com/birds/ka/grnsan/"><span class="soib_trends_img"><img src="https://wordpress-1024190-3615983.cloudwaysapps.com/wp-content/uploads/karnataka.png"></span><span class="soib_trends_lbl">Karnataka</span></a>,
# <a href="https://wordpress-1024190-3615983.cloudwaysapps.com/birds/tn/grnsan/"><span class="soib_trends_img"><img src="https://wordpress-1024190-3615983.cloudwaysapps.com/wp-content/uploads/karnataka.png"></span><span class="soib_trends_lbl">Tamil Nadu</span></a>,
# <a href="https://wordpress-1024190-3615983.cloudwaysapps.com/birds/rj/grnsan/"><span class="soib_trends_img"><img src="https://wordpress-1024190-3615983.cloudwaysapps.com/wp-content/uploads/karnataka.png"></span><span class="soib_trends_lbl">Rajasthan</span></a>,
# <a href="https://wordpress-1024190-3615983.cloudwaysapps.com/birds/ap/grnsan/"><span class="soib_trends_img"><img src="https://wordpress-1024190-3615983.cloudwaysapps.com/wp-content/uploads/karnataka.png"></span><span class="soib_trends_lbl">Andhra Pradesh</span></a>

# <a href="https://wordpress-1024190-3615983.cloudwaysapps.com/birds/tn/grnsan/"><span class="soib_trends_img"><img src="https://wordpress-1024190-3615983.cloudwaysapps.com/wp-content/uploads/karnataka.png"></span><span class="soib_trends_lbl">Tamil Nadu</span></a>,
# <a href="https://wordpress-1024190-3615983.cloudwaysapps.com/birds/rj/grnsan/"><span class="soib_trends_img"><img src="https://wordpress-1024190-3615983.cloudwaysapps.com/wp-content/uploads/karnataka.png"></span><span class="soib_trends_lbl">Rajasthan</span></a>,
# <a href="https://wordpress-1024190-3615983.cloudwaysapps.com/birds/ap/grnsan/"><span class="soib_trends_img"><img src="https://wordpress-1024190-3615983.cloudwaysapps.com/wp-content/uploads/karnataka.png"></span><span class="soib_trends_lbl">Andhra Pradesh</span></a>

# <a href="https://wordpress-1024190-3615983.cloudwaysapps.com/birds/ka/grnsan/"><span class="soib_trends_img"><img src="https://wordpress-1024190-3615983.cloudwaysapps.com/wp-content/uploads/karnataka.png"></span><span class="soib_trends_lbl">Karnataka</span></a>,
# <a href="https://wordpress-1024190-3615983.cloudwaysapps.com/birds/tn/grnsan/"><span class="soib_trends_img"><img src="https://wordpress-1024190-3615983.cloudwaysapps.com/wp-content/uploads/karnataka.png"></span><span class="soib_trends_lbl">Tamil Nadu</span></a>,
# <a href="https://wordpress-1024190-3615983.cloudwaysapps.com/birds/rj/grnsan/"><span class="soib_trends_img"><img src="https://wordpress-1024190-3615983.cloudwaysapps.com/wp-content/uploads/karnataka.png"></span><span class="soib_trends_lbl">Rajasthan</span></a>,
# <a href="https://wordpress-1024190-3615983.cloudwaysapps.com/birds/ap/grnsan/"><span class="soib_trends_img"><img src="https://wordpress-1024190-3615983.cloudwaysapps.com/wp-content/uploads/karnataka.png"></span><span class="soib_trends_lbl">Andhra Pradesh</span></a>

# creation of fields within species (diff. masks) -----------------------------------

# featured image


# convert to website format ---------------------------------------------------------

x <- web_db 
# rename: common, sci names
# relocate
# pivot_wider
