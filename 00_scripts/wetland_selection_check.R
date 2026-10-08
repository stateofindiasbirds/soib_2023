## script to compare the number of grid cells that qualify as

## this is done for two reasons
# 1) to reduce the impact of changes over time on the relative
# birding in wetlands and non-wetlands
# 2) to reduce the impact of cells with only occasional or vagrant records
# on trends

## wetland cells with 4 different criteria
# qualify even if a single wetland list is detected
# 25111 out of 58343
# qualify if 50% of lists in any year are wetland lists
# 19340 out of 58343
# qualify if 50% of lists in any season in any year are wetland lists
# 21771 out of 58343
# qualify if 20% of lists in any season in any year are wetland lists
# 23757 out of 58343 ##### SELECTED!

library(tidyverse)

wetland_filter_1 = read.csv("00_data/grid_wetland_classification_intersection_all_states_yearly_p50.csv")

# total all

wetland_filter_1a = wetland_filter_1 %>%
  mutate(grid_label = ifelse(wetland_proportion > 0, 
                             "Wetland", "Non-wetland")) %>%
  arrange(desc(gridg0),desc(grid_label)) %>%
  dplyr::distinct(gridg0,.keep_all = TRUE) %>%
  dplyr::distinct(gridg0,grid_label) %>%
  mutate(gridg0 = as.character(gridg0))

# total 50%

wetland_filter_1b = wetland_filter_1 %>%
  arrange(desc(gridg0),desc(grid_label)) %>%
  dplyr::distinct(gridg0,.keep_all = TRUE) %>%
  dplyr::distinct(gridg0,grid_label) %>%
  mutate(gridg0 = as.character(gridg0))

# seasonal 50%

wetland_filter_2 = read.csv("00_data/grid_wetland_classification_intersection_all_states_seasonal_p50.csv")

wetland_filter_2a = wetland_filter_2 %>%
  arrange(desc(gridg0),desc(grid_label)) %>%
  dplyr::distinct(gridg0,.keep_all = TRUE) %>%
  dplyr::distinct(gridg0,grid_label) %>%
  mutate(gridg0 = as.character(gridg0))

# seasonal 20%

wetland_filter_3 = read.csv("00_data/grid_wetland_classification_intersection_all_states_seasonal_p20.csv")

wetland_filter_3a = wetland_filter_3 %>%
  arrange(desc(gridg0),desc(grid_label)) %>%
  dplyr::distinct(gridg0,.keep_all = TRUE) %>%
  dplyr::distinct(gridg0,grid_label) %>%
  mutate(gridg0 = as.character(gridg0))

table(wetland_filter_1a$grid_label)
table(wetland_filter_1b$grid_label)
table(wetland_filter_2a$grid_label)
table(wetland_filter_3a$grid_label)