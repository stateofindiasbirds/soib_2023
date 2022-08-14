# Quick analysis to determine whether or not historical eBird data in the country is 
# spatially biased, at two levels: 
# - pre-2000 
# - pre-1990 & 1990-2000

# All these analyses are using the base dataset, without any of the SoIB filters, 
# but with removal of duplicate lists.

### Dependencies in the sections "Setup" and "Data:" ###


### Setup ####

library(tidyverse)
library(raster)
library(sf)
library(patchwork)

# ggplot theme
theme_set(theme_classic() +
            theme(strip.background = element_blank(),
                  strip.text = element_text(size = 12, face = "bold"),
                  plot.title = element_text(size = 16, face = "bold"),
                  panel.border = element_blank()))

# functions for historical spread analyses
source("hist_spread/hist_spread_functions.R")

### Data: initial setup (SKIP if latest hist_spread.RData exists) ####

### uncomment below if first time setting up. else, just load .RData (comment/skip lines below)

# # filtering data for this analysis
# load("ebd_IN_relJun-2022.RData")
# 
# tictoc::tic("distinct() over slice()") # total 64 secs
# timeline <- data %>%
#   # filtering for complete lists
#   filter(ALL.SPECIES.REPORTED == 1 & PROTOCOL.TYPE != "Incidental") %>%
#   # slicing to original checklist-level (GROUP.ID)
#   distinct(GROUP.ID,
#            LOCALITY.ID, LOCALITY, LOCALITY.TYPE, STATE, COUNTY, LATITUDE, LONGITUDE, # space
#            OBSERVATION.DATE, YEAR, MONTH, DAY.M, TIME.OBSERVATIONS.STARTED, # time
#            PROTOCOL.TYPE, DURATION.MINUTES, EFFORT.DISTANCE.KM, NUMBER.OBSERVERS, # effort
#            TRIP.COMMENTS) %>%  # done in 41 seconds on server (faster than slice)
#   # have to group-slice again as space, time, effort can vary btwn observers in each GROUP.ID
#   group_by(GROUP.ID) %>%
#   slice(1) %>%
#   ungroup() %>%
#   mutate(TIME.SOIB1 = case_when(YEAR < 1990 ~ "pre-1990",
#                                 YEAR %in% 1990:1999 ~ "1990-1999",
#                                 YEAR %in% 2000:2006 ~ "2000-2006",
#                                 YEAR %in% 2007:2010 ~ "2007-2010",
#                                 YEAR > 2010 ~ as.character(YEAR)),
#          # coarser periods: without splitting historical and 2011-13
#          TIME.SOIB2 = case_when(YEAR < 2000 ~ "pre-2000",
#                                 YEAR %in% 2000:2006 ~ "2000-2006",
#                                 YEAR %in% 2007:2010 ~ "2007-2010",
#                                 YEAR %in% 2011:2013 ~ "2011-2013",
#                                 YEAR > 2013 ~ as.character(YEAR)),
#          # for main paper looking at past, middle and present broadly
#          TIME.BROAD = case_when(YEAR < 2000 ~ "pre-2000",
#                                 YEAR %in% 2000:2015 ~ "2000-2015",
#                                 YEAR > 2015 ~ "2016-present")) %>%
#   mutate(TIME.SOIB1 = factor(TIME.SOIB1,
#                              levels = c("pre-1990", "1990-1999", "2000-2006", "2007-2010",
#                                         as.character(2011:2021))),
#          TIME.SOIB2 = factor(TIME.SOIB2,
#                              levels = c("pre-2000", "2000-2006", "2007-2010", "2011-2013",
#                                         as.character(2014:2021))),
#          TIME.BROAD = factor(TIME.BROAD,
#                              levels = c("pre-2000", "2000-2015",
#                                         as.character(2016:2021))))
# tictoc::toc()
# 
# 
# # adding map variables to main data
# load("maps.RData") # Ashwin's maps data 
# timeline <- joinmapvars(timeline)
# 
# 
# data_hist <- timeline %>% filter(YEAR < 2000)
# data_cur <- timeline %>% filter(YEAR == 2021)
# 
# 
# save(data_hist, data_cur, timeline, file = "hist_spread/hist_spread.RData")
# rm(data)
# 
# 
# # region data (from ashwinv2005/state-of-indias-birds)
# regions <- read_csv("hist_spread/districtlist.csv") %>%
#   rename(REGION = region,
#          STATE = ST_NM,
#          COUNTY = DISTRICT)


### Data: load .RData files (SKIP if above section run) ####

### if latest hist_spread.RData does not exist, comment below and run above section

# Ashwin's maps data
load("maps.RData")

# latest .RData with required objects (created in previous section)
load("hist_spread/hist_spread.RData")

# region data (from ashwinv2005/state-of-indias-birds)
regions <- read_csv("hist_spread/districtlist.csv") %>% 
  rename(REGION = region,
         STATE = ST_NM,
         COUNTY = DISTRICT)


### Preparing data for analyses ####

# comparisons for SoIB
soib_levels <- c("2021", "pre-2000", "1990-1999", "pre-1990")

# tidy version of 25*25 grid to which values of interest can be joined
gridmapg1_tidy <- gridmapg1 %>% 
  broom::tidy() %>% 
  # removing the "g" in order to later join by GRIDG1 column
  mutate(id2 = as.numeric(str_remove(id, "g")))

##### grid-level ####

temp1 <- data_hist %>% 
  bind_rows(data_cur) %>% 
  group_by(TIME.SOIB1) %>% 
  mutate(TOT.LISTS = n_distinct(GROUP.ID)) %>% 
  group_by(TIME.SOIB1, TOT.LISTS, GRIDG1) %>% 
  summarise(NO.LISTS = n_distinct(GROUP.ID)) %>% 
  group_by(TIME.SOIB1) %>% 
  mutate(MAX.LISTS = max(NO.LISTS),
         MED.LISTS = median(NO.LISTS)) %>% 
  group_by(TIME.SOIB1, TOT.LISTS, GRIDG1) %>% 
  summarise(NO.LISTS = NO.LISTS,
            STAN.LISTS = NO.LISTS/MAX.LISTS, # standardise by max lists/cell (cell with highest activity)
            CS.LISTS = (NO.LISTS - MED.LISTS)/MED.LISTS, # centre&standardise to median lists/cell
            PROP.LISTS = NO.LISTS/TOT.LISTS) %>%  # prop. of total Indian birding happening in this cell
  ungroup() %>% 
  rename(PERIOD = TIME.SOIB1)

temp2 <- data_hist %>% 
  group_by(TIME.SOIB2) %>% 
  mutate(TOT.LISTS = n_distinct(GROUP.ID)) %>% 
  group_by(TIME.SOIB2, TOT.LISTS, GRIDG1) %>% 
  summarise(NO.LISTS = n_distinct(GROUP.ID)) %>% 
  group_by(TIME.SOIB2) %>% 
  mutate(MAX.LISTS = max(NO.LISTS),
         MED.LISTS = median(NO.LISTS)) %>% 
  group_by(TIME.SOIB2, TOT.LISTS, GRIDG1) %>% 
  summarise(NO.LISTS = NO.LISTS,
            STAN.LISTS = NO.LISTS/MAX.LISTS,
            CS.LISTS = (NO.LISTS - MED.LISTS)/MED.LISTS, 
            PROP.LISTS = NO.LISTS/TOT.LISTS) %>% 
  ungroup() %>% 
  rename(PERIOD = TIME.SOIB2) %>% 
  # removing 2021 since temp1 already has
  filter(PERIOD != "2021")

grid_cov <- temp1 %>% 
  bind_rows(temp2) %>% 
  mutate(PERIOD = factor(PERIOD, levels = soib_levels)) %>% 
  group_by(PERIOD) %>% 
  # number of cells with at least 1 list
  mutate(CELLS.1 = n_distinct(GRIDG1)) %>% 
  # number of cells with at least 10 lists
  filter(NO.LISTS >= 10) %>% 
  mutate(CELLS.10 = n_distinct(GRIDG1)) %>% 
  # number of cells with at least 20 lists
  filter(NO.LISTS >= 20) %>% 
  mutate(CELLS.20 = n_distinct(GRIDG1)) %>% 
  # number of cells with at least 50 lists
  filter(NO.LISTS >= 50) %>% 
  mutate(CELLS.50 = n_distinct(GRIDG1)) %>% 
  distinct(CELLS.1, CELLS.10, CELLS.20, CELLS.50) %>% 
  arrange(PERIOD) %>% 
  ungroup()

# all grids across the different time periods under consideration (not all grids in India)
# this is enough to show difference/change; other grids will be blank
all_grids <- temp1 %>% 
  bind_rows(temp2) %>% 
  distinct(GRIDG1)
  
data0 <- temp1 %>% 
  bind_rows(temp2) %>% 
  mutate(PERIOD = factor(PERIOD, levels = soib_levels)) %>% 
  group_by(PERIOD) %>% 
  complete(GRIDG1 = all_grids$GRIDG1, 
           fill = list(NO.LISTS = 0, TOT.LISTS = 0, STAN.LISTS = 0, CS.LISTS = 0, PROP.LISTS = 0)) %>% 
  left_join(grid_cov) %>% 
  ungroup() 

data0grid <- gridmapg1_tidy %>% 
  left_join(data0, by = c("id2" = "GRIDG1")) %>% 
  # removing NA PERIOD formed from join
  filter(!is.na(PERIOD)) %>% 
  # converting metrics to NA when number of lists = 0
  mutate(across(c(STAN.LISTS, CS.LISTS, PROP.LISTS),
         ~ if_else(NO.LISTS == 0, as.numeric(NA_integer_), .x))) %>% 
  dplyr::select(-c(TOT.LISTS, CELLS.1, CELLS.10, CELLS.20, CELLS.50))

data0_change_no <- data0 %>%
  pivot_wider(names_from = PERIOD, values_from = NO.LISTS) %>%
  group_by(GRIDG1) %>%
  summarise(CHANGE.A = `2021` - `pre-2000`,
            CHANGE.B = `2021` - `1990-1999`,
            CHANGE.C = `2021` - `pre-1990`)

data0_change_prop <- data0 %>%
  pivot_wider(names_from = PERIOD, values_from = PROP.LISTS) %>%
  group_by(GRIDG1) %>%
  summarise(CHANGE.A = `2021` - `pre-2000`,
            CHANGE.B = `2021` - `1990-1999`,
            CHANGE.C = `2021` - `pre-1990`)

data0_change_stan <- data0 %>%
  pivot_wider(names_from = PERIOD, values_from = STAN.LISTS) %>%
  group_by(GRIDG1) %>%
  summarise(CHANGE.A = `2021` - `pre-2000`,
            CHANGE.B = `2021` - `1990-1999`,
            CHANGE.C = `2021` - `pre-1990`)

##### full timeline (for data progression line graph) ####

temp1 <- timeline %>%
  filter(YEAR != 2022) %>% 
  group_by(TIME.SOIB1) %>% 
  summarise(NO.LISTS = n_distinct(GROUP.ID),
            CELLS.1 = n_distinct(GRIDG1),
            # for axis tick position
            MEDIAN.YEAR = median(YEAR)) %>% 
  arrange(TIME.SOIB1)

temp2 <- timeline %>%
  filter(YEAR != 2022) %>% 
  group_by(TIME.SOIB2) %>% 
  summarise(NO.LISTS = n_distinct(GROUP.ID),
            CELLS.1 = n_distinct(GRIDG1),
            # for axis tick position
            MEDIAN.YEAR = median(YEAR)) %>% 
  arrange(TIME.SOIB2) 

data_prog <- temp1 %>% 
  full_join(temp2) %>% 
  arrange(MEDIAN.YEAR) %>% 
  mutate(PERIOD = if_else(is.na(as.character(TIME.SOIB1)), 
                          as.character(TIME.SOIB2), 
                          as.character(TIME.SOIB1))) %>% 
  mutate(PERIOD = factor(PERIOD,
                         levels = if_else(is.na(as.character(TIME.SOIB1)), 
                                          as.character(TIME.SOIB2), 
                                          as.character(TIME.SOIB1))))

##### region-level ####

temp1 <- data_hist %>% 
  bind_rows(data_cur) %>% 
  left_join(regions) %>% 
  group_by(TIME.SOIB1) %>% 
  mutate(TOT.LISTS = n_distinct(GROUP.ID)) %>% 
  group_by(TIME.SOIB1, REGION) %>% 
  summarise(NO.LISTS = n_distinct(GROUP.ID),
            TOT.LISTS = min(TOT.LISTS),
            PROP.LISTS = 100*NO.LISTS/TOT.LISTS) %>% 
  ungroup() %>% 
  rename(PERIOD = TIME.SOIB1)

temp2 <- data_hist %>% 
  left_join(regions) %>% 
  group_by(TIME.SOIB2) %>% 
  mutate(TOT.LISTS = n_distinct(GROUP.ID)) %>% 
  group_by(TIME.SOIB2, REGION) %>% 
  summarise(NO.LISTS = n_distinct(GROUP.ID),
            TOT.LISTS = min(TOT.LISTS),
            PROP.LISTS = 100*NO.LISTS/TOT.LISTS) %>% 
  ungroup() %>% 
  rename(PERIOD = TIME.SOIB2) %>% 
  # removing 2021 since temp1 already has
  filter(PERIOD != "2021")

data_reg <- temp1 %>% 
  bind_rows(temp2) %>% 
  mutate(PERIOD = factor(PERIOD, levels = soib_levels)) %>% 
  # removing NA districts
  filter(!is.na(REGION))


### 1. Maps of spatial spread ####

### Number

map1_nolists <- ggplot(data = data0, aes(LONGITUDE, LATITUDE)) +
  facet_wrap(~ PERIOD, ncol = 2) +
  geom_polygon(data = indiamap,
               aes(long, lat, group = group),
               colour = "black", fill = NA, size = 0.2) +
  geom_polygon(data = data0grid, 
               aes(long, lat, group = group, fill = log(NO.LISTS))) +
  scale_fill_viridis_c(na.value = "#CCCCCC") +
  labs(title = "Birding intensity in grid cells across the country",
       subtitle = "Fill: log-transformed no. of lists per grid cell per time period (grey: zero lists).",
       fill = "log(no. of lists)") +
  theme(axis.line = element_blank(), 
        axis.title = element_blank(), 
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "bottom")

ggsave("hist_spread/figs/map1_nolists.png", map1_nolists,
       width = 10, height = 10, units = "in", dpi = 300)

### Total proportion

map2_proplists <- ggplot(data = data0, aes(LONGITUDE, LATITUDE)) +
  facet_wrap(~ PERIOD, ncol = 2) +
  geom_polygon(data = indiamap,
               aes(long, lat, group = group),
               colour = "black", fill = NA, size = 0.2) +
  geom_polygon(data = data0grid, 
               aes(long, lat, group = group, fill = PROP.LISTS*100)) +
  scale_fill_viridis_c(na.value = "#CCCCCC") +
  labs(title = "How was birding spatially concentrated in different periods?",
       subtitle = "Fill: proportion of total nationwide lists from each grid cell (grey: zero lists).",
       fill = "Prop. of lists (%)") +
  theme(axis.line = element_blank(), 
        axis.title = element_blank(), 
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "bottom")

ggsave("hist_spread/figs/map2_proplists.png", map2_proplists,
       width = 10, height = 10, units = "in", dpi = 300)

### Standardised proportion

map3_stanlists <- ggplot(data = data0, aes(LONGITUDE, LATITUDE)) +
  facet_wrap(~ PERIOD, ncol = 2) +
  geom_polygon(data = indiamap,
               aes(long, lat, group = group),
               colour = "black", fill = NA, size = 0.2) +
  geom_polygon(data = data0grid, 
               aes(long, lat, group = group, fill = STAN.LISTS*100)) +
  scale_fill_viridis_c(na.value = "#CCCCCC") +
  labs(title = "How was birding spatially concentrated in different periods?",
       subtitle = "Fill: no. of lists in proportion to highest lists/cell value (grey: zero lists).",
       fill = "Standardised prop. of lists (%)") +
  theme(axis.line = element_blank(), 
        axis.title = element_blank(), 
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "bottom")

ggsave("hist_spread/figs/map3_stanlists.png", map3_stanlists,
       width = 10, height = 10, units = "in", dpi = 300)


### 2. Histograms ####

ggplot(data = data0) +
  facet_wrap(~ PERIOD, ncol = 1) +
  scale_y_continuous(expand = c(0, 0)) +
  # the discrepancy
  annotate("rect", 
           xmin = 2.2, xmax = 2.9, ymin = 0, ymax = +Inf, 
           fill = "#ff0000", col = NA, alpha = 0.1) +
  geom_histogram(aes(log(NO.LISTS))) +
  labs(title = "Distribution of birding intensities",
       x = "log(no. of lists per grid cell)")

### 

ggplot(data = data0) +
  facet_wrap(~ PERIOD, ncol = 1) +
  scale_y_continuous(expand = c(0, 0)) +
  # the discrepancy
  annotate("rect", 
           xmin = 12000, xmax = 17000, ymin = 0, ymax = +Inf, 
           fill = "#ff0000", col = NA, alpha = 0.1) +
  geom_histogram(aes(GRIDG1), bins = 60) +
  labs(title = "Distribution of spatial spread",
       x = "Cell ID")

### 3. Data across geographical regions ####

ggplot(data2, 
       aes(reorder(REGION, -PROP.LISTS), PROP.LISTS)) +
  facet_wrap(~ PERIOD, ncol = 1) +
  geom_col() +
  labs(title = "Distribution of birding across geographical regions",
       x = "Region", y = "Proportion of lists")

### 4. Data progression with time periods ####

(ggplot(data3, 
        aes(x = MEDIAN.YEAR, y = NO.CELLS)) +
   geom_point(size = 2) +
   geom_line(size = 1) +
   geom_hline(yintercept = 1000, linetype = "dotted") +
   scale_x_continuous(breaks = unique(data3$MEDIAN.YEAR),
                      labels = unique(data3$PERIOD)) +
   scale_y_continuous() +
   labs(x = "", y = "Number of grid cells with lists") +
   (ggplot(data3, 
           aes(x = MEDIAN.YEAR, y = NO.LISTS/1000))) +
   geom_point(size = 2) +
   geom_line(size = 1) +
   geom_hline(yintercept = 2, linetype = "dotted") +
   scale_x_continuous(breaks = unique(data3$MEDIAN.YEAR),
                      labels = unique(data3$PERIOD)) +
   scale_y_continuous(labels = function(x) paste(x, "K"), 
                      n.breaks = 4, breaks = waiver()) +
   labs(x = "Time period", y = "Number of lists (in thousands)")) &
  theme(axis.text.x = element_text(size = 6, angle = 45, vjust = 0.5)) &
  plot_layout(ncol = 1) &
  plot_annotation(title = "How much data in each time period?")
