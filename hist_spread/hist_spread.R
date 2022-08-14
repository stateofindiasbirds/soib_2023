# Quick analysis to determine whether or not historical eBird data in the country is 
# spatially biased, at two levels: 
# - pre-2000 
# - pre-1990 & 1990-2000

# All these analyses are using the base dataset, without any of the SoIB filters, 
# but with removal of duplicate lists.

### Setup ####

library(tidyverse)
library(raster)
library(sf)
library(here) # helpful to make relative paths consistent between .R run and .Rmd knit
library(patchwork)

# ggplot theme
theme_set(theme_classic() +
            theme(strip.background = element_blank(),
                  strip.text = element_text(size = 12, face = "bold"),
                  plot.title = element_text(size = 16, face = "bold"),
                  panel.border = element_blank()))

### Data ####

### uncomment below if first time setting up. else, just load .RData (lines below commented)

# # filtering data for this analysis
# load("ebd_IN_relJun-2022.RData")
# 
# data_hist <- data %>%
#   filter(YEAR < 2000) %>%
#   # slicing to original checklist-level
#   group_by(GROUP.ID) %>%
#   slice(1) %>%
#   mutate(PERIOD1 = "pre-2000",
#          PERIOD2 = case_when(YEAR %in% 1990:1999 ~ "1990-1999",
#                              YEAR < 1990 ~ "pre-1990"))
# 
# data_cur <- data %>%
#   filter(YEAR == 2021) %>%
#   # slicing to original checklist-level
#   group_by(GROUP.ID) %>%
#   slice(1) %>%
#   mutate(PERIOD1 = "2021",
#          PERIOD2 = "2021")
# 
# # for line graph
# timeline <- data %>%
#   # slicing to original checklist-level
#   group_by(GROUP.ID) %>%
#   slice(1) %>%
#   mutate(PERIOD = case_when(YEAR < 1990 ~ "pre-1990",
#                             YEAR %in% 1990:1999 ~ "1990-1999",
#                             YEAR %in% 2000:2006 ~ "2000-2006",
#                             YEAR %in% 2007:2010 ~ "2007-2010",
#                             YEAR > 2010 ~ as.character(YEAR)))
# 
# # 24x24 raster info to get cell ID (from https://github.com/rikudoukarthik/covid-ebirding)
# load("hist_spread/rast_SoIB.RData")
# 
# # Ashwin's maps data (https://github.com/ashwinv2005/state-of-indias-birds)
# load("hist_spread/maps.RData")
# 
# # cropping grid to india boundary
# temp1 <- as(indiamap, "sf") %>% sf::st_buffer(dist = 0)
# # to calculate total number of grids of each size
# indiagrid <- sf::st_intersection(as(gridmapg1,"sf"), temp1)
# 
# 
# save(data_hist, data_cur, indiagrid, timeline, file = "hist_spread/hist_spread.RData")
# rm(data)


### if .RData already created (else, comment below and run above lines)

# 24x24 raster info to get cell ID (from rikudoukarthik/covid-ebirding)
load("hist_spread/rast_SoIB.RData")

# Ashwin's maps data
load("hist_spread/maps.RData")

load("hist_spread/hist_spread.RData")



# region data (from ashwinv2005/state-of-indias-birds)
regions <- read_csv("hist_spread/districtlist.csv") %>% 
  rename(REGION = region,
         STATE = ST_NM,
         COUNTY = DISTRICT)

### Preparing data for analyses ####

temp <- data_hist %>% 
  bind_rows(data_cur) %>% 
  mutate(CELL.ID = raster::cellFromXY(rast_SoIB, cbind(LONGITUDE, LATITUDE)),
         CELL.LONG = raster::xyFromCell(rast_SoIB, CELL.ID)[,"x"],
         CELL.LAT = raster::xyFromCell(rast_SoIB, CELL.ID)[,"y"]) %>%
  group_by(PERIOD1, CELL.ID, CELL.LONG, CELL.LAT) %>% 
  summarise(NO.LISTS = n_distinct(GROUP.ID)) %>% 
  group_by(PERIOD1) %>% 
  mutate(MAX.LISTS = max(NO.LISTS),
         MED.LISTS = median(NO.LISTS)) %>% 
  group_by(PERIOD1, CELL.ID, CELL.LONG, CELL.LAT) %>% 
  # standardising listspercell by value in cell with highest lists
  summarise(NO.LISTS = NO.LISTS,
            STAN.LISTS = NO.LISTS/MAX.LISTS,
            CS.LISTS = (NO.LISTS - MED.LISTS)/MED.LISTS)

data0 <- data_hist %>% 
  mutate(CELL.ID = raster::cellFromXY(rast_SoIB, cbind(LONGITUDE, LATITUDE)),
         CELL.LONG = raster::xyFromCell(rast_SoIB, CELL.ID)[,"x"],
         CELL.LAT = raster::xyFromCell(rast_SoIB, CELL.ID)[,"y"]) %>%
  group_by(PERIOD2, CELL.ID, CELL.LONG, CELL.LAT) %>% 
  summarise(NO.LISTS = n_distinct(GROUP.ID)) %>% 
  group_by(PERIOD2) %>% 
  mutate(MAX.LISTS = max(NO.LISTS),
         MED.LISTS = median(NO.LISTS)) %>% 
  group_by(PERIOD2, CELL.ID, CELL.LONG, CELL.LAT) %>% 
  # standardising listspercell by value in cell with highest lists
  summarise(NO.LISTS = NO.LISTS,
            STAN.LISTS = NO.LISTS/MAX.LISTS,
            CS.LISTS = (NO.LISTS - MED.LISTS)/MED.LISTS) %>% 
  ungroup() %>% 
  rename(PERIOD = PERIOD2) %>% 
  bind_rows(temp %>% rename(PERIOD = PERIOD1)) %>% 
  mutate(PERIOD = factor(PERIOD, levels = c("2021", "pre-2000", "1990-1999", "pre-1990")))

# data1 <- data0 %>%
#   group_by(PERIOD) %>%
#   tidyr::complete(CELL.ID = 1:raster::ncell(rast_SoIB),
#                   fill = list(NO.LISTS = 0, STAN.LISTS = 0)) %>%
#   mutate(CELL.LONG = raster::xyFromCell(rast_SoIB, CELL.ID)[,"x"],
#          CELL.LAT = raster::xyFromCell(rast_SoIB, CELL.ID)[,"y"]) %>%
#   ungroup() %>%
#   pivot_wider(names_from = PERIOD1, values_from = STAN.LISTS) %>%
#   group_by(CELL.ID, CELL.LONG, CELL.LAT) %>%
#   summarise(CHANGE = `pre-2000` - `2021`)

temp <- data_hist %>% 
  bind_rows(data_cur) %>% 
  left_join(regions) %>% 
  group_by(PERIOD1) %>% 
  mutate(TOT.LISTS = n_distinct(GROUP.ID)) %>% 
  group_by(PERIOD1, REGION) %>% 
  summarise(NO.LISTS = n_distinct(GROUP.ID),
            TOT.LISTS = min(TOT.LISTS),
            PROP.LISTS = 100*NO.LISTS/TOT.LISTS)

data2 <- data_hist %>% 
  left_join(regions) %>% 
  group_by(PERIOD2) %>% 
  mutate(TOT.LISTS = n_distinct(GROUP.ID)) %>% 
  group_by(PERIOD2, REGION) %>% 
  summarise(NO.LISTS = n_distinct(GROUP.ID),
            TOT.LISTS = min(TOT.LISTS),
            PROP.LISTS = 100*NO.LISTS/TOT.LISTS) %>% 
  ungroup() %>% 
  rename(PERIOD = PERIOD2) %>% 
  bind_rows(temp %>% rename(PERIOD = PERIOD1)) %>% 
  mutate(PERIOD = factor(PERIOD, levels = c("2021", "pre-2000", "1990-1999", "pre-1990"))) %>% 
  # removing NA districts
  filter(!is.na(REGION))

# 
# x <- data_hist %>% 
#   bind_rows(data_cur) %>% 
#   ungroup() %>% 
#   left_join(regions) %>% 
#   filter(is.na(REGION)) %>% 
#   distinct(STATE, COUNTY, REGION)

data3 <- timeline %>%
  filter(YEAR != 2022) %>% 
  mutate(CELL.ID = raster::cellFromXY(rast_SoIB, cbind(LONGITUDE, LATITUDE))) %>% 
  group_by(PERIOD) %>% 
  summarise(NO.LISTS = n_distinct(GROUP.ID),
            NO.CELLS = n_distinct(CELL.ID),
            # for axis tick position
            MEDIAN.YEAR = median(YEAR)) %>% 
  mutate(PERIOD = factor(PERIOD, 
                         levels = c("pre-1990", "1990-1999", "2000-2006", "2007-2010",
                                    as.character(2011:2021))))
### 1. Maps of spatial spread ####

### Number

ggplot(data = data0, aes(CELL.LONG, CELL.LAT)) +
  facet_wrap(~ PERIOD, ncol = 2) +
  geom_polygon(data = indiamap, aes(long, lat, group = group), 
               fill = "#F0F0F0", colour = "black", size = 0.2) +
  geom_tile(aes(fill = log(NO.LISTS))) +
  scale_fill_viridis_c() +
  labs(title = "Birding intensity across grid cells",
       fill = "log(no. of lists)") +
  theme(axis.line = element_blank(), 
        axis.title = element_blank(), 
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "bottom")

### Standardised proportion

ggplot(data = data0, aes(CELL.LONG, CELL.LAT)) +
  facet_wrap(~ PERIOD, ncol = 2) +
  geom_polygon(data = indiamap, aes(long, lat, group = group), 
               fill = "#F0F0F0", colour = "black", size = 0.2) +
  geom_tile(aes(fill = STAN.LISTS)) +
  scale_fill_viridis_c() + 
  labs(title = "How was birding spatially concentrated in different periods?",
       fill = "Stand. prop. of lists") +
  theme(axis.line = element_blank(), 
        axis.title = element_blank(), 
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "bottom")

# x <- data0 %>% 
#   ungroup() %>% 
#   dplyr::select(CS.LISTS) %>% 
#   arrange(CS.LISTS) %>% 
#   rownames_to_column("INDEX")
# 
# legend <- c(seq(min(data0$CS.LISTS), median(data0$CS.LISTS), 
#                 length.out = (max(data0$CS.LISTS) - min(data0$CS.LISTS))/2),
#             seq(median(data0$CS.LISTS), quantile(data0$CS.LISTS, 0.75), 
#                 length.out = (max(data0$CS.LISTS) - min(data0$CS.LISTS))/4),
#             seq(quantile(data0$CS.LISTS, 0.75), max(data0$CS.LISTS),
#                 length.out = (max(data0$CS.LISTS) - min(data0$CS.LISTS))/4))
#   
# ggplot(data = data0, aes(CELL.LONG, CELL.LAT)) +
#   facet_wrap(~ PERIOD, ncol = 2) +
#   geom_polygon(data = indiamap, aes(long, lat, group = group), 
#                fill = "#F0F0F0", colour = "black", size = 0.2) +
#   geom_tile(aes(fill = CS.LISTS)) +
#   scale_fill_viridis_c(values = summary(legend)) +
#   scale_y_continuous(name = "Cent.+stand. number of lists)") +
#   labs(title = "Where was birding concentrated in different time periods?") +
#   theme(axis.line = element_blank(), 
#         axis.title = element_blank(), 
#         axis.text = element_blank(),
#         axis.ticks = element_blank(),
#         legend.position = "bottom")

### 2. Histograms ####

### 

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
  geom_histogram(aes(CELL.ID), bins = 60) +
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
