# Quick analysis to determine whether or not historical eBird data in the country is 
# spatially biased.

# All these analyses are using the base dataset of complete checklists, without any of the SoIB filters, 
# but with removal of duplicate lists.


### Setup ####

library(tidyverse)
library(raster)
library(sf)
library(patchwork)
library(glue)

# ggplot theme
theme_set(theme_classic() +
            theme(strip.background = element_blank(),
                  strip.text = element_text(size = 12, face = "bold"),
                  plot.title = element_text(size = 16, face = "bold"),
                  panel.border = element_blank()))

# functions for historical spread analyses
source("hist_spread/hist_spread_functions.R")

# comparisons for SoIB
soib_levels <- c("2021", "pre-2000", "1990-1999", "pre-1990")


# importing data
import_hist_spread_data(init_setup = F)


# tidy version of 25*25 grid to which values of interest can be joined (for spatial plotting)
gridmapg1_sf <- gridmapg1 %>% 
  st_as_sf() %>% 
  rename(GRIDG1 = id)


### 1. Grid-level ####

# grid cells coverage at different thresholds (for "coverage")
data_grid_cells_cov <- data0 %>% 
  group_by(PERIOD, GRIDG1) %>% 
  mutate(NO.LISTS = n_distinct(GROUP.ID)) %>% 
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

# list of all grids across the different time periods under consideration 
# (not all grids in India)
# this is enough to show difference/change; other grids will be blank
# for grids histogram, converting true CELL.ID to continuous values 
# (i.e., removing the gaps in ID of grid cells falling outside country boundary) so that
# histogram can be more intuitive because x axis will be serial
data_grid_cells_list <- data0 %>% 
  distinct(GRIDG1) %>% 
  rownames_to_column("GRIDG1.SERIAL") %>% 
  mutate(GRIDG1.SERIAL = as.integer(GRIDG1.SERIAL))


# getting lists metrics (number, proportion, standardised proportion) for 
# 25x25 cells across time periods
data_grid <- data0 %>% 
  group_by(PERIOD) %>% 
  mutate(TOT.LISTS = n_distinct(GROUP.ID)) %>% 
  group_by(PERIOD, TOT.LISTS, GRIDG1) %>% 
  summarise(NO.LISTS = n_distinct(GROUP.ID)) %>% 
  group_by(PERIOD) %>% 
  mutate(MAX.LISTS = max(NO.LISTS),
         MED.LISTS = median(NO.LISTS)) %>% 
  group_by(PERIOD, TOT.LISTS, GRIDG1) %>% 
  summarise(NO.LISTS = NO.LISTS,
            STAN.LISTS = NO.LISTS/MAX.LISTS, # standardise by max lists/cell (cell with highest activity)
            CS.LISTS = (NO.LISTS - MED.LISTS)/MED.LISTS, # centre&standardise to median lists/cell
            PROP.LISTS = NO.LISTS/TOT.LISTS) %>%  # prop. of total Indian birding happening in this cell
  group_by(PERIOD) %>% 
  # filling zeroes for cell-period combo with no lists
  complete(GRIDG1 = data_grid_cells_list$GRIDG1, 
           fill = list(NO.LISTS = 0, TOT.LISTS = 0, STAN.LISTS = 0, CS.LISTS = 0, PROP.LISTS = 0)) %>% 
  # joining serial cell IDs
  left_join(data_grid_cells_list) %>% 
  # joining grid cell coverage info
  left_join(data_grid_cells_cov) %>% 
  ungroup() %>% 
  right_join(gridmapg1_sf) %>% 
  # removing NA PERIOD formed from join
  filter(!is.na(PERIOD)) %>% 
  # converting metrics to NA when number of lists = 0 (to visualise non-overlap)
  mutate(across(c(STAN.LISTS, CS.LISTS, PROP.LISTS),
                ~ if_else(NO.LISTS == 0, as.numeric(NA_integer_), .x))) %>% 
  dplyr::select(-c(TOT.LISTS, CELLS.1, CELLS.10, CELLS.20, CELLS.50)) %>% 
  arrange(GRIDG1, NO.LISTS)

### 1a. Maps of spatial spread ####

### Number

grid_map_no <- gg_map(data = data_grid, sf = T, 
                      facetvar = PERIOD, ncol = 2,
                      poly1 = indiamap,
                      mainvar = log(NO.LISTS), 
                      gg_viridis_option = "rocket",
                      title = "Birding intensity in grid cells across the country",
                      subtitle = "Fill: log-transformed no. of lists per grid cell per time period (grey: zero lists).",
                      legend_title = "log(no. of lists)")

ggsave("hist_spread/figs/grid_map_no.png", grid_map_no,
       width = 10, height = 12, units = "in", dpi = 300)

### Total proportion

grid_map_prop <- gg_map(data = data_grid, sf = T, 
                        facetvar = PERIOD, ncol = 2,
                        poly1 = indiamap,
                        mainvar = PROP.LISTS*100,
                        scale_trans = "log10", scale_trans_breaks = c(0.01, 0.1, 0.5, 2, 5, 10),
                        gg_viridis_option = "rocket",
                        title = "How was birding spatially concentrated in different periods?",
                        subtitle = "Fill: percentage of total nationwide lists from each grid cell (grey: zero lists).",
                        legend_title = "Percentage of lists (%)")

ggsave("hist_spread/figs/grid_map_prop.png", grid_map_prop,
       width = 10, height = 12, units = "in", dpi = 300)

### Standardised proportion

grid_map_stan <- gg_map(data = data_grid, sf = T, 
                        facetvar = PERIOD, ncol = 2,
                        poly1 = indiamap,
                        mainvar = STAN.LISTS*100,
                        scale_trans = "log10", scale_trans_breaks = c(0.1, 1, 5, 20, 50, 100),
                        gg_viridis_option = "rocket",
                        title = "How was birding spatially concentrated in different periods?",
                        subtitle = "Fill: no. of lists in proportion to highest lists/cell value (grey: zero lists).",
                        legend_title = "Standardised prop. of lists (%)")

ggsave("hist_spread/figs/grid_map_stan.png", grid_map_stan,
       width = 10, height = 12, units = "in", dpi = 300)


### 1b. Histograms ####

grid_hist_no <- ggplot(filter(data_grid, NO.LISTS != 0)) + # need to filter here but not in regional ones
  facet_wrap(~ PERIOD, ncol = 1) +
  scale_y_continuous(expand = c(0, 0)) +
  # the discrepancy
  annotate("rect", 
           xmin = 1.7, xmax = 2.7, ymin = 0, ymax = +Inf, 
           fill = "#ff0000", col = NA, alpha = 0.1) +
  geom_histogram(aes(log(NO.LISTS))) +
  labs(x = "log(no. of lists per grid cell)")

ggsave("hist_spread/figs/grid_hist_no.png", grid_hist_no,
       width = 7, height = 12, units = "in", dpi = 300)

### 2. Region-level ####

# getting info of all grid cells per region
# (produces NAs because lots of cells outside)
eco_grids <- gridmapg1_sf %>% 
  st_set_crs(st_crs(ecoregions)) %>% 
  st_join(ecoregions) %>% 
  group_by(PJ_RECLASS) %>% 
  summarise(TOT.CELLS = n_distinct(GRIDG1)) %>% 
  filter(!is.na(PJ_RECLASS)) %>% 
  st_drop_geometry() # we only need this to later join using PJ_RECLASS


data_reg <- data0 %>% 
  # to add region info
  st_as_sf(coords = c("LONGITUDE", "LATITUDE")) %>% 
  st_set_crs(st_crs(ecoregions)) %>% 
  st_join(ecoregions) %>%
  # this joins attributes but not geom of ecoregion (retains geom of lists)
  # so, removing lists geom and 
  st_drop_geometry() %>% 
  group_by(PERIOD) %>% 
  mutate(TOT.LISTS = n_distinct(GROUP.ID)) %>% 
  left_join(eco_grids) %>% 
  group_by(PERIOD, PJ_RECLASS) %>% 
  summarise(NO.LISTS = n_distinct(GROUP.ID),
            TOT.LISTS = min(TOT.LISTS),
            # out of total lists in country in time period, how many in this region?
            PROP.LISTS = 100*NO.LISTS/TOT.LISTS,
            NO.CELLS = n_distinct(GRIDG1),
            TOT.CELLS = min(TOT.CELLS),
            # out of total cells in region, how many covered?
            PROP.CELL.COV = 100*NO.CELLS/TOT.CELLS) %>% 
  # filling zeroes for cell-period combo with no lists
  group_by(PERIOD) %>% 
  complete(PJ_RECLASS = eco_grids$PJ_RECLASS, 
           fill = list(NO.LISTS = 0, PROP.LISTS = 0,
                       NO.CELLS = 0, PROP.CELLS.COV = 0)) %>% 
  # joining ecoregions object which contains their geom
  left_join(ecoregions) %>% 
  # removing NA regions in ecoregions name but where data exists (i.e., marine regions)
  filter(!is.na(PJ_RECLASS)) %>% 
  # converting metrics to NA when number of lists = 0 (to visualise non-overlap)
  mutate(PROP.LISTS = if_else(NO.LISTS == 0, as.numeric(NA_integer_), PROP.LISTS),
         PROP.CELL.COV = if_else(NO.CELLS == 0, as.numeric(NA_integer_), PROP.CELL.COV))

# for change in representation of regions

data_reg_change1 <- data_reg %>%
  st_drop_geometry() %>% 
  dplyr::select(PJ_RECLASS, PERIOD, NO.LISTS) %>% 
  pivot_wider(names_from = PERIOD, values_from = NO.LISTS) %>%
  # filling zeros for region-period combinations
  mutate(across(2:5, ~ replace_na(.x, 0))) %>% 
  # adding 1 to all to allow calculation of proportional change
  mutate(across(2:5, ~ .x + 1)) %>% 
  group_by(PJ_RECLASS) %>%
  # proportional change (not percent)
  summarise(CHANGE1 = `2021` / `pre-2000`,
            CHANGE2 = `2021` / `1990-1999`,
            CHANGE3 = `2021` / `pre-1990`) %>% 
  pivot_longer(cols = contains("CHANGE"), 
               names_to = "COMPARISON", names_transform = ~ str_remove(.x, "CHANGE"),
               values_to = "CHANGE") %>% 
  mutate(COMPARISON.LAB = case_when(COMPARISON == 1 ~ "pre-2000 to 2021",
                                    COMPARISON == 2 ~ "1990-1999 to 2021",
                                    COMPARISON == 3 ~ "pre-1990 to 2021")) %>% 
  mutate(COMPARISON.LAB = factor(COMPARISON.LAB, 
                                 levels = c("pre-1990 to 2021", 
                                            "1990-1999 to 2021", 
                                            "pre-2000 to 2021"))) %>% 
  left_join(ecoregions)

data_reg_change2 <- data_reg %>%
  st_drop_geometry() %>% 
  dplyr::select(PJ_RECLASS, PERIOD, PROP.LISTS) %>% 
  pivot_wider(names_from = PERIOD, values_from = PROP.LISTS) %>%
  # filling zeros for region-period combinations
  mutate(across(2:5, ~ replace_na(.x, 0))) %>% 
  # adding 10^(-6) to all to allow calculation of proportional change
  mutate(across(2:5, ~ .x + 10^(-6))) %>% 
  group_by(PJ_RECLASS) %>%
  # proportional change (not percent)
  summarise(CHANGE1 = `2021` / `pre-2000`,
            CHANGE2 = `2021` / `1990-1999`,
            CHANGE3 = `2021` / `pre-1990`) %>% 
  pivot_longer(cols = contains("CHANGE"), 
               names_to = "COMPARISON", names_transform = ~ str_remove(.x, "CHANGE"),
               values_to = "CHANGE") %>% 
  mutate(COMPARISON.LAB = case_when(COMPARISON == 1 ~ "pre-2000 to 2021",
                                    COMPARISON == 2 ~ "1990-1999 to 2021",
                                    COMPARISON == 3 ~ "pre-1990 to 2021")) %>% 
  mutate(COMPARISON.LAB = factor(COMPARISON.LAB, 
                                 levels = c("pre-1990 to 2021", 
                                            "1990-1999 to 2021", 
                                            "pre-2000 to 2021"))) %>% 
  left_join(ecoregions)

data_reg_change3 <- data_reg %>%
  st_drop_geometry() %>% 
  dplyr::select(PJ_RECLASS, PERIOD, PROP.CELL.COV) %>% 
  pivot_wider(names_from = PERIOD, values_from = PROP.CELL.COV) %>%
  # filling zeros for region-period combinations
  mutate(across(2:5, ~ replace_na(.x, 0))) %>% 
  # adding 10^(-6) to all to allow calculation of proportional change
  mutate(across(2:5, ~ .x + 10^(-6))) %>% 
  group_by(PJ_RECLASS) %>%
  # proportional change (not percent)
  summarise(CHANGE1 = `2021` / `pre-2000`,
            CHANGE2 = `2021` / `1990-1999`,
            CHANGE3 = `2021` / `pre-1990`) %>% 
  pivot_longer(cols = contains("CHANGE"), 
               names_to = "COMPARISON", names_transform = ~ str_remove(.x, "CHANGE"),
               values_to = "CHANGE") %>% 
  mutate(COMPARISON.LAB = case_when(COMPARISON == 1 ~ "pre-2000 to 2021",
                                    COMPARISON == 2 ~ "1990-1999 to 2021",
                                    COMPARISON == 3 ~ "pre-1990 to 2021")) %>% 
  mutate(COMPARISON.LAB = factor(COMPARISON.LAB, 
                                 levels = c("pre-1990 to 2021", 
                                            "1990-1999 to 2021", 
                                            "pre-2000 to 2021"))) %>% 
  left_join(ecoregions)

### 2.1. Absolute numbers: tables ####

data_reg %>% 
  st_drop_geometry() %>% 
  dplyr::select(PERIOD, PJ_RECLASS, NO.LISTS)

### 2.2.1. Proportions of birding across regions: columns ####

reg_prop_col <- ggplot(data_reg) +
  geom_col(aes(PJ_RECLASS, PROP.LISTS)) +
  facet_wrap(~ PERIOD, ncol = 1) +
  labs(x = "Ecoregion (reclassified)", y = "Percentage of birding occurring in region (%)",
       title = "Proportional birding across ecoregions over time") +
  theme(axis.text = element_text(angle = 90, size = 6))

ggsave("hist_spread/figs/reg_prop_col.png", reg_prop_col, 
       width = 9, height = 15, units = "in", dpi = 300)

### 2.2.2. Proportions of birding across regions: maps ####

# out of total lists in country in time period, how many in this region?
reg_prop_map <- gg_map(data = data_reg, sf = T, 
                       facetvar = PERIOD, ncol = 2,
                       poly1 = indiamap, poly2type = "region",
                       mainvar = PROP.LISTS, 
                       title = "Proportional birding across ecoregions over time",
                       subtitle = "Fill: percentage contribution of ecoregions to total lists (grey: zero lists).",
                       legend_title = "Percentage (%)")

ggsave("hist_spread/figs/reg_prop_map.png", reg_prop_map,
       width = 10, height = 12, units = "in", dpi = 300)

### 2.3.1. Grid coverage within regions: columns ####

reg_gridcov_col <- ggplot(data_reg) +
  geom_col(aes(PJ_RECLASS, PROP.CELL.COV)) +
  facet_wrap(~ PERIOD, ncol = 1) +
  labs(x = "Ecoregion (reclassified)", y = "Percentage grid coverage within region (%)",
       title = "Birding coverage of grid cells within ecoregions over time") +
  theme(axis.text = element_text(angle = 90, size = 6))

ggsave("hist_spread/figs/reg_gridcov_col.png", reg_gridcov_col, 
       width = 9, height = 15, units = "in", dpi = 300)

### 2.3.2. Grid coverage within regions: maps ####

reg_gridcov_map <- gg_map(data = data_reg, sf = T, 
                          facetvar = PERIOD, ncol = 2,
                          poly1 = indiamap, poly2type = "region",
                          mainvar = PROP.CELL.COV, 
                          title = "Birding coverage of grid cells within ecoregions over time",
                          subtitle = "Fill: percentage grid coverage within region (grey: zero lists).",
                          legend_title = "Percentage (%)")

ggsave("hist_spread/figs/reg_gridcov_map.png", reg_gridcov_map,
       width = 10, height = 12, units = "in", dpi = 300)


### 3. Data progression with time periods ####

temp1 <- timeline %>%
  filter(M.YEAR != 2022) %>% 
  group_by(TIME.SOIB1) %>% 
  mutate(TOT.LISTS = n_distinct(GROUP.ID),
         # for axis tick position
         MEDIAN.M.YEAR = median(M.YEAR)) %>% 
  group_by(TIME.SOIB1, TOT.LISTS, MEDIAN.M.YEAR, GRIDG1) %>% 
  summarise(NO.LISTS = n_distinct(GROUP.ID),
            CELLS.1 = if_else(NO.LISTS >= 1, 1, 0),
            CELLS.10 = if_else(NO.LISTS >= 10, 1, 0),
            CELLS.20 = if_else(NO.LISTS >= 20, 1, 0),
            CELLS.50 = if_else(NO.LISTS >= 50, 1, 0),
            CELLS.100 = if_else(NO.LISTS >= 100, 1, 0)) %>% 
  summarise(CELLS.1 = sum(CELLS.1), # number of cells with at least 1 list
            CELLS.10 = sum(CELLS.10), # number of cells with at least 10 lists
            CELLS.20 = sum(CELLS.20), # number of cells with at least 20 lists
            CELLS.50 = sum(CELLS.50), # number of cells with at least 50 lists
            CELLS.100 = sum(CELLS.100)) %>% # number of cells with at least 100 lists
  arrange(TIME.SOIB1) %>% 
  ungroup() %>% 
  mutate(RESOLUTION = 1)

temp2 <- timeline %>%
  filter(M.YEAR != 2022) %>% 
  group_by(TIME.SOIB2) %>% 
  mutate(TOT.LISTS = n_distinct(GROUP.ID),
         # for axis tick position
         MEDIAN.M.YEAR = median(M.YEAR)) %>% 
  group_by(TIME.SOIB2, TOT.LISTS, MEDIAN.M.YEAR, GRIDG1) %>% 
  summarise(NO.LISTS = n_distinct(GROUP.ID),
            CELLS.1 = if_else(NO.LISTS >= 1, 1, 0),
            CELLS.10 = if_else(NO.LISTS >= 10, 1, 0),
            CELLS.20 = if_else(NO.LISTS >= 20, 1, 0),
            CELLS.50 = if_else(NO.LISTS >= 50, 1, 0),
            CELLS.100 = if_else(NO.LISTS >= 100, 1, 0)) %>% 
  summarise(CELLS.1 = sum(CELLS.1), # number of cells with at least 1 list
            CELLS.10 = sum(CELLS.10), # number of cells with at least 10 lists
            CELLS.20 = sum(CELLS.20), # number of cells with at least 20 lists
            CELLS.50 = sum(CELLS.50), # number of cells with at least 50 lists
            CELLS.100 = sum(CELLS.100)) %>% # number of cells with at least 100 lists
  arrange(TIME.SOIB2) %>% 
  ungroup() %>% 
  mutate(RESOLUTION = 2)

grid_cov_timeline <- temp1 %>% 
  full_join(temp2) %>% 
  arrange(MEDIAN.M.YEAR) %>% 
  mutate(PERIOD = if_else(is.na(as.character(TIME.SOIB1)), 
                          as.character(TIME.SOIB2), 
                          as.character(TIME.SOIB1))) 
# mutate(PERIOD = factor(PERIOD,
#                        levels = if_else(is.na(as.character(TIME.SOIB1)), 
#                                         as.character(TIME.SOIB2), 
#                                         as.character(TIME.SOIB1))))

temp1 <- grid_cov_timeline %>% filter(RESOLUTION == 1)
temp2 <- grid_cov_timeline %>% filter(RESOLUTION == 2)

dataprog_grids <- ((ggplot(temp1, aes(x = MEDIAN.M.YEAR)) +
    annotate("rect", 
             xmin = 1981.5, xmax = 1995.5, ymin = 0, ymax = +Inf, 
             fill = "#EBEAEC", col = NA, alpha = 0.75) +
    annotate("rect", 
             xmin = 2010.5, xmax = 2013.5, ymin = 0, ymax = +Inf, 
             fill = "#EBEAEC", col = NA, alpha = 0.75) +
    geom_line(aes(y = CELLS.1, col = "1"), size = 1) +
    geom_line(aes(y = CELLS.10, col = "10"), size = 1) +
    geom_line(aes(y = CELLS.20, col = "20"), size = 1) +
    geom_point(aes(y = CELLS.1), size = 2) +
    geom_point(aes(y = CELLS.10), size = 2) +
    geom_point(aes(y = CELLS.20), size = 2) +
    geom_hline(yintercept = 1000, linetype = "dotted") +
    scale_x_continuous(breaks = temp1$MEDIAN.M.YEAR,
                       labels = temp1$PERIOD, 
                       limits = c(1980, 2021)) +
    scale_y_continuous(breaks = seq(0, 5000, 500), 
                       expand = c(0, 0),
                       limits = c(0, 3500)) +
    labs(x = "", y = "Number of grid cells with threshold lists",
         title = "At finer temporal resolution",
         col = c("Threshold lists per cell"))) +
  (ggplot(temp2, aes(x = MEDIAN.M.YEAR)) +
     annotate("rect", 
              xmin = 1981.5, xmax = 1995.5, ymin = 0, ymax = +Inf, 
              fill = "#EBEAEC", col = NA, alpha = 0.75) +
     annotate("rect", 
              xmin = 2010.5, xmax = 2013.5, ymin = 0, ymax = +Inf, 
              fill = "#EBEAEC", col = NA, alpha = 0.75) +
     geom_line(aes(y = CELLS.1, col = "1"), size = 1) +
     geom_line(aes(y = CELLS.10, col = "10"), size = 1) +
     geom_line(aes(y = CELLS.20, col = "20"), size = 1) +
     geom_point(aes(y = CELLS.1), size = 2) +
     geom_point(aes(y = CELLS.10), size = 2) +
     geom_point(aes(y = CELLS.20), size = 2) +
     geom_hline(yintercept = 1000, linetype = "dotted") +
     scale_x_continuous(breaks = temp2$MEDIAN.M.YEAR,
                        labels = temp2$PERIOD, 
                        limits = c(1980, 2021)) +
     scale_y_continuous(breaks = seq(0, 5000, 500), 
                        expand = c(0, 0),
                        limits = c(0, 3500)) +
     labs(x = "", y = "Number of grid cells with threshold lists",
          title = "At coarser temporal resolution",
          col = c("Threshold lists per cell")))) &
  theme(axis.text.x = element_text(size = 6, angle = 45, vjust = 0.5)) &
  plot_layout(ncol = 1) &
  plot_annotation(title = "How much data in each time period?")

ggsave("hist_spread/figs/dataprog_grids.png", dataprog_grids,
       width = 11, height = 10, units = "in", dpi = 300)


axislabels <- grid_cov_timeline %>% 
  dplyr::select(MEDIAN.M.YEAR, PERIOD) %>% 
  mutate(LAG.M.YEAR = lag(MEDIAN.M.YEAR),
         LEAD.M.YEAR = lead(MEDIAN.M.YEAR)) %>% 
  mutate(LABELS = case_when((PERIOD != lag(PERIOD) & MEDIAN.M.YEAR == LAG.M.YEAR) ~ 
                              glue("{PERIOD}\n{lag(PERIOD)}"),
                            (PERIOD != lead(PERIOD) & MEDIAN.M.YEAR == LEAD.M.YEAR) ~ 
                              glue("{PERIOD}\n{lead(PERIOD)}"),
                            TRUE ~ PERIOD)) %>% 
  group_by(MEDIAN.M.YEAR) %>% 
  slice(1) %>% 
  ungroup()

dataprog_lists <- ggplot(grid_cov_timeline, 
                         aes(x = MEDIAN.M.YEAR, y = log(TOT.LISTS/1000), col = as.factor(RESOLUTION))) +
  geom_point(size = 2) +
  geom_line(size = 1) +
  scale_colour_manual(values = c("goldenrod", "dark green"), 
                      name = "Temporal resolution",
                      labels = c("Fine", "Coarse")) +
  geom_hline(yintercept = log(2), linetype = "dotted") +
  annotate("text", x = 1981, y = (log(2)+0.1), label = "2K lists") +
  scale_x_continuous(breaks = axislabels$MEDIAN.M.YEAR,
                     labels = axislabels$LABELS,
                     limits = c(1980, 2021)) +
  scale_y_continuous(breaks = log(seq(0, 500, 50)),
                     labels = paste(seq(0, 500, 50), "K")) +
  labs(x = "Time period", y = "Number of lists (in thousands)",
       title = "How much data in each time period?",
       subtitle = " ") +
  theme(axis.text.x = element_text(size = 5, angle = 45, vjust = 0.5))

ggsave("hist_spread/figs/dataprog_lists.png", dataprog_lists,
       width = 11, height = 6, units = "in", dpi = 300)

