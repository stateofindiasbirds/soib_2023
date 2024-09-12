# map showing grids with and without lists

library(tidyverse)
library(sf)
library(spdep)
library(glue)

load("00_data/dataforanalyses_extra.RData")
load("00_data/maps_sf.RData")
sf_use_s2(FALSE) # not using spherical geometries

source("00_scripts/00_plot_functions.R")


palette_plot_elem <- "#56697B"
palette_plot_title <- "#A13E2B"
plot_fontfamily <- "Gandhi Sans"

grids_lists <- data0 %>% 
  group_by(gridg1) %>% 
  reframe(LISTS = 1)

g1_in_sf <- g1_in_sf %>% 
  left_join(grids_lists, by = c("GRID.G1" = "gridg1")) %>% 
  replace_na(list(LISTS = 0)) %>% 
  mutate(LISTS = as.factor(LISTS))


grids_lists_map <- ggplot() +
  geom_sf(data = g1_in_sf, aes(fill = LISTS), col = NA, alpha = 0.6) +
  scale_fill_manual(values = c("grey80", "grey30"), na.value = "transparent",
                    labels = c("Absent", "Present"), name = "Checklists") +
  geom_sf(data = states_sf, col = "white", fill = NA, alpha = 0.6) +
  ggtheme_soibrangemap() +
  theme(legend.title = element_text(colour = palette_plot_elem, size = 14))

ggsave(filename = "02_graphs/00_methods/india_grids_lists_rangemap.png", plot = grids_lists_map,
       dpi = 1000, bg = "white",
       width = 7, height = 7, units = "in")


grids_lists <- g1_in_sf %>% distinct(GRID.G1, LISTS)

write.csv(grids_lists, "02_graphs/00_methods/india_grids_lists_rangemap.csv", 
          row.names = FALSE)