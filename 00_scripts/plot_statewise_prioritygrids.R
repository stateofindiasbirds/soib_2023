library(tidyverse)
library(sf)
library(glue)
library(tictoc)

load("00_data/maps_sf.RData") 
load("00_data/grids_st_sf.RData") 
load("00_data/dataforanalyses_extra.RData")
data0_filt <- data0 %>% distinct(ST_NM, gridg1, COMMON.NAME)

source("00_scripts/20_functions.R")

# don't use spherical geometry for sf objects
sf_use_s2(FALSE)

# importing SoIB results data
main0 <- get_metadata("none") %>% 
  pull(SOIBMAIN.PATH) %>% 
  read_csv()


states_to_walk <- get_metadata() %>% 
  filter(MASK.TYPE == "state") %>% 
  # temp. exclude AR due to issue with admin boundary
  filter(MASK != "Arunachal Pradesh") %>% 
  arrange(MASK) %>% 
  pull(MASK)


# function to create plot -----------------------------------------------------------

create_state_prioritygrids_plot <- function(state, 
                                            data_ebd = data0_filt, data_priority = main0,
                                            cur_path = "02_graphs/statewise_prioritygrids/") {
  
  # create subfolder for these plots if doesn't exist
  if (!dir.exists(cur_path)) {dir.create(cur_path, recursive = TRUE)}
  
  
  tic(glue("Created plot for {state}"))
  
  cur_str <- glue("{cur_path}{state}.png")
  
  
  cur_states_sf <- states_sf %>% 
    filter(STATE.NAME == state) %>%
    dplyr::select(-AREA)
  
  cur_g1_sf <- g1_st_sf %>% filter(STATE.NAME == state)
  
  cur_data <- data_ebd %>% 
    filter(ST_NM == state) %>% 
    distinct(gridg1, COMMON.NAME) %>% 
    # add priority status
    left_join(data_priority %>% distinct(eBird.English.Name.2024, SoIB.Latest.Priority.Status), 
              by = c("COMMON.NAME" = "eBird.English.Name.2024")) %>% 
    # get top 5 grid cells by number of high priority species
    group_by(gridg1, SoIB.Latest.Priority.Status) %>% 
    reframe(NO.SP = n_distinct(COMMON.NAME)) %>% 
    filter(SoIB.Latest.Priority.Status == "High") %>% 
    arrange(desc(NO.SP)) %>% 
    slice(1:5) %>% 
    distinct(gridg1, NO.SP) %>% 
    # joining sf grids
    left_join(cur_g1_sf, by = c("gridg1" = "GRID.G1"))
  
  cur_plot <- ggplot() +
    geom_sf(data = cur_states_sf, fill = "grey80", col = "black") +
    geom_sf(data = cur_data, mapping = aes(geometry = GEOM.G1),
            fill = "darkred", col = "black") +
    theme_void() +
    theme(plot.background = element_rect(fill = "transparent", colour = NA),
          panel.background = element_rect(fill = "transparent", colour = NA))
  
  ggsave(filename = cur_str, plot = cur_plot,
         dpi = 1000, bg = "transparent",
         width = 7, height = 7, units = "in")
  
  toc()
  
}


# executing walk to create plots ----------------------------------------------------

tic("Created plots for all states")

states_to_walk %>% 
  walk(create_state_prioritygrids_plot)

toc() 

