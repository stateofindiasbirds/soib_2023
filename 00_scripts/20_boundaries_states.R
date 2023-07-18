library(tidyverse)
library(glue)
library(sf)
library(tictoc)

load("00_data/maps_sf.RData")

tic("Creating boundaries for states")
states_sf %>% 
  dplyr::select(-AREA) %>% 
  group_by(STATE.NAME) %>% 
  group_split() %>% 
  walk(~ {

    cur_state <- .$STATE.NAME
    
    cur_sf <- ggplot(data = .) +
      geom_sf(col = "black", fill = NA) +
      theme_void()
    
    # Create SVG file path
    cur_svg <- glue("20_website/boundaries_states/{cur_state}.svg")

    # Save the state boundary as an SVG file
    ggsave(plot = cur_sf, filename = cur_svg, 
           bg = "transparent", width = 10, height = 10, units = "in", dpi = 300)

  })
toc()
