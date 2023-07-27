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
      geom_sf(col = NA, fill = "white") +
      theme_void()
    
    # Create PNG file path
    cur_png <- glue("20_website/boundaries_icons/{cur_state}.png")
    # Create SVG file path
    cur_svg <- glue("20_website/boundaries_icons/{cur_state}.svg")

    # Save the state boundary as an PNG file
    ggsave(plot = cur_sf, filename = cur_png, 
           bg = "transparent", width = 10, height = 10, units = "in", dpi = 300)
    # Save the state boundary as an SVG file
    ggsave(plot = cur_sf, filename = cur_svg, 
           bg = "transparent", width = 10, height = 10, units = "in", dpi = 300)

  })
toc()


tic("Creating boundaries for whole country")
cur_sf <- india_sf %>% 
  dplyr::select(-AREA) %>% 
  ggplot() +
  geom_sf(col = NA, fill = "white") +
  theme_void() 

# Create PNG file path
cur_png <- glue("20_website/boundaries_icons/none.png")
# Create SVG file path
cur_svg <- glue("20_website/boundaries_icons/none.svg")

# Save the state boundary as an PNG file
ggsave(plot = cur_sf, filename = cur_png, 
       bg = "transparent", width = 10, height = 10, units = "in", dpi = 300)
# Save the state boundary as an SVG file
ggsave(plot = cur_sf, filename = cur_svg, 
       bg = "transparent", width = 10, height = 10, units = "in", dpi = 300)

toc()