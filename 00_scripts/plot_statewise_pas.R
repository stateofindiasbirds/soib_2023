library(tidyverse)
library(sf)
library(glue)
library(tictoc)
library(furrr)

load("00_data/maps_sf.RData")
load("00_data/maps_pa_sf.RData")


states_to_walk <- get_metadata() %>% 
  filter(MASK.TYPE == "state") %>% 
  # temp. exclude AR due to issue with admin boundary
  filter(MASK != "Arunachal Pradesh") %>% 
  arrange(MASK) %>% 
  pull(MASK)


# function to create plot -----------------------------------------------------------

create_statepas_plot <- function(state, cur_path = "02_graphs/statewise_pas/") {
  
  # create subfolder for these plots if doesn't exist
  if (!dir.exists(cur_path)) {dir.create(cur_path, recursive = TRUE)}
  
  # don't use spherical geometry for sf objects
  sf_use_s2(FALSE)
  
  
  tic(glue("Created plot for {state}"))
  
  cur_str <- glue("{cur_path}{state}.png")
  

  cur_states_sf <- states_sf %>% 
    filter(STATE.NAME == state) %>% 
    dplyr::select(-AREA)
  
  cur_pa_sf <- pa_sf %>% 
    st_intersection(cur_states_sf)
  
  
  cur_plot <- ggplot() +
    geom_sf(data = cur_states_sf, fill = "grey80", col = "black") +
    geom_sf(data = cur_pa_sf, fill = "#BBD096", col = "black") +
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

print(glue("Activated future-walking using advanced Kenbunshoku Haki!"))

# start multiworker parallel session
plan(multisession, workers = parallel::detectCores()/2)

states_to_walk %>% 
  future_walk(create_statepas_plot,
              .progress = TRUE, .options = furrr_options(seed = TRUE))

# end multiworker parallel session
plan(sequential)

toc() 

