web_db[, c(1:2, 19:30, 65:67)] %>% View()

gen_trend_plots("single_mask", "LTT", "White-winged Duck") 
gen_trend_plots("single_mask", "CAT", "White-winged Duck") 


web_db0 %>% 
  filter(MASK == "Ladakh",
         India.Checklist.Common.Name %in% c("White-throated Dipper", "Black-winged Snowfinch")) %>% 
  select(India.Checklist.Common.Name, longtermlci, longtermmean, longtermrci,
           currentslopelci, currentslopemean, currentsloperci, starts_with("SOIBv2.")) %>% 
  View()

x %>% 
  filter(India.Checklist.Common.Name %in% c("White-throated Dipper", "Black-winged Snowfinch")) %>% 
  select(India.Checklist.Common.Name, longtermlci, longtermmean, longtermrci,
         currentslopelci, currentslopemean, currentsloperci, starts_with("SOIBv2.")) %>% 
  View()


web_db %>% 
  filter(MASK == "Ladakh",
         India.Checklist.Common.Name %in% c("White-throated Dipper", "Black-winged Snowfinch")) %>% 
  select(India.Checklist.Common.Name, longtermlci, longtermmean, longtermrci,
         currentslopelci, currentslopemean, currentsloperci, starts_with("SOIBv2.")) %>% 
  View()

web_db %>% 
  filter(post_content == "Ladakh",
         post_title %in% c("White-throated Dipper", "Black-winged Snowfinch")) %>% 
  select(post_title, starts_with("long-term_trend"), starts_with("current_annual_change"),
         starts_with("graph_"), ends_with("status")) %>% 
  View()

x <- read_csv("01_analyses_states/Ladakh/results/SoIB_main.csv", 
              col_types = "ccccccccccccccccccdddccccccccdcccddddddddddddddddddddddddddddddddddddcccc")

# -----------------------------------------------------------------------------------


require(tidyverse)
require(ggpubr) # geom_bracket
require(extrafont)
require(glue)
require(ggrepel) # text repel
require(tictoc)
require(furrr)
require(parallel)

source('00_scripts/00_functions.R')
source('00_scripts/00_plot_functions.R')

load("00_data/analyses_metadata.RData")

plot_type = "single_mask"
cur_trend = "LTT" 
cur_spec = "White-winged Duck"

plot_load_filter_data(plot_type, cur_trend, "Assam")

soib_trend_plot(plot_type = plot_type,
                cur_trend = cur_trend,
                cur_spec = cur_spec,
                data_trends = data_trends,
                data_main = data_main,
                path_write = path_write,
                cur_plot_metadata = web_metadata, 
                haki = advanced_kenbunshoku)