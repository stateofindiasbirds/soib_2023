# script to create dummy long-term trend for all selected species-mask combos

library(tidyverse)

source("00_scripts/00_functions.R")
source("00_scripts/20_functions.R")
load("00_data/analyses_metadata.RData")

###

# get list of species selected for LTT in each mask
### needs to be changed to use MAIN file later ###
spec_mask_LTT_list <- map2(analyses_metadata$SPECLISTDATA.PATH, 
                           analyses_metadata$MASK.ORDERED, 
                           ~ {
                             
                             load(.x)
                             
                             new_list <- specieslist %>% 
                               left_join(restrictedspecieslist, by = "COMMON.NAME") %>%
                               mutate(ht = case_when(ht.y == 1 ~ 1,
                                                     TRUE ~ ht.x)) %>%
                               filter(ht == 1) %>%
                               dplyr::select(COMMON.NAME, ht) %>% 
                               mutate(MASK.ORDERED = .y)
                             
                             new_list
                             
                           }) %>% 
  list_rbind() %>% 
  arrange(MASK.ORDERED)

save(spec_mask_LTT_list, file = "00_data/spec_mask_selections.RData")


###

# plotting for each mask
tic.clearlog()
tic("Plotting all species trends for all masks") # 20 min for dummy plots
analyses_metadata %>% 
  distinct(MASK.ORDERED) %>% 
  pull(MASK.ORDERED) %>% 
  # walking over each mask
  walk(~ {
    
    tic(glue("Plotted all species trends for {.x}"))
    
    cur_list <- spec_mask_LTT_list %>% 
      filter(MASK.ORDERED == .x)
    
    walk2(cur_list$COMMON.NAME, cur_list$MASK.ORDERED, ~ {

      # creating metadata
      cur_metadata <- data.frame(COMMON.NAME = .x,
                                 MASK.ORDERED = .y) %>% 
        left_join(analyses_metadata) %>% 
        join_mask_codes() %>% 
        # renaming species for website
        mutate(SPECIES.NAME = str_replace_all(COMMON.NAME, 
                                              c(" " = "-", "'" = "_")),
               WEB.PLOT.PATH.TREND = glue("{WEB.PLOTS.FOLDER}{SPECIES.NAME}_{MASK.CODE}_trend.png"),
               WEB.PLOT.PATH.RANGEMAP = glue("{WEB.PLOTS.FOLDER}{SPECIES.NAME}_{MASK.CODE}_rangemap.png"),
               PLOT.SINGLE.PATH = glue("{PLOT.SINGLE.FOLDER}{SPECIES.NAME}_{MASK.CODE}_trend.png"),
               PLOT.MULTI.PATH = glue("{PLOT.MULTI.FOLDER}{SPECIES.NAME}_{MASK.CODE}_trend.png"),
               PLOT.COMPOSITE.PATH = glue("{PLOT.COMPOSITE.FOLDER}{SPECIES.NAME}_{MASK.CODE}_trend.png"))
      
      # paths
      webpath_plot_trend <- cur_metadata$WEB.PLOT.PATH.TREND
      webpath_plot_rangemap <- cur_metadata$WEB.PLOT.PATH.RANGEMAP
      # path_single_plot <- cur_metadata$PLOT.SINGLE.PATH
      # path_multispecies_plot <- cur_metadata$PLOT.MULTI.PATH
      # path_composite_plot <- cur_metadata$PLOT.COMPOSITE.PATH

      dummy_plot <- ggplot(cur_metadata) +
        labs(title = cur_metadata$SPECIES.NAME)

      ggsave(filename = webpath_plot_trend, dummy_plot)
      ggsave(filename = webpath_plot_rangemap, dummy_plot)
      
        
      })
    
    toc(log = TRUE, quiet = TRUE)
    
  })
toc(log = TRUE, quiet = TRUE)
tic.log()


