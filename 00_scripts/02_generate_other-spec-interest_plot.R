library(tidyverse)
library(glue)
library(ggpattern)

source("00_scripts/00_functions.R")

out_path <- "02_graphs/other_species_of_interest/"
if (!dir.exists(out_path)) {dir.create(out_path, recursive = TRUE)}

# "other species of interest" (India Checklist)
spec_list_left <- c(
  "Black-capped Kingfisher", "Pied Kingfisher", "Sarus Crane", "Kentish Plover", 
  "Alexandrine Parakeet", "Common Crane", "Forest Wagtail", "Sirkeer Malkoha"
) 
spec_list_right <- c(
  "Baillon's Crake", "Small Minivet", "White-rumped Shama", "Grey Wagtail", 
  "Spot-breasted Fantail", "Blue Rock Thrush", "Rosy Starling", "Spot-winged Starling"
)

###

plot_fn <- function(main_data) {
  
  main_plot <- main_data %>% 
    pivot_longer(cols = c(everything(), -India.Checklist.Common.Name),
                 names_to = "Trend", values_to = "Value") %>%
    mutate(Trend = case_when(Trend == "longterm" ~ "Long-term",
                             Trend == "currentslope" ~ "Current Annual")) %>% 
    mutate(Trend = factor(Trend, levels = c("Long-term", "Current Annual"))) %>% 
    ggplot(aes(x = India.Checklist.Common.Name, y = Value)) +
    geom_point(aes(fill = Trend), 
               colour = "black", size = 4, shape = 23,
               position = position_dodge(0.5)) +
    scale_fill_manual(values = c("transparent", "black")) +
    scale_y_continuous(limits = c(-5, 5), breaks = c(-5, 5)) +
    theme_void() +
    theme(legend.position = "none",
          # guide lines
          panel.grid.major.y = element_line(colour = "black"))
  
  return(main_plot)
  
}


###

main_data0 <- read.csv("01_analyses_full/results/SoIB_main.csv") %>% 
  dplyr::select(India.Checklist.Common.Name, 
                contains("longterm"), contains("currentslope"),
                SoIB.Latest.Long.Term.Status, SoIB.Latest.Current.Status) %>% 
  # we want to use the original scale itself, without negatives and positives
  # (so that we can use the original thresholds)
  mutate(across(contains("longterm"), ~ . + 100)) %>% 
  # scaling 
  scale_trends_to_bands() %>% 
  dplyr::select(India.Checklist.Common.Name, 
                longtermlci, longtermmean, longtermrci, longterm,
                currentslopelci, currentslopemean, currentsloperci, currentslope)

main_data_left = main_data0 %>% 
  filter(India.Checklist.Common.Name %in% spec_list_left) %>% 
  mutate(India.Checklist.Common.Name = factor(India.Checklist.Common.Name, 
                                              levels = spec_list_left)) %>% 
  dplyr::select(India.Checklist.Common.Name, longterm, currentslope)

main_data_right = main_data0 %>% 
  filter(India.Checklist.Common.Name %in% spec_list_right) %>% 
  mutate(India.Checklist.Common.Name = factor(India.Checklist.Common.Name, 
                                              levels = spec_list_right)) %>% 
  dplyr::select(India.Checklist.Common.Name, longterm, currentslope)


plot_left <- plot_fn(main_data_left)
plot_right <- plot_fn(main_data_right)


ggsave(plot_left, filename = glue("{out_path}01_left.svg"),
       dpi = 300, height = 65, width = 225, units = "mm", bg = "transparent")
ggsave(plot_right, filename = glue("{out_path}02_right.svg"),
       dpi = 300, height = 65, width = 225, units = "mm", bg = "transparent")

