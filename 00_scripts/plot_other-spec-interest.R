library(tidyverse)
library(glue)
library(ggpattern)

plot_fn <- function(main_data) {
  
  main_plot <- main_data %>% 
    pivot_longer(cols = c(everything(), -India.Checklist.Common.Name),
                 names_to = "Trend", values_to = "Value") %>%
    mutate(Trend = str_replace(Trend, "_std", "_ltt")) %>% 
    mutate(Trend = str_replace(Trend, "_ltt_recent", "_cat")) %>% 
    separate(col = "Trend", into = c("Measure", "Trend"), 
             sep = "_", remove = FALSE) %>%
    mutate(Trend = case_when(Trend == "ltt" ~ "Long-term",
                             Trend == "cat" ~ "Current Annual")) %>% 
    mutate(Trend = factor(Trend, levels = c("Long-term", "Current Annual"))) %>% 
    pivot_wider(names_from = Measure, values_from = Value) %>% 
    ggplot() +
    geom_linerange(aes(x = India.Checklist.Common.Name, y = mean,
                       ymin = lci, ymax = rci, linetype = Trend), 
                   linewidth = 2, position = position_dodge(0.5)) +
    scale_linetype_manual(values = c("dotdash", "solid")) +
    theme_void()
  
  return(main_plot)
  
}

out_path <- "02_graphs/other_species_of_interest/"
if (!dir.exists(out_path)) {dir.create(out_path, recursive = TRUE)}

# "other species of interest" (India Checklist)
spec_list_left <- c(
  "Black-capped Kingfisher", "Sarus Crane", "Alexandrine Parakeet", "Forest Wagtail",
  "Pied Kingfisher", "Kentish Plover", "Common Crane", "Sirkeer Malkoha"
) 
spec_list_right <- c(
  "Baillon's Crake", "White-rumped Shama", "Spot-breasted Fantail", "Rosy Starling", 
  "Small Minivet", "Grey Wagtail", "Blue Rock Thrush", "Spot-winged Starling"
)


###

trends_data = read.csv("01_analyses_full/results/trends.csv") %>% 
  # taking latest (2022-23) values
  filter(timegroups == 2022) %>% 
  rename(eBird.English.Name.2022 = COMMON.NAME) %>% 
  # selecting standardised values
  dplyr::select(c(eBird.English.Name.2022, lci_std, mean_std, rci_std, contains("std_recent")))

# main data
main_data_left = read.csv("01_analyses_full/results/SoIB_main.csv") %>% 
  filter(India.Checklist.Common.Name %in% spec_list_left) %>% 
  mutate(India.Checklist.Common.Name = factor(India.Checklist.Common.Name, 
                                              levels = spec_list_left)) %>% 
  left_join(trends_data) %>% 
  dplyr::select(India.Checklist.Common.Name, 
                lci_std, mean_std, rci_std, contains("std_recent"))
main_data_right = read.csv("01_analyses_full/results/SoIB_main.csv") %>% 
  filter(India.Checklist.Common.Name %in% spec_list_right) %>% 
  mutate(India.Checklist.Common.Name = factor(India.Checklist.Common.Name, 
                                              levels = spec_list_right)) %>% 
  left_join(trends_data) %>% 
  dplyr::select(India.Checklist.Common.Name, 
                lci_std, mean_std, rci_std, contains("std_recent"))

plot_left <- plot_fn(main_data_left)
plot_right <- plot_fn(main_data_right)


ggsave(plot_left, filename = glue("{out_path}01_left.svg"),
       dpi = 300, height = 10, width = 40, units = "in", bg = "transparent")
ggsave(plot_right, filename = glue("{out_path}02_right.svg"),
       dpi = 300, height = 10, width = 40, units = "in", bg = "transparent")
