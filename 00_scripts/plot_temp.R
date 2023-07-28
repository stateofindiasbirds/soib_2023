cur_mask <- "none"
cur_trend <- "LTT"

# current species in the overall iteration
# cur_spec <- .x
cur_spec <- "Alpine Swift"

### ###


path_write_file <- glue("{path_write}{cur_spec}.png")

if (length(cur_spec) == 1) {
  palette_trend_groups <- palette_trend_groups[1]
}

### ###

if (cur_trend == "LTT") {
  
  cur_data_trends <- data_trends %>% 
    filter(COMMON.NAME %in% cur_spec)

} else if (cur_trend == "CAT") {
  
  cur_data_trends <- data_trends %>%
    # renaming columns to be plotted
    mutate(lci_std = lci_std_recent,
           mean_std = mean_std_recent,
           rci_std = rci_std_recent) %>%
    filter(COMMON.NAME %in% cur_spec)
  
}

cur_data_trends_present <- cur_data_trends %>% filter(timegroups == 2022)


# determining limits for current plot -----------------------------------------------

# saving non-rounded values for later use in plotting
plot_ymax0 <- cur_data_trends %>% 
  filter(!is.na(rci_std)) %>% 
  pull(rci_std) %>% 
  max()

plot_ymin0 <- cur_data_trends %>% 
  filter(!is.na(lci_std)) %>% 
  pull(rci_std) %>% 
  min()

plot_ymax <- plot_ymax0 %>% plyr::round_any(accuracy = 50, f = ceiling)
plot_ymin <- plot_ymin0 %>% plyr::round_any(accuracy = 50, f = floor)


# ensuring range is not too small
if ((plot_ymax - plot_ymin) < 100 & plot_ymin < 0) {
  plot_ymin <- plot_ymin - 50
}
if ((plot_ymax - plot_ymin) < 100 & plot_ymax > 0) {
  plot_ymax <- plot_ymax + 50
}

plot_range_max <- plot_ymax - plot_ymin
  

# determining y-axis breaks for current plot ----------------------------------------

plot_ybreaks <- seq(plot_ymin, plot_ymax, length.out = 5)

# if 100 not in the ybreaks, adjustment needed
if (any(plot_ybreaks != 100)) {
  
  # how far from 100 is the absolute closest break 
  breaks_abs_distance = sort(abs(plot_ybreaks - 100))

  # what happens when closest distance is subtracted from original breaks?  
  breaks_subtract = plot_ybreaks - breaks_abs_distance[1] 
  # what happens when closest distance is added to original breaks?
  breaks_add = plot_ybreaks + breaks_abs_distance[1] 
  
  # we don't want to subtract if that results in negative breaks (lower limit is 0)
  if (any(breaks_subtract == 100) & min(breaks_subtract) >= 0) {
    
    plot_ybreaks = breaks_subtract
    
    # need to update lower plot limit
    plot_ymin = plyr::round_any(plot_ybreaks[1], 50, floor)
    
  } else { # obvious that we can't subtract anything, so add
    
    if (any(breaks_subtract == 100) & min(breaks_subtract) < 0) {
      
      # min is negative because closest break can only be subtracted to get to 100
      # hence, now we need to use second closest break; only add cos otherwise negative.
      plot_ybreaks = plot_ybreaks + breaks_abs_distance[2]
      
    } else if (any(breaks_add == 100)) {
      
      plot_ybreaks = breaks_add
      
    }
    
    # need to update upper plot limit
    plot_ymax = plyr::round_any(plot_ybreaks[5], 50, ceiling)
    # adding 1% buffer <annotation_pending_AV>
    plot_ymax = plot_ymax + round(0.01 * (plot_ymax - plot_ymin))
    
  }
  
  plot_ybreaks = plyr::round_any(plot_ybreaks, 10, round)

}


# fixing the Status reference grid line -------------------------------------------------

# depending on the present-day trend value (and which Status), we want its nearest 
# grid line to act as a reference for the Status threshold. 

ref_line <- cur_data_trends_present %>% 
  mutate(ref = case_when(
    
    # stable/inconclusive
    lci_std <= 100 & rci_std >= 100 ~ 100,
    
    # increases
    lci_std > 100 & lci_std <= 125 ~ 125,
    lci_std > 125 & lci_std <= 150 ~ 125, # I
    lci_std > 150 & lci_std <= 200 ~ 150, # RI
    lci_std > 200 ~ 200, # RI+
    
    # declines
    rci_std < 100 & rci_std >= 75 ~ 75,
    rci_std < 75 & rci_std >= 50 ~ 75, # D
    rci_std < 50 ~ 50 # RD
    
  )) %>% 
  pull(ref)


plot_ybreaks_df <- data.frame(breaks = plot_ybreaks,
                              ref_line = ref_line) %>% 
  mutate(abs_diff = abs(breaks - ref_line),
         min = min(abs_diff)) %>% # we need to adjust the closest line (min distance)
  # changing the appropriate line to our reference line
  mutate(breaks = if_else(abs_diff == min, ref_line, breaks)) %>% 
  # labels for each line/break
  mutate(breaks_eff = breaks - 100) %>% # convert to + and - values
  mutate(labs = case_when(breaks_eff > 0 ~ glue("+{breaks_eff}%"),
                          breaks == 100 ~ glue(""), # baseline blank
                          TRUE ~ glue("{breaks_eff}%"))) 

plot_ybreaks <- plot_ybreaks_df$breaks
plot_ybreaks_lab <- plot_ybreaks_df$labs


# for plotting
if (min(plot_ybreaks) < plot_ymin0) {
  plot_ymin0 = min(plot_ybreaks)
}
if (max(plot_ybreaks) > plot_ymax0) {
  plot_ymax0 = max(plot_ybreaks)
}


# get x-axis right ------------------------------------------------------------------

plot_xmin <- cur_data_trends %>%
  select(COMMON.NAME, timegroups) %>%
  arrange(COMMON.NAME, timegroups) %>%
  group_by(COMMON.NAME) %>% 
  slice(2) %>% # because 1st is the baseline
  ungroup() %>% 
  pull(timegroups) %>% 
  max() # when multi-species, we take the latest year ### ###


# creating and writing the plot -----------------------------------------------------

cur_plot <- ggplot(cur_data_trends, 
                    aes(x = timegroups, y = mean_std, ymin = lci_std, ymax = rci_std)) +
  geom_lineribbon(fill = palette_trend_groups, colour = "black", 
                  linewidth = 0.7, alpha = 1) +
  geom_point(size = 3, colour = "black") +
  geom_axisbracket("time") + # timegroup brackets
  geom_axisbracket("trend") + # "Current Trend" bracket
  # manual grid lines with labels because we want empty space before the first timegroup
  geom_gridline(1) +
  geom_gridline(2) +
  geom_gridline(3) +
  geom_gridline(4) +
  geom_gridline(5) +
  geom_gridline(baseline = TRUE) +
  scale_x_continuous(expand = c(0, 0), limits = c(1999, 2024.7)) +
  scale_y_continuous(expand = c(0, 0)) +
  coord_cartesian(ylim = c(plot_ymin0 - 0.1 * plot_range_max, 
                           plot_ymax0 + 0.1 * plot_range_max), 
                  clip = "off") +
  # ggtitle(cur_spec) +
  labs(x = "Time-steps", y = "Change in eBird Abundance Index") +
  guides(colour = "none") +
  # theme
  ggtheme_soibtrend()
  

ggsave(filename = path_write_file, plot = cur_plot,
       dpi = 500, bg = "transparent",
       width = 11, height = 7.5, units = "in")



