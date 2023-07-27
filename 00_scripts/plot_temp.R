cur_mask <- "none"
cur_trend <- "LTT"

# current species in the overall iteration
cur_spec <- .x
# cur_spec <- "Alpine Swift"



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
plot_CI_max0 <- cur_data_trends %>% 
  filter(!is.na(rci_std)) %>% 
  pull(rci_std) %>% 
  max()

plot_CI_min0 <- cur_data_trends %>% 
  filter(!is.na(lci_std)) %>% 
  pull(rci_std) %>% 
  min()

plot_CI_max <- plot_CI_max0 %>% plyr::round_any(accuracy = 50, f = ceiling)
plot_CI_min <- plot_CI_min0 %>% plyr::round_any(accuracy = 50, f = floor)


# ensuring range is not too small
if ((plot_CI_max - plot_CI_min) < 100 & plot_CI_min < 0) {
  plot_CI_min <- plot_CI_min - 50
}
if ((plot_CI_max - plot_CI_min) < 100 & plot_CI_max > 0) {
  plot_CI_max <- plot_CI_max + 50
}

plot_range_max <- plot_CI_max - plot_CI_min
  

# determining y-axis breaks for current plot ----------------------------------------

plot_ybreaks <- seq(plot_CI_min, plot_CI_max, length.out = 5)

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
    plot_CI_min = plyr::round_any(plot_ybreaks[1], 50, floor)
    
  } else { # obvious that we can't subtract anything, so add
    
    if (any(breaks_subtract == 100) & min(breaks_subtract) < 0) {
      
      # min is negative because closest break can only be subtracted to get to 100
      # hence, now we need to use second closest break; only add cos otherwise negative.
      plot_ybreaks = plot_ybreaks + breaks_abs_distance[2]
      
    } else if (any(breaks_add == 100)) {
      
      plot_ybreaks = breaks_add
      
    }
    
    # need to update upper plot limit
    plot_CI_max = plyr::round_any(plot_ybreaks[5], 50, ceiling)
    # adding 1% buffer <annotation_pending_AV>
    plot_CI_max = plot_CI_max + round(0.01 * (plot_CI_max - plot_CI_min))
    
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
if (min(plot_ybreaks) < plot_CI_min0) {
  plot_CI_min0 = min(plot_ybreaks)
}
if (max(plot_ybreaks) > plot_CI_max0) {
  plot_CI_max0 = max(plot_ybreaks)
}


