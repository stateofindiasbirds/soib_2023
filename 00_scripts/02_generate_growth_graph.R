## plot growth in data

library(tidyverse)
library(ggdist)
library(ggridges)
library(ggpubr)
library(extrafont)
library(glue)

source('00_scripts/00_plot_functions.R')

timegroups = c("before 2000","2000-2006","2007-2010","2011-2012",
               "2013","2014","2015","2016","2017","2018")
years = c(1992, 2003, 2009, 2012, 2013, 2014, 2015, 2016, 2017, 2018)

data2 = c(7884, 7293, 7642, 7123, 12146, 35322, 64899, 96052, 129610, 143715)

data1 = c(4606, 5131, 5787, 5459, 11002, 34926, 64662, 96012, 131280, 144533)

growth = data.frame(timegroups = timegroups, years = years, soib1 = data1, soib2 = data2)
growth = growth %>%
  mutate(ratio = round(100*(soib2-soib1)/soib1) + 100)

palette_plot_elem <- "#56697B"
palette_plot_title <- "#A13E2B"
palette_trend_groups <- c("#869B27", "#E49B36", "#436b74", "#CC6666", 
                          "#B69AC9", "#78CAE0","#31954E","#493F3D",
                          "#EA5599", "#9999CC", "#A13E2B", "#66CC99")
palette_trend_groups <- palette_trend_groups[1]
plot_fontfamily <- "Gandhi Sans"


timegroups_lab <- c("before 2000", "2000-2006", "2007-2010", "2011-2012", 
                    "", "2014", "", "2016", "", "2018")
timegroups_bracket_min <- c(1990.5,2000, 2006, 2010, 2012, seq(2013, 2017)) + 0.5
timegroups_bracket_max <- c(2000, 2006, 2010, 2012, seq(2013, 2018)) + 0.5
plot_ytitle_margin <- margin(0, -0.6, 0, 0.4, "cm")
plot_xlimits <- c(1989.5, 2021)
plot_gridline_x <- 2019.8
plot_xmin = 1991

plot_ymax0 <- growth %>%
  pull(ratio) %>%
  max()

plot_ymin0 <- growth %>%
  pull(ratio) %>%
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
    # adding 1% buffer to ensure space near the margins
    plot_ymax = plot_ymax + round(0.01 * (plot_ymax - plot_ymin))
    
  }
  
  plot_ybreaks = plyr::round_any(plot_ybreaks, 10, round)
  
}

plot_range_max <- plot_ymax - plot_ymin

plot_ybreaks_df <- data.frame(breaks = plot_ybreaks) %>%
  # labels for each line/break
  mutate(breaks_eff = breaks - 100) %>% # convert to + and - values
  mutate(labs = case_when(breaks_eff > 0 ~ glue("+{breaks_eff}%"),
                          breaks == 100 ~ glue("0%"), # baseline blank
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

bracket_min <- timegroups_bracket_min
bracket_max <- timegroups_bracket_max
bracket_ypos <- plot_ymin0 - 0.01 * plot_range_max
bracket_tiplength <- 0
bracket_labelsize <- 4.8
bracket_fontface <- 1
bracket_vjust <- 2.5

bracket_shorten <- 0.15
bracket_lab <- timegroups_lab

geom_gridline_growth <- function(index_y = NULL, baseline = FALSE) {
  
  if (baseline) {
    
    line_y <- plot_ybreaks[index_y]
    line_linetype <- "solid"
    line_linewidth <- 0.9
    
    # label on y-axis
    ann_size <- 6
    ann_lab <- plot_ybreaks_lab[index_y]
    ann_fontface <- 1
    
  } else {
    
    line_y <- plot_ybreaks[index_y]
    line_linetype <- "dotted"
    line_linewidth <- 0.7
    
    # label on y-axis
    ann_size <- 6
    ann_lab <- plot_ybreaks_lab[index_y]
    ann_fontface <- 1
    
  }
  
  line_end <- 2018.5

  # provide both the grid line and its label
  list(
    geom_segment(x = plot_xmin, xend = line_end, col = palette_plot_elem, # constant 
                 y = line_y, yend = line_y, 
                 linetype = line_linetype, linewidth = line_linewidth),
    
    annotate("text", x = plot_gridline_x, # constant
             colour = palette_plot_elem, family = plot_fontfamily, # constant
             y = line_y, label = ann_lab, size = ann_size, fontface = ann_fontface)
  )
  
}


plot_base <- ggplot(growth, aes(x = years, y = ratio)) +
  #geom_line(aes(group = 1), linewidth = 1, lineend = "round", col = palette_trend_groups) +
  geom_segment(aes(x = years, xend = years, y = 100, yend = ratio), color = palette_trend_groups, size = 4)

cur_plot <- plot_base +
  geom_bracket(inherit.aes = FALSE, family = plot_fontfamily, col = palette_plot_elem, # constant
               xmin = bracket_min, xmax = bracket_max, y.position = bracket_ypos,
               label = bracket_lab, label.size = bracket_labelsize, fontface = bracket_fontface,
               tip.length = bracket_tiplength, vjust = bracket_vjust, bracket.shorten = bracket_shorten) +
  # manual grid lines with labels because we want empty space before the first timegroup
  geom_gridline_growth(1) +
  geom_gridline_growth(2,baseline=T) +
  geom_gridline_growth(3) +
  geom_gridline_growth(4) +
  geom_gridline_growth(5) +
  coord_cartesian(ylim = c(plot_ymin0 - 0.1 * plot_range_max,
                           plot_ymax0 + 0.1 * plot_range_max),
                  clip = "off") +
  scale_x_continuous(expand = c(0, 0), limits = plot_xlimits) +
  scale_y_continuous(expand = c(0, 0)) +
  labs(x = "Time-steps", y = "Change in available data from SoIB 2020") +
  guides(colour = "none", fill = "none") +
  # theme
  ggtheme_soibtrend()

ggsave(filename = "02_graphs/SoIB_growth.png", plot = cur_plot,
       dpi = 500, bg = "transparent",
       width = 11, height = 7.5, units = "in")
