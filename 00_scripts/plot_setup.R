
# create ----------------------------------------------------------------------------

create_soib_trend_plot <- function(plot_type, cur_mask, 
                                   cur_trend, cur_spec,
                                   data_trends, path_write) {

  # setup -----------------------------------------------------------------------------
  
  # output file name
  path_write_file <- glue("{path_write}{cur_spec}.png")
  
  # filtering data for current case
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
  
  
  # plot/theme settings -----------------------------------------------------
  
  palette_plot_elem <- "#56697B"
  palette_plot_title <- "#A13E2B"
  palette_trend_groups <- c("#869B27", "#E49B36", "#436b74", "#CC6666", 
                            "#B69AC9", "#78CAE0","#31954E","#493F3D",
                            "#EA5599", "#9999CC", "#A13E2B", "#66CC99")
  if (plot_type == "single") {
    palette_trend_groups <- palette_trend_groups[1]
  }
  
  plot_fontfamily <- "Gill Sans MT"
  
  
  # other plot settings
  if (cur_trend == "LTT") {
    
    timegroups_lab <- c("before 2000", "2000-2006", "2007-2010", "2011-2012", 
                        "2013", "2014", "2015", "2016", "2017", "2018", "2019", 
                        "2020", "2021", "2022")
    timegroups_bracket_min <- c(1999, 2006, 2010, 2012, seq(2013, 2021)) + 0.5
    timegroups_bracket_max <- c(2006, 2010, 2012, seq(2013, 2022)) + 0.5
    plot_ytitle_margin <- margin(0, -0.6, 0, 0.4, "cm")
    plot_xlimits <- c(1999, 2024.7)
    plot_gridline_x <- 2023.5
    plot_baseline_lab <- "Pre-2000\nbaseline"
    
  } else if (cur_trend == "CAT") {
    
    timegroups_lab <- c("2015", "2016", "2017", "2018", "2019", "2020", "2021", "2022")
    timegroups_bracket_min <- seq(2015, 2021) + 0.5
    timegroups_bracket_max <- seq(2016, 2022) + 0.5
    plot_ytitle_margin <- margin(0, 0.6, 0, 0.4, "cm")
    plot_xlimits <- c(2015.5, 2023.1)
    plot_gridline_x <- 2022.7
    plot_baseline_lab <- "2015\nbaseline"
    
  }

    
  # determining limits for current plot -----------------------------------------------

  plot_xmin <- cur_data_trends %>%
    dplyr::select(COMMON.NAME, timegroups) %>%
    arrange(COMMON.NAME, timegroups) %>%
    group_by(COMMON.NAME) %>%
    slice(2) %>% # because 1st is the baseline
    ungroup() %>%
    pull(timegroups) %>%
    max() # when multi-species, we take the latest year ### ###


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

  plot_range_max <- plot_ymax - plot_ymin


  # fixing the Status reference grid line -------------------------------------------------

  # depending on the present-day trend value (and which Status), we want its nearest
  # grid line to act as a reference for the Status threshold.

  ref_line <- cur_data_trends %>%
    filter(timegroups == 2022) %>%
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
    # we need to adjust the closest line (min distance)
    mutate(abs_diff = abs(breaks - ref_line),
           min = min(abs_diff)) %>%
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


  # assigning objects to environment --------------------------------------------------

  obj_list <- list(path_write_file = path_write_file,
                   cur_trend = cur_trend,
                   cur_data_trends = cur_data_trends,
                   palette_plot_elem = palette_plot_elem,
                   palette_plot_title = palette_plot_title,
                   palette_trend_groups = palette_trend_groups,
                   plot_fontfamily = plot_fontfamily,
                   timegroups_lab = timegroups_lab,
                   timegroups_bracket_min = timegroups_bracket_min,
                   timegroups_bracket_max = timegroups_bracket_max,
                   plot_ytitle_margin = plot_ytitle_margin,
                   plot_xlimits = plot_xlimits,
                   plot_gridline_x = plot_gridline_x,
                   plot_baseline_lab = plot_baseline_lab,
                   plot_xmin = plot_xmin,
                   plot_ymin = plot_ymin,
                   plot_ymin0 = plot_ymin0,
                   plot_ymax = plot_ymax,
                   plot_ymax0 = plot_ymax0,
                   plot_ybreaks = plot_ybreaks,
                   plot_ybreaks_lab = plot_ybreaks_lab,
                   plot_range_max = plot_range_max)
  
  list2env(obj_list, envir = .GlobalEnv)

  # creating and writing the plot -----------------------------------------------------

  cur_plot <- ggplot(cur_data_trends,
                     aes(x = timegroups, y = mean_std, ymin = lci_std, ymax = rci_std)) +
    geom_lineribbon(fill = palette_trend_groups, colour = "black",
                    linewidth = 0.7, alpha = 1) +
    geom_point(size = 3, colour = "black") +
    geom_axisbracket("time") + # timegroup brackets
    # "Current Trend" bracket
    {if (cur_trend != "CAT") {
      geom_axisbracket("trend")
    }} +
    # manual grid lines with labels because we want empty space before the first timegroup
    geom_gridline(1) +
    geom_gridline(2) +
    geom_gridline(3) +
    geom_gridline(4) +
    geom_gridline(5) +
    geom_gridline(baseline = TRUE) +
    scale_x_continuous(expand = c(0, 0), limits = plot_xlimits) +
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
  
  # removing objects from global environment
  rm(list = names(obj_list), envir = .GlobalEnv)

}


# full ------------------------------------------------------------------------------

plot_soib_trends <- function(plot_type = "single", 
                             cur_mask = "none",
                             cur_trend = cur_trend,
                             cur_spec = cur_spec) {
  
  # error checks ----------------------------------------------------------------------
  
  if (!plot_type %in% c("single", "single_mask", "multi", "composite")) {
    return("Select valid plot type!")
  }
  
  if (!exists("cur_trend")) {
    return("Need to select which trend to plot!")
  } else if (!cur_trend %in% c("LTT", "CAT")) {
    return("Select valid trend!")
  }
    
  if (!exists("cur_spec")) {
    return('Need to select at least one species to plot trends for! Or use "all" to plot this trend for all qualifying species.')
  }
  
  
  # packages -------------------------------------------------------
  
  require(tidyverse)
  require(ggdist) # geom_lineribbon
  require(ggpubr) # geom_bracket
  require(extrafont)
  require(glue)
  library(ggrepel) # text repel

  source('00_scripts/00_functions.R')
  source('00_scripts/plot_functions.R')
  
  
  # current metadata --------------------------------------------------
  
  load("00_data/analyses_metadata.RData")
  cur_metadata <- analyses_metadata %>% filter(MASK == cur_mask)
  
  path_data_main <- cur_metadata$SOIBMAIN.PATH
  path_data_trends <- cur_metadata$TRENDS.OUTPATH
  
  
  # path (folder) to write to; create path if doesn't exist
  
  cur_metadata <- cur_metadata %>% 
    mutate(CUR.OUT.PATH = case_when(plot_type == "single" ~ PLOT.SINGLE.FOLDER,
                                    plot_type == "single_mask" ~ PLOT.SINGLE.MASKS.FOLDER,
                                    plot_type == "multi" ~ PLOT.MULTI.FOLDER,
                                    plot_type == "composite" ~ PLOT.COMPOSITE.FOLDER))
  
  if (cur_trend == "LTT") {
    path_write <- cur_metadata %>% 
      mutate(PLOT.OUTPATH = glue("{CUR.OUT.PATH}long-term trends/")) %>% 
      pull(PLOT.OUTPATH)
  } else if (cur_trend == "CAT") {
    path_write <- cur_metadata %>% 
      mutate(PLOT.OUTPATH = glue("{CUR.OUT.PATH}current trends/")) %>% 
      pull(PLOT.OUTPATH)
  }

  if (!dir.exists(path_write)) {
    dir.create(path_write, recursive = TRUE)
  }
  
  
  # load data ---------------------------------------------------------------
  
  data_main = read.csv(path_data_main)
  data_trends = read.csv(path_data_trends)
  
  # filtering for qualified species only
  # - not plotting inconclusive or data deficient; 
  # - only species sel. for that trend; 
  # - only till MY 2022
  if (cur_trend == "LTT") {
    
    spec_qual <- data_main %>% 
      filter(!(SOIBv2.Long.Term.Status %in% c("eBird Data Inconclusive",
                                              "eBird Data Deficient")),
             Long.Term.Analysis == "X") %>%  #### ### ### ###
      pull(eBird.English.Name.2022)
    
    data_trends <- data_trends %>% 
      filter(COMMON.NAME %in% spec_qual,
             timegroups <= 2022)
    
  } else if (cur_trend == "CAT") {
    
    spec_qual <- data_main %>% 
      filter(!(SOIBv2.Current.Status %in% c("eBird Data Indecisive",
                                            "eBird Data Deficient")),
             Current.Analysis == "X") %>%  #### ### ### ###
      pull(eBird.English.Name.2022)
    
    data_trends <- data_trends %>% 
      filter(COMMON.NAME %in% spec_qual,
             timegroups >= 2015 & timegroups <= 2022)
    
  }
  

  # generating plots ----------------------------------------
  
  if (cur_spec == "all") {
    
    walk(spec_qual, create_soib_trend_plot(plot_type = plot_type, 
                                           cur_mask = cur_mask, 
                                           cur_trend = cur_trend, 
                                           cur_spec = spec_qual,
                                           data_trends = data_trends,
                                           path_write = path_write))
    
  } else {
    
    create_soib_trend_plot(plot_type = plot_type, 
                           cur_mask = cur_mask, 
                           cur_trend = cur_trend, 
                           cur_spec = cur_spec,
                           data_trends = data_trends,
                           path_write = path_write)
    
  }


}

