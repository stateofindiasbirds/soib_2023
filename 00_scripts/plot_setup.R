
# create ----------------------------------------------------------------------------

create_soib_trend_plot <- function(plot_type, cur_trend, cur_spec,
                                   data_trends, data_main, path_write,
                                   cur_plot_metadata) {

  # convert to India Checklist names --------------------------------------------------
  
  if (plot_type != "composite") { 
    # because composite doesn't have any species names at this stage
    data_trends <- data_trends %>% 
      mutate(COMMON.NAME = specname_to_india_checklist(COMMON.NAME))
    data_main <- data_main %>% 
      mutate(eBird.English.Name.2022 = specname_to_india_checklist(eBird.English.Name.2022))
    cur_spec <- cur_spec %>% 
      specname_to_india_checklist()
  }
  
  # setup -----------------------------------------------------------------------------
  
  # output file name
  if (!(plot_type %in% c("multi", "composite"))) {
    
    path_write_file <- glue("{path_write}{cur_spec}.png")
    
    if (cur_trend == "LTT") {
      # for website
      source("00_scripts/20_functions.R")
      cur_plot_metadata <- join_mask_codes(cur_plot_metadata)
      web_folder <- cur_plot_metadata$WEB.PLOTS.FOLDER
      web_spec <- str_replace_all(cur_spec, c(" " = "-", "'" = "_"))
      web_mask <- cur_plot_metadata$MASK.CODE
      path_write_file_web <- glue("{web_folder}{web_spec}_{web_mask}_trend.jpg")
    } else {
      print("Not generating CAT plots for website.")
    }
    
  } else {
    path_write_file <- glue("{path_write}{cur_plot_metadata$FILE.NAME}.png")
  }
  
  # filtering data for current case
  
  if (plot_type != "composite") {
    cur_data_trends <- data_trends %>%
      mutate(lci_std = case_when(cur_trend == "CAT" ~ lci_std_recent, 
                                 TRUE ~ lci_std),
             mean_std = case_when(cur_trend == "CAT" ~ mean_std_recent, 
                                  TRUE ~ mean_std),
             rci_std = case_when(cur_trend == "CAT" ~ rci_std_recent, 
                                 TRUE ~ rci_std))%>%
      filter(COMMON.NAME %in% cur_spec)
  } else {
    cur_data_trends <- data_trends
  }
  
  
  # don't plot full-country trend line (which does not have CI band) if it is Inconclusive
  # single-species will always plot Inconclusive
  if (plot_type == "single_mask") {
    
    plot_full_country <- data_main %>% 
      filter(eBird.English.Name.2022 %in% cur_spec,
             MASK == "none") %>% 
      {if (cur_trend == "LTT") {
        pull(., SOIBv2.Long.Term.Status)
      } else if (cur_trend == "CAT") {
        pull(., SOIBv2.Current.Status)
      }}
    
  }
  
  # plot/theme settings -----------------------------------------------------
  
  palette_plot_elem <- "#56697B"
  palette_plot_title <- "#A13E2B"
  palette_trend_groups <- c("#869B27", "#E49B36", "#436b74", "#CC6666", 
                            "#B69AC9", "#78CAE0","#31954E","#493F3D",
                            "#EA5599", "#9999CC", "#A13E2B", "#66CC99")
  if (plot_type == "single") {
    palette_trend_groups <- palette_trend_groups[1]
  } else if (plot_type == "single_mask") {
    palette_trend_groups <- palette_trend_groups[1:2]
  }
  
  plot_fontfamily <- "Gill Sans MT"
  
  
  # other plot settings (LTT vs CAT)
  if (cur_trend == "LTT") {
    
    timegroups_lab <- c("before 2000", "2000-2006", "2007-2010", "2011-2012", 
                        "2013", "2014", "2015", "2016", "2017", "2018", "2019", 
                        "2020", "2021", "2022")
    timegroups_bracket_min <- c(1999, 2006, 2010, 2012, seq(2013, 2021)) + 0.5
    timegroups_bracket_max <- c(2006, 2010, 2012, seq(2013, 2022)) + 0.5
    plot_ytitle_margin <- margin(0, -0.6, 0, 0.4, "cm")
    plot_xlimits <- c(1999.5, 2024.5)
    plot_gridline_x <- 2023.3
    plot_baseline_lab <- "Pre-2000\nbaseline"
    plot_repel_nudge <- -1.5
    plot_xmin_minus <- 0.26
    
  } else if (cur_trend == "CAT") {
    
    timegroups_lab <- c("2015", "2016", "2017", "2018", "2019", "2020", "2021", "2022")
    timegroups_bracket_min <- seq(2015, 2021) + 0.5
    timegroups_bracket_max <- seq(2016, 2022) + 0.5
    plot_ytitle_margin <- margin(0, 0.6, 0, 0.4, "cm")
    plot_xlimits <- c(2015.09, 2023.1)
    plot_gridline_x <- 2022.7
    plot_baseline_lab <- "2015\nbaseline"
    plot_repel_nudge <- -0.57
    plot_xmin_minus <- 0.1
    
  }
  

  wrap_nchar <- 15 # number of characters to retain in one line while wrapping
  
  if (plot_type %in% c("single", "single_mask")) {
    
    # wrapping the labels for each trend line based on number of characters
    # we will be plotting the label as a geom, not as a label of a geom, so only need for first x value
    cur_data_trends <- cur_data_trends %>% 
      mutate(MASK.TITLE.WRAP = case_when(
        timegroupsf == timegroups_lab[2] ~ str_wrap(MASK.TITLE, width = wrap_nchar),
        TRUE ~ ""
      ))
    
    # for each species, arranging different (latest) mask trend values in desc. order
    # (mainly useful for mask comparison graphs)
    mask_order <- cur_data_trends %>%
      filter(timegroups == 2022) %>%
      arrange(desc(mean_std)) %>% 
      distinct(MASK, MASK.TITLE.WRAP)
    
    cur_data_trends <- cur_data_trends %>% 
      mutate(MASK = factor(MASK, levels = mask_order$MASK))
    
  } else if (plot_type == "multi") {

    # ordered as in the metadata (mainly for colour selection in graphs)
    cur_data_trends <- cur_data_trends %>% 
      mutate(COMMON.NAME = factor(COMMON.NAME, levels = cur_spec)) %>% 
      mutate(COMMON.NAME.WRAP = case_when(
        timegroupsf == timegroups_lab[2] ~ str_wrap(COMMON.NAME, width = wrap_nchar),
        TRUE ~ ""
        )) %>% 
      mutate(COMMON.NAME.WRAP = factor(COMMON.NAME.WRAP,
                                       levels = str_wrap(cur_spec, width = wrap_nchar)))
    
  } else if (plot_type == "composite") {
    
    # ordered as in the metadata (mainly for colour selection in graphs)
    cur_data_trends <- cur_data_trends %>% 
      mutate(GROUP = factor(GROUP, levels = cur_spec)) %>% 
      mutate(GROUP.WRAP = case_when(
        timegroupsf == timegroups_lab[2] ~ str_wrap(GROUP, width = wrap_nchar),
        TRUE ~ ""
      )) %>% 
      mutate(GROUP.WRAP = factor(GROUP.WRAP,
                                 levels = str_wrap(cur_spec, width = wrap_nchar)))
    
  }
  
  # determining limits for current plot -----------------------------------------------
  
  plot_xmin <- cur_data_trends %>%
    {if (plot_type != "composite") {
      distinct(., COMMON.NAME, timegroups) %>%
        arrange(., COMMON.NAME, timegroups) %>%
        group_by(., COMMON.NAME)
    } else {
      distinct(., GROUP, timegroups) %>%
        arrange(., GROUP, timegroups) %>%
        group_by(., GROUP)
    }} %>% 
    slice(2) %>% # because 1st is the baseline
    ungroup() %>%
    pull(timegroups) %>%
    max() # when multi-species, we take the latest year ### ###


  # saving non-rounded values for later use in plotting
  if (plot_type == "single") {
    plot_ymax0 <- cur_data_trends %>%
      filter(!is.na(rci_std)) %>%
      pull(rci_std) %>%
      max()
    
    plot_ymin0 <- cur_data_trends %>%
      filter(!is.na(lci_std)) %>%
      pull(rci_std) %>%
      min()
  } else if (plot_type == "single_mask") {
    plot_ymax0 <- cur_data_trends %>%
      mutate(max = case_when(MASK == "none" ~ mean_std,
                             MASK != "none" ~ rci_std)) %>% # mask will have CI band
      filter(!is.na(max)) %>%
      pull(max) %>%
      max()
    
    plot_ymin0 <- cur_data_trends %>%
      mutate(min = case_when(MASK == "none" ~ mean_std,
                             MASK != "none" ~ lci_std)) %>% # mask will have CI band
      filter(!is.na(min)) %>%
      pull(min) %>%
      min()
  } else {
    plot_ymax0 <- cur_data_trends %>%
      filter(!is.na(mean_std)) %>% # no CI band
      pull(mean_std) %>%
      max()
    
    plot_ymin0 <- cur_data_trends %>%
      filter(!is.na(mean_std)) %>% # no CI band
      pull(mean_std) %>%
      min()
  }

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
    {if (plot_type == "single_mask") {
      filter(., MASK != "none")
    } else {
      .
    }} %>% 
    {if (!(plot_type %in% c("multi", "composite"))) {
      mutate(., ref = case_when(
        
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
        
      ))
    } else {
      mutate(., ref = case_when(
        
        # stable/inconclusive
        mean_std <= 100 & mean_std >= 100 ~ 100,
        
        # increases
        mean_std > 100 & mean_std <= 125 ~ 125,
        mean_std > 125 & mean_std <= 150 ~ 125, # I
        mean_std > 150 & mean_std <= 200 ~ 150, # RI
        mean_std > 200 ~ 200, # RI+
        
        # declines
        mean_std < 100 & mean_std >= 75 ~ 75,
        mean_std < 75 & mean_std >= 50 ~ 75, # D
        mean_std < 50 ~ 50 # RD
        
      )) 
    }} %>%
    pull(ref)

  # Define a function to update breaks based on ref_line
  update_breaks <- function(breaks, ref_line) {
    abs_diff <- abs(breaks - ref_line)
    min_diff <- min(abs_diff)
    index <- which(abs_diff == min_diff)[1] # if draw between two, we take just one
    
    breaks[index] <- ref_line
    return(breaks)
  }
  
  # updating breaks based on each of 2022 trend values plotted (one in single, multiple in others)
  plot_ybreaks <- reduce(ref_line, update_breaks, .init = plot_ybreaks)
  
  plot_ybreaks_df <- data.frame(breaks = plot_ybreaks) %>%
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

  # creating the plot base based on plot type ------------------------------------------

  if (plot_type == "single_mask") {

    plot_base <- ggplot(cur_data_trends, 
                        aes(x = timegroups, y = mean_std, 
                            col = fct_inorder(MASK), fill = fct_inorder(MASK), 
                            label = MASK.TITLE.WRAP)) +
      # ribbon only for mask (not filter, cos we need colour palette to remain)
      geom_ribbon(data = cur_data_trends %>% 
                    mutate(across(c(lci_std, rci_std), 
                                  ~ if_else(MASK == "none", NA_real_, .))),
                  aes(ymin = lci_std, ymax = rci_std), 
                  colour = NA, linewidth = 0.7, alpha = 0.5) +
      # don't plot full country trend line if Inconclusive
      {if (plot_full_country == "Trend Inconclusive") {
        geom_line(data = cur_data_trends %>% filter(MASK != "none"),
                  linewidth = 1, lineend = "round")
      } else {
        geom_line(linewidth = 1, lineend = "round")
      }} +
      geom_text_repel(nudge_x = plot_repel_nudge, direction = "y", 
                      hjust = 0.5, size = 4, force_pull = 0,
                      xlim = c(plot_xlimits[1] - 0.1, plot_xmin - plot_xmin_minus),
                      family = plot_fontfamily, min.segment.length = Inf) +
      geom_point(size = 3) +
      scale_colour_manual(values = palette_trend_groups) +
      scale_fill_manual(values = palette_trend_groups) 
    
  } else if (plot_type == "single") {
    
    plot_base <- ggplot(cur_data_trends,
                        aes(x = timegroups, y = mean_std, 
                            ymin = lci_std, ymax = rci_std, 
                            col = fct_inorder(MASK), fill = fct_inorder(MASK))) +
      geom_ribbon(colour = NA, linewidth = 0.7, alpha = 0.5) +
      geom_line(linewidth = 1, lineend = "round") +
      geom_point(size = 3) +
      scale_colour_manual(values = palette_trend_groups) +
      scale_fill_manual(values = palette_trend_groups) 
    
  } else {
    
    plot_base <- {if (plot_type == "multi") {
      ggplot(cur_data_trends, 
             aes(x = timegroups, y = mean_std, col = COMMON.NAME, label = COMMON.NAME.WRAP))
      } else {
        ggplot(cur_data_trends, 
               aes(x = timegroups, y = mean_std, col = GROUP, label = GROUP.WRAP))
      }} +
      geom_line(linewidth = 1, lineend = "round") +
      geom_text_repel(nudge_x = plot_repel_nudge, direction = "y", 
                      hjust = 0.5, size = 4, force_pull = 0,
                      xlim = c(plot_xlimits[1] - 0.1, plot_xmin - plot_xmin_minus),
                      family = plot_fontfamily, min.segment.length = Inf) +
      geom_point(size = 3) +
      scale_colour_manual(values = palette_trend_groups) +
      scale_fill_manual(values = palette_trend_groups) 
    
  }
  
  # completing and writing the plot -----------------------------------------------------

  # joining plot base with other constant aesthetic features of graph

  cur_plot <- plot_base +
    # timegroup brackets
    geom_axisbracket("time") + 
    # "Current Trend" bracket
    {if (cur_trend != "CAT") {
      geom_axisbracket("trend", bracket_trend = cur_trend)
    }} +
    # manual grid lines with labels because we want empty space before the first timegroup
    geom_gridline(1) +
    geom_gridline(2) +
    geom_gridline(3) +
    geom_gridline(4) +
    geom_gridline(5) +
    geom_gridline(baseline = TRUE) +
    coord_cartesian(ylim = c(plot_ymin0 - 0.1 * plot_range_max,
                             plot_ymax0 + 0.1 * plot_range_max),
                    clip = "off") +
    scale_x_continuous(expand = c(0, 0), limits = plot_xlimits) +
    scale_y_continuous(expand = c(0, 0)) +
    # ggtitle(cur_spec) +
    labs(x = "Time-steps", y = "Change in Abundance Index") +
    guides(colour = "none", fill = "none") +
    # theme
    ggtheme_soibtrend()

  ggsave(filename = path_write_file, plot = cur_plot,
         dpi = 500, bg = "transparent",
         width = 11, height = 7.5, units = "in")
  
  if (plot_type %in% c("single", "single_mask") & cur_trend == "LTT") {
    ggsave(filename = path_write_file_web, plot = cur_plot,
           dpi = 500, bg = "transparent",
           width = 11, height = 7.5, units = "in")
  }

  # removing objects from global environment ------------------------------------------
  
  rm(list = names(obj_list), envir = .GlobalEnv)

}


# full ------------------------------------------------------------------------------

plot_soib_trends <- function(plot_type = "single", 
                             cur_trend = NULL, cur_spec = NULL) {
  
  # error checks ----------------------------------------------------------------------
  
  if (!plot_type %in% c("single", "single_mask", "multi", "composite")) {
    return("Select valid plot type!")
  }
  
  if (!(plot_type %in% c("multi", "composite"))) {
    
    if (is.null(cur_trend)) {
      return("Need to select which trend to plot!")
    } else if (!cur_trend %in% c("LTT", "CAT")) {
      return("Select valid trend!")
    }
    
    if (is.null(cur_spec)) {
      cur_spec <- "all"
      print("No species selected, plotting trends for all qualifying species.")
    }
    
  } else {

    if (!is.null(cur_trend) | !is.null(cur_trend)) {
      return("Specific trend types or species are not allowed for current plot type!")
    }
    
  }
  
  
  # setup -------------------------------------------------------
  
  require(tidyverse)
  require(ggpubr) # geom_bracket
  require(extrafont)
  require(glue)
  library(ggrepel) # text repel

  source('00_scripts/00_functions.R')
  source('00_scripts/plot_functions.R')
  
  load("00_data/analyses_metadata.RData")
  
  # assigning objects to environment --------------------------------------------------
  
  if (!(plot_type %in% c("multi", "composite"))) {
    obj_list <- list(plot_type = plot_type, 
                     cur_trend = cur_trend,
                     cur_spec = cur_spec,
                     analyses_metadata = analyses_metadata)
  } else {
    obj_list <- list(plot_type = plot_type, 
                     analyses_metadata = analyses_metadata)
  }
  
  list2env(obj_list, envir = .GlobalEnv)
  
  
  # import metadata and data, filter for qual. species, generate plots -----------------

  if (plot_type == "single") {
    
    plot_load_filter_data(plot_type, cur_trend)
    
    # generating plots if there are any qualified
    if (length(spec_qual) != 0) {
      
      if (cur_spec == "all") {
        walk(spec_qual, create_soib_trend_plot(plot_type = plot_type, 
                                               cur_trend = cur_trend, 
                                               cur_spec = spec_qual,
                                               data_trends = data_trends,
                                               data_main = data_main,
                                               path_write = path_write,
                                               cur_plot_metadata = web_metadata))
      } else {
        create_soib_trend_plot(plot_type = plot_type, 
                               cur_trend = cur_trend, 
                               cur_spec = cur_spec,
                               data_trends = data_trends,
                               data_main = data_main,
                               path_write = path_write,
                               cur_plot_metadata = web_metadata)
      }
      
    } else {
      
      print(glue("Skipping single-species plots for {cur_trend} (no qualified species)"))
      
    }
    
    rm(web_metadata, envir = .GlobalEnv)
    
  } else if (plot_type == "single_mask") {
    
    walk(analyses_metadata %>% 
           filter(MASK != "none") %>% 
           pull(MASK), ~ {
             
             plot_load_filter_data(plot_type, cur_trend, .x)
             
             # generating plots if there are any qualified
             if (length(spec_qual) != 0) {
               
               if (cur_spec == "all") {
                 walk(spec_qual, create_soib_trend_plot(plot_type = plot_type,
                                                        cur_trend = cur_trend,
                                                        cur_spec = spec_qual,
                                                        data_trends = data_trends,
                                                        data_main = data_main,
                                                        path_write = path_write,
                                                        cur_plot_metadata = web_metadata))
               } else {
                 create_soib_trend_plot(plot_type = plot_type,
                                        cur_trend = cur_trend,
                                        cur_spec = cur_spec,
                                        data_trends = data_trends,
                                        data_main = data_main,
                                        path_write = path_write,
                                        cur_plot_metadata = web_metadata)
               }
               
             } else {
               
               print(glue("Skipping mask-comparison plots for {.x} (no qualified species)"))
               
             }
             
             rm(web_metadata, envir = .GlobalEnv)
             
           })
    
  } else if (plot_type == "multi") {
    
    plot_metadata <- fetch_plot_metadata(plot_type)
    
    walk(plot_metadata %>% pull(PLOT.NO), ~ {
      
      cur_plot_metadata <- plot_metadata %>%
        filter(PLOT.NO == .x) %>% 
        mutate(PLOT.SPEC = str_split(PLOT.SPEC, ", ")) %>%
        unnest(PLOT.SPEC)
      
      cur_spec <- cur_plot_metadata %>% pull(PLOT.SPEC)
      cur_plot_metadata <- cur_plot_metadata %>% mutate(PLOT.SPEC = NULL) %>% distinct()
      cur_trend <- cur_plot_metadata %>% pull(TREND)
      
      plot_load_filter_data(fn_plot_type = plot_type, fn_cur_trend = cur_trend)

      create_soib_trend_plot(plot_type = plot_type,
                             cur_trend = cur_trend,
                             cur_spec = cur_spec,
                             data_trends = data_trends,
                             data_main = data_main,
                             path_write = path_write,
                             cur_plot_metadata = cur_plot_metadata)

    })
    
  } else if (plot_type == "composite") {
    
    plot_metadata <- fetch_plot_metadata(plot_type)
    
    # initialise composite summary 
    init_summary <- plot_metadata %>% 
      distinct(PLOT.NO) %>% 
      rename(COMPOSITE.NO = PLOT.NO)
    
    walk(plot_metadata %>% 
           distinct(PLOT.NO) %>% 
           pull(PLOT.NO), ~ {
      
      cur_trend <- "LTT"
      
      cur_plot_metadata <- plot_metadata %>% 
        filter(PLOT.NO == .x) %>% 
        # for each plot, we only want relevant columns to join
        dplyr::select(where(~ !any(is.na(.)))) 
      
      
      plot_load_filter_data(plot_type, cur_trend, "none")
      
      cur_plot_metadata <- cur_plot_metadata %>% 
        # joining species or species groups for current composite
        left_join(data_main) %>% 
        dplyr::select(starts_with("PLOT."), FILE.NAME, GROUP,
                      eBird.English.Name.2022, SOIBv2.Priority.Status) %>% 
        filter(eBird.English.Name.2022 %in% spec_qual) %>% 
        rename(PLOT.SPEC = eBird.English.Name.2022)
      
      # saving summary of current composite groups
      cur_summary <- init_summary %>% filter(COMPOSITE.NO == .x)
      
      if (exists("full_summary", envir = .GlobalEnv)) {
        full_summary <- bind_rows(full_summary,
                                  create_composite_summary(cur_plot_metadata, cur_summary))
      } else {
        full_summary <- create_composite_summary(cur_plot_metadata, cur_summary)
      }
      assign("full_summary", full_summary, envir = .GlobalEnv)
      
      # summarising trends for groups (from individual species)
      data_trends <- cur_plot_metadata %>%
        left_join(data_trends, by = c("PLOT.SPEC" = "COMMON.NAME")) %>%
        dplyr::select(starts_with("PLOT."), FILE.NAME, GROUP,
                      timegroups, timegroupsf, lci_std, mean_std, rci_std) %>%
        # get trends per group
        group_by(GROUP, timegroups, timegroupsf) %>%
        reframe(across(ends_with("_std"), ~ mean(.)))

      # to order factor levels later (here "spec" is actually "group")
      cur_spec <- data_trends %>% distinct(GROUP) %>% pull(GROUP)

      cur_plot_metadata <- cur_plot_metadata %>%
        mutate(GROUP = NULL, PLOT.SPEC = NULL) %>%
        distinct()

      create_soib_trend_plot(plot_type = plot_type,
                             cur_trend = cur_trend,
                             cur_spec = cur_spec,
                             data_trends = data_trends,
                             data_main = data_main,
                             path_write = path_write,
                             cur_plot_metadata = cur_plot_metadata)

    })
    
    write.csv(full_summary, row.names = FALSE,
              file = "02_graphs/03_composite/00_composites_summary.csv")
    
    rm(full_summary, envir = .GlobalEnv)
    
  }
  
  # removing objects from global environment (from import step) ------------------------

  rm(list = c(names(obj_list), "spec_qual", "data_trends", "data_main", "path_write"), 
     envir = .GlobalEnv)

}

