
# reference grid lines --------------------------------------------------------------

# we have to manually create grid lines because we want empty space before t=1

geom_gridline <- function(index_y = NULL, baseline = FALSE) {
  
  if (baseline) {
    
    line_y <- 100
    line_linetype <- "solid"
    line_linewidth <- 0.9
    
    # label on y-axis
    ann_size <- 5
    ann_lab <- plot_baseline_lab
    
  } else {
    
    line_y <- plot_ybreaks[index_y]
    line_linetype <- "dotted"
    line_linewidth <- 0.7
    
    # label on y-axis
    ann_size <- 6
    ann_lab <- plot_ybreaks_lab[index_y]
  
  }
  
  # provide both the grid line and its label
  list(
    geom_segment(x = plot_xmin, xend = 2022, col = palette_plot_elem, # constant 
                 y = line_y, yend = line_y, 
                 linetype = line_linetype, linewidth = line_linewidth),
    
    annotate("text", x = plot_gridline_x, # constant
             colour = palette_plot_elem, family = plot_fontfamily, # constant
             y = line_y, label = ann_lab, size = ann_size)
  )
  
}


# x-axis brackets -------------------------------------------------------------------

geom_axisbracket <- function(bracket_type = "time") {

  if (bracket_type == "time") {

  bracket_min <- timegroups_bracket_min
  bracket_max <- timegroups_bracket_max
  bracket_ypos <- plot_ymin0 - 0.01 * plot_range_max
  bracket_lab <- timegroups_lab[-1]
  bracket_tiplength <- 0.03 # CAT 0.04? ###
  bracket_vjust <- 2.5 # CAT 3?
  
  } else if (bracket_type == "trend") {
    
    if (cur_trend == "CAT") {
      return("Current Trend bracket should not be plotted in the CAT plot!")
    }
    
    bracket_min <- 2015 - 0.5
    bracket_max <- 2022 + 0.5
    bracket_ypos <- plot_ymin0 - 0.05 * plot_range_max
    bracket_lab <- "Current Trend"
    bracket_tiplength <- 0.02
    bracket_vjust <- 2.1
    
  }
  
  geom_bracket(inherit.aes = FALSE, bracket.shorten = 0.15, # constant
               label.size = 3, col = palette_plot_elem, # constant
               xmin = bracket_min, xmax = bracket_max, y.position = bracket_ypos,
               label = bracket_lab, tip.length = bracket_tiplength, vjust = bracket_vjust)
  
}


# SoIB trend plot theme -------------------------------------------------------------

ggtheme_soibtrend <- function() {
  
  theme_void() +
    theme(axis.title.y = element_text(size = 22, colour = palette_plot_elem,
                                      angle = 90, margin = plot_ytitle_margin),
          plot.margin = unit(c(0, 0, 0, 0), "cm"),
          plot.title = element_text(face = "bold", size = 20, hjust = 0.5, vjust = -2, 
                                    colour = palette_plot_title),
          text = element_text(family = plot_fontfamily),
          plot.background = element_rect(fill = "transparent", colour = NA),
          panel.background = element_rect(fill = "transparent", colour = NA))
  
}


# read necessary data for a given mask  ---------------------------------------------

# read data and add 'Mask' column
plot_import_data <- function(mask) {
  
  cur_metadata <- analyses_metadata %>% filter(MASK == mask)
  
  # to catch if main/trends file does not exist
  if (!(file.exists(cur_metadata$SOIBMAIN.PATH) & file.exists(cur_metadata$TRENDS.OUTPATH))) {
    
    print(glue("Data file(s) for {mask} missing."))
    return(NULL)
    
  } else {
    
    # load data ---------------------------------------------------------------

    data_main <- read.csv(cur_metadata$SOIBMAIN.PATH) %>%
      mutate(MASK = mask,
             MASK.TITLE = case_when(
               mask == "none"      ~ "Country as a whole",
               mask == "woodland"  ~ "Grids with threshold woodland",
               mask == "PA"        ~ "Protected areas",
               mask == "cropland"  ~ "Grids with threshold cropland",
               mask == "ONEland"   ~ "Grids with threshold ONEs"
             ))
    
    data_trends <- read.csv(cur_metadata$TRENDS.OUTPATH) %>%
      mutate(MASK = mask,
             MASK.TITLE = case_when(
               mask == "none"      ~ "Country as a whole",
               mask == "woodland"  ~ "Grids with threshold woodland",
               mask == "PA"        ~ "Protected areas",
               mask == "cropland"  ~ "Grids with threshold cropland",
               mask == "ONEland"   ~ "Grids with threshold ONEs"
             ))
    
    # filtering for qualified species ---------------------------------------------------
    
    # - not plotting inconclusive or data deficient; 
    # - only species sel. for that trend; 
    # - only till MY 2022
    
    if (cur_trend == "LTT") {
      
      spec_qual <- data_main %>% 
        filter(!(SOIBv2.Long.Term.Status %in% c("eBird Data Inconclusive",
                                                "eBird Data Deficient")),
               Long.Term.Analysis == "X") %>% 
        dplyr::select(eBird.English.Name.2022) %>% 
        mutate(MASK = mask,
               MASK.TITLE = case_when(
                 mask == "none"      ~ "Country as a whole",
                 mask == "woodland"  ~ "Grids with threshold woodland",
                 mask == "PA"        ~ "Protected areas",
                 mask == "cropland"  ~ "Grids with threshold cropland",
                 mask == "ONEland"   ~ "Grids with threshold ONEs"
               ))
      
      data_trends <- data_trends %>% 
        filter(COMMON.NAME %in% spec_qual$eBird.English.Name.2022,
               timegroups <= 2022)
      
    } else if (cur_trend == "CAT") {
      
      spec_qual <- data_main %>% 
        filter(!(SOIBv2.Current.Status %in% c("eBird Data Indecisive",
                                              "eBird Data Deficient")),
               Current.Analysis == "X") %>% 
        dplyr::select(eBird.English.Name.2022) %>% 
        mutate(MASK = mask,
               MASK.TITLE = case_when(
                 mask == "none"      ~ "Country as a whole",
                 mask == "woodland"  ~ "Grids with threshold woodland",
                 mask == "PA"        ~ "Protected areas",
                 mask == "cropland"  ~ "Grids with threshold cropland",
                 mask == "ONEland"   ~ "Grids with threshold ONEs"
               ))
      
      data_trends <- data_trends %>% 
        filter(COMMON.NAME %in% spec_qual$eBird.English.Name.2022,
               timegroups >= 2015 & timegroups <= 2022)
      
    }

    # return ----------------------------------------------------------------------------

    return(list(spec_qual = spec_qual, data_trends = data_trends))

  }
  
}

# load appropriate data and filter for species qualified for plotting ------------------

plot_load_filter_data <- function(plot_type, cur_trend) {
  
  # metadata and paths --------------------------------------------------
  
  if (plot_type == "single") {
    
    cur_metadata <- analyses_metadata %>% 
      filter(MASK == "none") %>% 
      mutate(CUR.OUT.PATH = PLOT.SINGLE.FOLDER) # path (folder) to write to
    
  } else if (plot_type == "single_mask") {
    
    # process all masks' data
    cur_metadata <- analyses_metadata %>% 
      mutate(CUR.OUT.PATH = PLOT.SINGLE.MASKS.FOLDER) # path (folder) to write to
  #   
  # } else if (plot_type == "multi") {
  #   
  #   cur_metadata <- analyses_metadata %>% 
  #     mutate(CUR.OUT.PATH = PLOT.MULTI.FOLDER) # path (folder) to write to
  #   
  # } else if (plot_type == "composite") {
  #   
  #   cur_metadata <- analyses_metadata %>% 
  #     mutate(CUR.OUT.PATH = PLOT.COMPOSITE.FOLDER) # path (folder) to write to
    
  }

  # long-term or current trend?
  if (cur_trend == "LTT") {
    path_write <- cur_metadata %>% 
      mutate(PLOT.OUTPATH = glue("{CUR.OUT.PATH}long-term trends/")) %>% 
      pull(PLOT.OUTPATH)
  } else if (cur_trend == "CAT") {
    path_write <- cur_metadata %>% 
      mutate(PLOT.OUTPATH = glue("{CUR.OUT.PATH}current trends/")) %>% 
      pull(PLOT.OUTPATH)
  }
  
  # create path(s) if doesn't exist
  walk(path_write, ~ {
    if (!dir.exists(.x)) {dir.create(.x, recursive = TRUE)}
  })
  
  # load data ---------------------------------------------------------------
  
  # importing appropriate data filtered for qualified species
  
  data_processed <- map(cur_metadata$MASK, plot_import_data) %>% 
    # remove NULL elements, which are masks whose data file(s) are missing
    purrr::compact() 
  
  data_trends <- map(data_processed, pluck, "data_trends") %>% bind_rows()

  # for mask comparison, even though we have filtered each mask's trends for its qual. spp.,
  # we want an additional filter so that comparisons are only made for those species in masks
  # that are also there in full-country data
  
  if (plot_type == "single_mask") {
    
    spec_qual <- map(data_processed, pluck, "spec_qual") %>% bind_rows()
    
    spec_qual_country <- spec_qual %>% filter(MASK == "none") %>% pull(eBird.English.Name.2022)
    spec_qual_masks <- spec_qual %>% filter(MASK != "none") %>% pull(eBird.English.Name.2022)
    
    spec_qual <- intersect(spec_qual_country, spec_qual_masks)
    
    data_trends <- data_trends %>% filter(COMMON.NAME %in% spec_qual)

  } else {
    
    spec_qual <- map(data_processed, pluck, "spec_qual") %>% bind_rows() %>% pull(eBird.English.Name.2022)
    
  }
  

  # assigning necessary objects to global environment ---------------------------------

  obj_list <- list(spec_qual = spec_qual, data_trends = data_trends, path_write = path_write)
  
  list2env(obj_list, envir = .GlobalEnv)
  
  
}
