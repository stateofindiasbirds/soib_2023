
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
    ann_fontface <- 3
    
  } else {
    
    line_y <- plot_ybreaks[index_y]
    line_linetype <- "dotted"
    line_linewidth <- 0.7
    
    # label on y-axis
    ann_size <- 6
    ann_lab <- plot_ybreaks_lab[index_y]
    ann_fontface <- 1
  
  }
  
  # provide both the grid line and its label
  list(
    geom_segment(x = plot_xmin, xend = 2022, col = palette_plot_elem, # constant 
                 y = line_y, yend = line_y, 
                 linetype = line_linetype, linewidth = line_linewidth),
    
    annotate("text", x = plot_gridline_x, # constant
             colour = palette_plot_elem, family = plot_fontfamily, # constant
             y = line_y, label = ann_lab, size = ann_size, fontface = ann_fontface)
  )
  
}


# x-axis brackets -------------------------------------------------------------------

geom_axisbracket <- function(bracket_type = "time", bracket_trend = cur_trend) {

  if (bracket_type == "time") {

  bracket_min <- timegroups_bracket_min
  bracket_max <- timegroups_bracket_max
  bracket_ypos <- plot_ymin0 - 0.01 * plot_range_max
  bracket_lab <- timegroups_lab[-1]
  bracket_tiplength <- 0
  bracket_labelsize <- 3.8
  bracket_fontface <- 1
  bracket_vjust <- 2.5
  
  } else if (bracket_type == "trend") {
    
    if (bracket_trend == "CAT") {
      return("Current Trend bracket should not be plotted in the CAT plot!")
    }
    
    bracket_min <- 2015 - 0.5
    bracket_max <- 2022 + 0.5
    bracket_ypos <- plot_ymin0 - 0.065 * plot_range_max
    bracket_lab <- "Current Trend"
    bracket_tiplength <- -0.017
    bracket_labelsize <- 5
    bracket_fontface <- 3
    bracket_vjust <- 2.7
    
  }
  
  geom_bracket(inherit.aes = FALSE, bracket.shorten = 0.15, # constant
               family = plot_fontfamily, col = palette_plot_elem, # constant
               xmin = bracket_min, xmax = bracket_max, y.position = bracket_ypos,
               label = bracket_lab, label.size = bracket_labelsize, fontface = bracket_fontface,
               tip.length = bracket_tiplength, vjust = bracket_vjust)
  
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


# import necessary data for a given mask  ---------------------------------------------

# read data and add 'Mask' column
plot_import_data <- function(mask, import_trend = fn_cur_trend) {
  
  cur_metadata <- analyses_metadata %>% filter(MASK == mask)
  
  # function to add mask titles/labels
  add_mask_titles <- function(data, which_mask) {
    data %>% 
      mutate(MASK = which_mask,
             MASK.TITLE = case_when(
               which_mask == "none"      ~ "Full country",
               which_mask == "woodland"  ~ "Woodland grids",
               which_mask == "cropland"  ~ "Cropland grids",
               which_mask == "ONEland"   ~ "ONE grids",
               which_mask == "PA"        ~ "Protected areas",
               TRUE                      ~ which_mask # states
             )) %>% 
      mutate(MASK = factor(MASK, levels = levels(analyses_metadata$MASK)),
             MASK.TITLE = factor(MASK.TITLE, 
                                 levels = analyses_metadata %>% 
                                   mutate(MASK.TITLE = case_when(
                                     MASK == "none"      ~ "Full country",
                                     MASK == "woodland"  ~ "Woodland grids",
                                     MASK == "cropland"  ~ "Cropland grids",
                                     MASK == "ONEland"   ~ "ONE grids",
                                     MASK == "PA"        ~ "Protected areas",
                                     TRUE                ~ MASK # states
                                   )) %>% 
                                   pull(MASK.TITLE)))
  }
  
  # to catch if main/trends file does not exist
  if (!(file.exists(cur_metadata$SOIBMAIN.PATH) & file.exists(cur_metadata$TRENDS.OUTPATH))) {
    
    print(glue("Data file(s) for {mask} missing."))
    return(NULL)
    
  } else {
    
    # load data ---------------------------------------------------------------

    data_main <- read.csv(cur_metadata$SOIBMAIN.PATH) %>%
      add_mask_titles(mask)
    
    data_trends <- read.csv(cur_metadata$TRENDS.OUTPATH) %>%
      add_mask_titles(mask)
    
    # filtering for qualified species ---------------------------------------------------
    
    # - not plotting data deficient; inconclusive will be plotted in single-species
    # - only species sel. for that trend; 
    # - only till MY 2022
    
    if (import_trend == "LTT") {
      
      spec_qual <- data_main %>% 
        filter(!(SOIBv2.Long.Term.Status %in% c("Insufficient Data")),
               Long.Term.Analysis == "X") %>% 
        dplyr::select(eBird.English.Name.2022) %>% 
        add_mask_titles(mask)
      
      data_trends <- data_trends %>% 
        filter(COMMON.NAME %in% spec_qual$eBird.English.Name.2022,
               timegroups <= 2022)
      
    } else if (import_trend == "CAT") {
      
      spec_qual <- data_main %>% 
        filter(!(SOIBv2.Current.Status %in% c("Insufficient Data")),
               Current.Analysis == "X") %>% 
        dplyr::select(eBird.English.Name.2022) %>% 
      add_mask_titles(mask)
      
      data_trends <- data_trends %>% 
        filter(COMMON.NAME %in% spec_qual$eBird.English.Name.2022,
               timegroups >= 2015 & timegroups <= 2022)
      
    }


    # return ----------------------------------------------------------------------------

    return(list(spec_qual = spec_qual, data_trends = data_trends, data_main = data_main))

  }
  
}

# load appropriate data and filter for species qualified for plotting ------------------

plot_load_filter_data <- function(fn_plot_type, fn_cur_trend, fn_cur_mask) {
  
  # metadata and paths --------------------------------------------------

  if (fn_plot_type == "single") {
    
    cur_metadata <- analyses_metadata %>% 
      filter(MASK == "none") %>% 
      mutate(CUR.OUT.PATH = PLOT.SINGLE.FOLDER) # path (folder) to write to
    
  } else if (fn_plot_type == "multi") {
    
    cur_metadata <- analyses_metadata %>%
      filter(MASK == "none") %>% 
      mutate(CUR.OUT.PATH = PLOT.MULTI.FOLDER) # path (folder) to write to
    
  } else if (fn_plot_type == "single_mask") {
    
    # process mask data
    cur_metadata <- analyses_metadata %>% 
      filter(MASK %in% c("none", as.character(fn_cur_mask))) %>% 
      mutate(CUR.OUT.PATH = PLOT.SINGLE.FOLDER) # path (folder) to write to

  } else if (fn_plot_type == "composite") {

    cur_metadata <- analyses_metadata %>% 
      filter(MASK %in% c("none", as.character(fn_cur_mask))) %>% 
      mutate(CUR.OUT.PATH = PLOT.COMPOSITE.FOLDER) # path (folder) to write to
    
  }
  
  
  path_write <- cur_metadata %>% 
    {if (!(fn_plot_type %in% c("multi", "composite"))) {
      mutate(., PLOT.OUTPATH = case_when(fn_cur_trend == "LTT" ~ glue("{CUR.OUT.PATH}long-term trends/"),
                                         fn_cur_trend == "CAT" ~ glue("{CUR.OUT.PATH}current trends/")))
    } else {
      mutate(., PLOT.OUTPATH = CUR.OUT.PATH)
    }} %>% 
    # for mask comparisons, don't output full country plot
    {if (fn_plot_type == "single_mask") {
      filter(., MASK != "none")
    } else {
      .
    }} %>% 
    pull(PLOT.OUTPATH) %>% 
    unique()

  # create path(s) if doesn't exist
  walk(path_write, ~ {
    if (!dir.exists(.x)) {dir.create(.x, recursive = TRUE)}
  })
  
  # load data ---------------------------------------------------------------
  
  # importing appropriate data filtered for qualified species

  data_processed <- map(cur_metadata$MASK, ~ plot_import_data(., import_trend = fn_cur_trend)) %>% 
    # remove NULL elements, which are masks whose data file(s) are missing
    purrr::compact() 
  
  data_main <- map(data_processed, pluck, "data_main") %>% bind_rows()
  data_trends <- map(data_processed, pluck, "data_trends") %>% bind_rows()

  # for mask comparison, even though we have filtered each mask's trends for its qual. spp.,
  # we want an additional filter so that comparisons are only made for those species in masks
  # that are also there in full-country data
  
  if (fn_plot_type == "single_mask" |
      (fn_plot_type == "composite" & fn_cur_mask != "none")) {
    
    spec_qual <- map(data_processed, pluck, "spec_qual") %>% bind_rows()
    
    spec_qual_country <- spec_qual %>% filter(MASK == "none") %>% pull(eBird.English.Name.2022)
    spec_qual_masks <- spec_qual %>% filter(MASK != "none") %>% pull(eBird.English.Name.2022)
    
    spec_qual <- intersect(spec_qual_country, spec_qual_masks)
    
    data_trends <- data_trends %>% filter(COMMON.NAME %in% spec_qual)
    data_main <- data_main %>% filter(eBird.English.Name.2022 %in% spec_qual)
    
  } else {
    
    spec_qual <- map(data_processed, pluck, "spec_qual") %>% 
      bind_rows() %>% 
      pull(eBird.English.Name.2022)
    
  }


  # assigning necessary objects to global environment ---------------------------------
  
  obj_list2 <- list(spec_qual = spec_qual, data_trends = data_trends,
                    data_main = data_main, path_write = path_write)
  
  list2env(obj_list2, envir = .GlobalEnv)
  

}

# fetch plot metadata for multi and composite graphs --------------------------------

fetch_plot_metadata <- function(plot_type) {
  
  if (plot_type == "multi") {
    
    metadata_LTT <- data.frame(
      
      PLOT.SPEC = c(
        str_flatten_comma(c("Isabelline Wheatear","Great Gray Shrike","Rufous-tailed Lark",
                            "Yellow-billed Babbler","Eurasian Kestrel","Jerdon's Bushlark")),
        str_flatten_comma(c("Little Ringed Plover","Little Tern","Great Thick-knee","Small Pratincole")),
        str_flatten_comma(c("Lesser Sand-Plover","Terek Sandpiper","Whimbrel","Curlew Sandpiper",
                            "Eurasian Curlew")),
        str_flatten_comma(c("Oriental Honey-buzzard","Black Kite","Eurasian Marsh-Harrier",
                            "Short-toed Snake-Eagle","Pallid Harrier","Greater Spotted Eagle")),
        str_flatten_comma(c("White-rumped Vulture","Indian Vulture","Red-headed Vulture",
                            "Egyptian Vulture","Eurasian Griffon")),
        str_flatten_comma(c("Indian Gray Hornbill","Oriental Pied-Hornbill")),
        str_flatten_comma(c("Spot-billed Pelican","Black-headed Ibis","Purple Heron","Painted Stork",
                            "Eurasian Spoonbill","Glossy Ibis")),
        str_flatten_comma(c("Black-rumped Flameback","Lesser Yellownape","White-bellied Woodpecker",
                            "Yellow-crowned Woodpecker","Brown-capped Pygmy Woodpecker",
                            "Himalayan Woodpecker")),
        str_flatten_comma(c("Northern Shoveler","Garganey","Cotton Pygmy-Goose",
                            "Common Merganser","Ruddy Shelduck","Tufted Duck")),
        str_flatten_comma(c("Ashy Prinia","Rock Pigeon","Indian Peafowl","Asian Koel")),
        str_flatten_comma(c("Large-billed Crow","Olive-backed Pipit","Greater Coucal"))
        
      )) %>% 
      mutate(PLOT.NAME = c("Open Ecosystems", "Rivers", "Coasts", "Raptors", "Vultures",
                           "Hornbills", "Large Waterbirds", "Woodpeckers", "Ducks", "Thriving Birds",
                           "Methods_Comparison"),
             TREND = "LTT")
    
    metadata_CAT <- data.frame(
      PLOT.SPEC = c(
        str_flatten_comma(c("White-rumped Vulture","Indian Vulture","Red-headed Vulture",
                            "Egyptian Vulture")),
        str_flatten_comma(c("Spot-billed Pelican","Black-headed Ibis","Purple Heron","Painted Stork",
                            "Eurasian Spoonbill","Oriental Darter"))
      )) %>% 
      mutate(PLOT.NAME = c("Vultures", "Large Waterbirds"),
             TREND = "CAT") 
    
    plot_metadata <- metadata_LTT %>% 
      bind_rows(metadata_CAT) %>% 
      mutate(PLOT.NAME = factor(PLOT.NAME, levels = 
                                  c("Open Ecosystems", "Rivers", "Coasts", "Raptors", "Vultures",
                                    "Hornbills", "Large Waterbirds", "Woodpeckers", "Ducks", "Thriving Birds",
                                    "Methods_Comparison"))) %>% 
      arrange(PLOT.NAME) %>% 
      mutate(PLOT.NO = 1:n() %>% str_pad(width = 2, pad = 0),
             PLOT.NAME.MOD = str_replace_all(PLOT.NAME, " ", "-")) %>% 
      mutate(FILE.NAME = glue("{PLOT.NO}_{PLOT.NAME.MOD}_{TREND}"))
    
    return(plot_metadata)
    
  } else if (plot_type == "composite") {
    
    cur_trend <- "LTT"
    data_processed <- plot_import_data("none", cur_trend)
    data_main <- data_processed %>% pluck("data_main") %>% bind_rows()


    data1 <- data_main %>% 
      mutate(GROUP = case_when(Diet.Guild == "" ~ NA_character_, 
                               Diet.Guild == "Fruit & Nect" ~ "Fruit & Nectar",
                               TRUE ~ Diet.Guild)) %>% 
      filter(!is.na(GROUP)) %>% 
      distinct(GROUP, Diet.Guild) %>% 
      mutate(PLOT.NAME = "Diet Guilds")
    
    data2 <- data_main %>% 
      mutate(GROUP = case_when(
        Habitat.Specialization == "Grassland" ~ "Grassland & Scrub",
        Habitat.Specialization == "Alpine & Cold Desert" ~ "Open Habitat",
        Habitat.Specialization == "None" ~ "No Specialisation",
        TRUE ~ Habitat.Specialization
      )) %>% 
      filter(!is.na(GROUP)) %>% 
      distinct(GROUP, Habitat.Specialization) %>% 
      mutate(PLOT.NAME = "Habitat Specialisations")
    
    data3 <- data_main %>% 
      mutate(GROUP = case_when(
        Endemic.Region == "Andaman and Nicobar Islands" ~ NA_character_, 
        str_ends(Endemic.Region, "Himalayas") ~ "Himalaya",
        # Endemic.Region %in% c("Eastern Himalayas", "Western Himalayas") ~ "Himalaya",
        Endemic.Region == "Western Ghats" ~ "Western Ghats & Sri Lanka",
        Endemic.Region == "Mainland India" ~ "Indian Subcontinent",
        Endemic.Region == "Western Ghats" ~ "Western Ghats & Sri Lanka",
        Endemic.Region == "None" ~ "Non-endemic",
        TRUE ~ Endemic.Region
      )) %>% 
      filter(!is.na(GROUP)) %>% # A&N removed
      distinct(GROUP, Endemic.Region) %>% 
      mutate(PLOT.NAME = "Endemic Regions")
    
    
    # shorebirds composite
    data4 <- data_main %>% 
      mutate(GROUP = fct_collapse(
        
        eBird.English.Name.2022,
        
        "Near Resident or Palearctic Migrant" = c(
          "Indian Thick-knee","Great Thick-knee","Beach Thick-knee","Black-winged Stilt",
          "Red-wattled Lapwing","Little Ringed Plover","Greater Painted-Snipe",
          "Pheasant-tailed Jacana","Bronze-winged Jacana","Solitary Snipe",
          "Barred Buttonquail","Indian Courser","Jerdon's Courser","Small Pratincole"
        ),
        
        "Near Resident or Palearctic Migrant" = c(
          "Pied Avocet","Ibisbill","Eurasian Oystercatcher","Northern Lapwing",
          "Gray-headed Lapwing","Sociable Lapwing","White-tailed Lapwing",
          "Lesser Sand-Plover","Greater Sand-Plover","Caspian Plover",
          "Kentish Plover","Common Ringed Plover","Long-billed Plover",
          "Oriental Plover","Eurasian Curlew","Black-tailed Godwit","Ruff",
          "Jack Snipe","Eurasian Woodcock","Wood Snipe","Great Snipe","Common Snipe",
          "Pin-tailed Snipe","Swinhoe's Snipe","Common Sandpiper","Green Sandpiper",
          "Spotted Redshank","Common Greenshank","Nordmann's Greenshank",
          "Marsh Sandpiper","Wood Sandpiper","Common Redshank","Small Buttonquail",
          "Yellow-legged Buttonquail","Crab-Plover","Cream-colored Courser",
          "Collared Pratincole","Oriental Pratincole"
        ),
        
        "Arctic Migrant" = c(
          "Black-bellied Plover","European Golden-Plover","American Golden-Plover",
          "Pacific Golden-Plover","Whimbrel","Bar-tailed Godwit","Ruddy Turnstone",
          "Great Knot","Red Knot","Broad-billed Sandpiper","Sharp-tailed Sandpiper",
          "Curlew Sandpiper","Temminck's Stint","Long-toed Stint",
          "Spoon-billed Sandpiper","Red-necked Stint","Sanderling","Dunlin",
          "Little Stint","Buff-breasted Sandpiper","Pectoral Sandpiper",
          "Asian Dowitcher","Long-billed Dowitcher","Terek Sandpiper",
          "Red-necked Phalarope","Red Phalarope","Gray-tailed Tattler"
        )
        
      )) %>% 
      mutate(GROUP = case_when(!GROUP %in% c("Near Resident or Palearctic Migrant",
                                             "Arctic Migrant") ~ NA_character_,
                               TRUE ~ GROUP)) %>% 
      filter(!is.na(GROUP)) %>% 
      distinct(GROUP, eBird.English.Name.2022) %>% 
      mutate(PLOT.NAME = "Shorebird Migratory Behaviours")
    
    data5 <- data_main %>% 
      filter(Order %in% c("Accipitriformes", "Falconiformes")) %>% 
      mutate(GROUP = case_when(
        Habitat.Specialization %in% c("Grassland", "Wetland", 
                                      "Alpine & Cold Desert") ~ "Open Habitat",
        Habitat.Specialization == "Forest" ~ "Forest & Plantation",
        Habitat.Specialization == "None" ~ "No Specialisation",
        TRUE ~ Habitat.Specialization
      )) %>% 
      filter(!is.na(GROUP)) %>% 
      distinct(GROUP, Habitat.Specialization) %>% 
      mutate(PLOT.NAME = "Raptor Habitat Specialisations")

    
    # mask composites
    
    plot_load_filter_data(plot_type, cur_trend, "woodland")
    
    data6 <- data_main %>% 
      rename(GROUP = MASK.TITLE) %>% 
      filter(!is.na(GROUP)) %>% 
      distinct(GROUP, eBird.English.Name.2022) %>% 
      mutate(PLOT.NAME = "Woodland")
    
    
    plot_load_filter_data(plot_type, cur_trend, c("ONEland", "cropland"))
    
    data7 <- data_main %>% 
      rename(GROUP = MASK.TITLE) %>% 
      filter(!is.na(GROUP)) %>% 
      distinct(GROUP, eBird.English.Name.2022) %>% 
      mutate(PLOT.NAME = "Open Natural Ecosystems & Cropland")

    
    plot_load_filter_data(plot_type, cur_trend, "PA")
    
    data8 <- data_main %>% 
      rename(GROUP = MASK.TITLE) %>% 
      filter(!is.na(GROUP)) %>% 
      distinct(GROUP, eBird.English.Name.2022) %>% 
      mutate(PLOT.NAME = "Protected Areas")
    
    
    
    plot_metadata <- bind_rows(data1, data2, data3, data4, data5, data6, data7, data8) %>% 
      mutate(PLOT.NAME = factor(PLOT.NAME, levels = c(
        "Diet Guilds", "Habitat Specialisations", "Endemic Regions",
        "Shorebird Migratory Behaviours", "Raptor Habitat Specialisations",
        "Woodland", "Open Natural Ecosystems & Cropland", "Protected Areas"
      ))) %>% 
      mutate(PLOT.NO = as.numeric(PLOT.NAME) %>% str_pad(width = 2, pad = 0),
             # LTT for all composites
             TREND = "LTT") %>% 
      mutate(PLOT.NAME.MOD = str_replace_all(PLOT.NAME, " ", "-"),
             FILE.NAME = glue("{PLOT.NO}_{PLOT.NAME.MOD}_{TREND}")) %>% 
      relocate(PLOT.NO, PLOT.NAME, PLOT.NAME.MOD, FILE.NAME) %>% 
      # as of latest, we don't use the habitat-mask composites
      filter(!PLOT.NAME %in% c("Woodland", "Open Natural Ecosystems & Cropland", "Protected Areas"))
    
    
    
    rm(list = c("spec_qual", "data_trends", "data_main", "path_write"), 
       envir = .GlobalEnv)
    

    return(plot_metadata)
    
  }
  
}
