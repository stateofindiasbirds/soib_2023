
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
plot_import_data <- function(mask, import_trend = fn_cur_trend, import_plot_type = fn_plot_type) {
  
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
    
    # inconclusive should only be plotted when we have CI bands
    # since single-mask has band only for mask, we remove Inconclusive for country in plotting step
    # but for multispecies and composites we don't have CI bands, so remove here itself
    status_to_filter <- if (import_plot_type %in% c("single", "single_mask")) {
      c("Insufficient Data") 
    } else {
      c("Insufficient Data", "Trend Inconclusive") 
    }
    
    if (import_trend == "LTT") {
      
      spec_qual <- data_main %>% 
        filter(!(SOIBv2.Long.Term.Status %in% status_to_filter),
               Long.Term.Analysis == "X") %>% 
        dplyr::select(eBird.English.Name.2022) %>% 
        add_mask_titles(mask)
      
      data_trends <- data_trends %>% 
        filter(COMMON.NAME %in% spec_qual$eBird.English.Name.2022,
               timegroups <= 2022)
      
    } else if (import_trend == "CAT") {
      
      spec_qual <- data_main %>% 
        filter(!(SOIBv2.Current.Status %in% status_to_filter),
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

plot_load_filter_data <- function(fn_plot_type, fn_cur_trend, fn_cur_mask = "none") {
  
  # metadata and paths --------------------------------------------------

  if (fn_plot_type == "single") {
    
    cur_metadata <- analyses_metadata %>% 
      filter(MASK == "none") %>% 
      mutate(CUR.OUT.PATH = PLOT.SINGLE.FOLDER) # path (folder) to write to
    
  } else if (fn_plot_type == "multi") {
    
    cur_metadata <- analyses_metadata %>%
      filter(MASK == "none") %>% 
      mutate(CUR.OUT.PATH = PLOT.MULTI.FOLDER) # path (folder) to write to
    
  } else {
    
    # process mask data
    cur_metadata <- analyses_metadata %>% 
      filter(MASK %in% c("none", as.character(fn_cur_mask))) %>% 
      # path (folder) to write to
      {if (fn_plot_type == "single_mask") {
        mutate(., CUR.OUT.PATH = PLOT.SINGLE.FOLDER) 
      } else if (fn_plot_type == "composite") {
        mutate(., CUR.OUT.PATH = PLOT.COMPOSITE.FOLDER) 
      }}
    
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
  
  if (fn_plot_type %in% c("single", "single_mask") & fn_cur_trend == "LTT") {
    web_metadata <- cur_metadata %>% 
      {if (fn_plot_type == "single_mask") {
        filter(., MASK != "none")
      } else {
        .
      }} 
  } else {
    web_metadata <- NULL
  }
  
  # load data ---------------------------------------------------------------
  
  # importing appropriate data filtered for qualified species
  
  data_processed <- map(
    cur_metadata$MASK, ~ plot_import_data(., 
                                          import_trend = fn_cur_trend, 
                                          import_plot_type = fn_plot_type)
  ) %>% 
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
                    data_main = data_main, path_write = path_write, web_metadata = web_metadata)
  
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
        str_flatten_comma(c("Greater Coucal","Jerdon's Bushlark","Forest Wagtail"))
        
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
    data_processed <- plot_import_data("none", cur_trend, plot_type)
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
        str_detect(Endemic.Region, "Island") ~ NA_character_, 
        str_ends(Endemic.Region, "Himalayas") ~ "Himalaya",
        # Endemic.Region %in% c("Eastern Himalayas", "Western Himalayas") ~ "Himalaya",
        Endemic.Region == "Western Ghats" ~ "Western Ghats & Sri Lanka",
        Endemic.Region == "Mainland India" ~ "Indian Subcontinent",
        Endemic.Region == "None" ~ "Non-endemic",
        TRUE ~ Endemic.Region
      )) %>% 
      filter(!is.na(GROUP)) %>% # all islands removed
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
      distinct(GROUP, Order, Habitat.Specialization) %>% 
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

# generate summary of composites ----------------------------------------------------

create_composite_summary <- function(metadata, init_obj) {

  summary_composite <- metadata %>% 
    rename(COMPOSITE.NO = PLOT.NO, COMPOSITE.NAME = PLOT.NAME) %>% 
    # converting to India Checklist names
    mutate(PLOT.SPEC = specname_to_india_checklist(PLOT.SPEC),
           SOIBv2.Priority.Status = case_when(SOIBv2.Priority.Status == "Moderate" ~ "MOD",
                                              TRUE ~ str_to_upper(SOIBv2.Priority.Status))) %>% 
    mutate(SOIBv2.Priority.Status = factor(SOIBv2.Priority.Status,
                                           levels = c("HIGH", "MOD", "LOW"))) %>% 
    group_by(COMPOSITE.NO, COMPOSITE.NAME, GROUP, SOIBv2.Priority.Status) %>% 
    dplyr::summarise(NO.SPEC = n_distinct(PLOT.SPEC),
                     LIST.SPEC = str_flatten_comma(PLOT.SPEC)) %>% 
    ungroup()
  
  init_obj <- init_obj %>% left_join(summary_composite)
  
  return(init_obj)

}

# ### PLOT: SoIB 2023 trend -------------------------------------------------------------

soib_trend_plot <- function(plot_type, cur_trend, cur_spec,
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
  
  if (!(plot_type %in% c("multi", "composite"))) {
    tic(glue("Finished plotting {plot_type} ({cur_trend}) for {cur_spec}"))
  } else {
    tic(glue("Finished plotting {cur_plot_metadata$FILE.NAME}"))
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
    path_write_file <- glue("{path_write}{unique(cur_plot_metadata$FILE.NAME)}.png")
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
  
  plot_fontfamily <- "Gandhi Sans"
  
  
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
    plot_xmin_minus <- 0.22
    
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
  
  toc()
}

