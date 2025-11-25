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
  
  if (analysis_type == "sysmon") {
    line_end <- tail(timegroups_num, 1)
  } else {
    line_end <- soib_year_info("latest_year")
  }
  
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


# x-axis brackets -------------------------------------------------------------------

geom_axisbracket <- function(bracket_type = "time", bracket_trend = cur_trend) {

  if (bracket_type == "time") {

  bracket_min <- timegroups_bracket_min
  bracket_max <- timegroups_bracket_max
  bracket_ypos <- plot_ymin0 - 0.01 * plot_range_max
  bracket_lab <- timegroups_lab[-1]
  bracket_tiplength <- 0
  bracket_labelsize <- 4.8
  bracket_fontface <- 1
  bracket_vjust <- 2.5
  
  } else if (bracket_type == "trend") {
    
    if (bracket_trend == "CAT") {
      return("Current Trend bracket should not be plotted in the CAT plot!")
    }
    
    bracket_min <- soib_year_info("cat_start") - 0.5
    bracket_max <- soib_year_info("latest_year") + 0.5
    bracket_ypos <- plot_ymin0 - 0.065 * plot_range_max
    bracket_lab <- "Current Trend"
    bracket_tiplength <- -0.017
    bracket_labelsize <- 5
    bracket_fontface <- 3
    bracket_vjust <- 2.7
    
  }
  
  bracket_shorten <- 0.15

  if (analysis_type == "sysmon") {
    
    if (plot_type == "bustards") {
      bracket_lab <- timegroups_lab
      bracket_shorten <- 0
    } 
    
    if (plot_type %in% c("spiti", "vembanad")) {
      bracket_lab <- timegroups_lab
    }  
    
  } 
  
  geom_bracket(inherit.aes = FALSE, family = plot_fontfamily, col = palette_plot_elem, # constant
               xmin = bracket_min, xmax = bracket_max, y.position = bracket_ypos,
               label = bracket_lab, label.size = bracket_labelsize, fontface = bracket_fontface,
               tip.length = bracket_tiplength, vjust = bracket_vjust, bracket.shorten = bracket_shorten)
  
}


# rangemap: remove false presences (states) ----------------------------------------

# due to edge cases: reported from same cell but point is outside current state

# First, get true list of species-grid presence combos:
# - species relevant for state actually reported from cell relevant for state (info_state_spec_grid)
# - retain only state and species of interest (using specieslist for state); removes slashes/spuhs
# Then, anti_join these "true presences" from the "to plot" data where occupancy == 1 --> false presences
# Then, remove these false presence rows from "to plot" data


rangemap_rm_falsepres <- function(data_occ, state, specieslist_for_state) {
  
  load("01_analyses_full/data_rangemap_info4state.RData")
  
  true_presence <- info_state_spec_grid %>% 
    filter(COMMON.NAME %in% specieslist_for_state$COMMON.NAME,
           ST_NM == state)
  
  false_presence <- data_occ %>% 
    filter(occupancy == 1) %>% 
    anti_join(true_presence, by = c("COMMON.NAME", "gridg1")) %>% 
    distinct(COMMON.NAME, gridg1)
  
  new_data_occ <- anti_join(data_occ, false_presence, by = c("COMMON.NAME", "gridg1"))
  
  return(new_data_occ)
  
}


# rangemap: custom key_glyph for occupancy fill legend ------------------------------

# creating our own key_glyph to be used for ggplot legend in rangemaps
# (ht https://www.emilhvitfeldt.com/post/changing-glyph-in-ggplot2/, 
#     https://stackoverflow.com/a/69958849/13000254)

draw_key_occupancy <- function(data, params, size) {
  
  grobTree(
    
    rectGrob(width = 0.67, height = 0.67,
             gp = gpar(col = NA, fill = alpha(data$fill, data$alpha))),
    rectGrob(gp = gpar(col = NA, fill = alpha(data$fill, data$alpha), 
                       # this grob gets repeated because we have fill for grids and the x-points
                       # hence, using slightly lower alpha to balance out
                       alpha = 0.3))
    
  )
  
}

# # testing
# ggplot(iris, aes(x = Species, y = Sepal.Length, fill = Species, alpha = Sepal.Length)) +
#   geom_point(shape = 21, key_glyph = draw_key_occupancy) +
#   scale_fill_manual(values = levels(palette_range_groups$COLOUR)) +
#   guides(alpha = "none")


# rangemap: manual legend --------------------------------------------------------------

# # we have to manually create the legend
# 
# geom_rangemap_legend <- function() {
# 
#   # outer translucent colour block
#   block1 <- annotate("rect", fill = palette_range_groups$COLOUR, # constant
#                      alpha = 0.6,
#                      xmin = c(82.2, 82.2, 82.2, 82.2), xmax = c(83.7, 83.7, 83.7, 83.7),
#                      ymin = c(13, 11, 9, 7), ymax = c(14.5, 12.5, 10.5, 8.5))
# 
#   # inner opaque colour block
#   block2 <- annotate("rect", fill = palette_range_groups$COLOUR, # constant
#                      alpha = 1,
#                      xmin = c(82.4, 82.4, 82.4, 82.4), xmax = c(83.5, 83.5, 83.5, 83.5),
#                      ymin = c(13.2, 11.2, 9.2, 7.2), ymax = c(14.3, 12.3, 10.3, 8.3))
# 
#   block_labels <- annotate("text", colour = palette_plot_elem, family = plot_fontfamily,
#                            size = 4.5, label = palette_range_groups$LABEL, hjust = 0,
#                            x = c(84.5, 84.5, 84.5, 84.5), y = c(13.75, 11.75, 9.75, 7.75))
#   # output:
#   list(block1, block2, block_labels)
# 
# }


# rangemap: manually filling occupancy  -------------------------------

# we have to manually add the four migratory types (since scale_fill already used for DEM basemap)

geom_rangemap_occ <- function(data_occ, data_admin, data_vag) {
  
  # colour block for proper occurrence
  occ_block <- geom_sf(data = data_occ, 
                       aes(alpha = occupancy, fill = fct_inorder(COLOUR)), 
                       col = NA, key_glyph = draw_key_occupancy)
  
  # admin outlines (above occupancy grid fill)
  outline_admin <- geom_sf(data = data_admin, colour = "white", fill = NA, size = 0.2)
  
  # x-points for vagrant records
  occ_vag <- geom_point(data = data_vag, aes(x = LONGITUDE, y = LATITUDE, 
                                             colour = fct_inorder(COLOUR), fill = fct_inorder(COLOUR)), 
                        shape = 4, size = 1, alpha = 1, key_glyph = draw_key_occupancy)
  
  # output:
  list(occ_block, outline_admin, occ_vag)
  
}


# SoIB trend plot theme -------------------------------------------------------------

ggtheme_soibtrend <- function() {
  
  theme_void() +
    theme(axis.title.y = element_text(size = plot_ylab_size, colour = palette_plot_elem,
                                      angle = 90, margin = plot_ytitle_margin),
          plot.margin = unit(c(0, 0, 0, 0), "cm"),
          plot.title = element_text(face = "bold", size = 20, hjust = 0.5, vjust = -2, 
                                    colour = palette_plot_title),
          text = element_text(family = plot_fontfamily),
          plot.background = element_rect(fill = "transparent", colour = NA),
          panel.background = element_rect(fill = "transparent", colour = NA))
  
}


# SoIB range map theme -------------------------------------------------------------

ggtheme_soibrangemap <- function() {
  
  theme_void() +
    theme(plot.margin = unit(c(0.5, 0, 0.5, 0), "cm"),
          plot.title = element_text(face = "bold", size = 20, hjust = 0.5, vjust = -2, 
                                    colour = palette_plot_title),
          text = element_text(family = plot_fontfamily),
          plot.background = element_rect(fill = "transparent", colour = NA),
          panel.background = element_rect(fill = "transparent", colour = NA)) +
    theme(legend.position = "bottom",
          legend.text = element_text(colour = palette_plot_elem, size = 12),
          legend.box.margin = margin(3, 0, 0, 0))
  
}


# import necessary data for a given mask  ---------------------------------------------

# read data and add 'Mask' column
plot_import_data <- function(mask, import_trend = fn_cur_trend, import_plot_type = fn_plot_type) {
  
  cur_metadata <- get_metadata(mask)
  
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
      mutate(MASK = factor(MASK, levels = levels(get_metadata()$MASK)),
             MASK.TITLE = factor(MASK.TITLE, 
                                 levels = get_metadata() %>% 
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
    # - only till latest MY
    
    # inconclusive should only be plotted when we have CI bands
    # since single-mask has band only for mask, we remove Inconclusive for country in plotting step
    # but for multispecies and composites we don't have CI bands, so remove here itself
    status_to_filter <- if (import_plot_type %in% c("single", "single_mask")) {
      c("Insufficient Data") 
    } else {
      c("Insufficient Data", "Trend Inconclusive") 
    }
    
    
    # here, we want to use the original "major" SoIB statuses to determine
    # which species get plotted
    # so in interannual update we use Major.Update column, 
    # while in major we use Latest (because no Major.Update created there)
    if (import_trend == "LTT") {
      
      spec_qual <- data_main %>% 
        {if (interannual_update == TRUE) {
          filter(.,
                 !(SoIB.Major.Update.Long.Term.Status %in% status_to_filter),
                 Long.Term.Analysis == "X") 
        } else {
          filter(.,
                 !(SoIB.Latest.Long.Term.Status %in% status_to_filter),
                 Long.Term.Analysis == "X")  
        }} %>% 
        dplyr::select(eBird.English.Name.2024) %>% 
        add_mask_titles(mask)
      
      data_trends <- data_trends %>% 
        filter(COMMON.NAME %in% spec_qual$eBird.English.Name.2024,
               timegroups <= soib_year_info("latest_year"))
      
    } else if (import_trend == "CAT") {
      
      spec_qual <- data_main %>% 
        {if (interannual_update == TRUE) {
          filter(.,
                 !(SoIB.Major.Update.Current.Status %in% status_to_filter),
                 Current.Analysis == "X")
        } else {
          filter(.,
                 !(SoIB.Latest.Current.Status %in% status_to_filter),
                 Current.Analysis == "X")
        }} %>% 
        dplyr::select(eBird.English.Name.2024) %>% 
        add_mask_titles(mask)
      
      data_trends <- data_trends %>% 
        filter(COMMON.NAME %in% spec_qual$eBird.English.Name.2024,
               timegroups >= soib_year_info("cat_start") & timegroups <= soib_year_info("latest_year"))
      
    }
    
    
    # return ----------------------------------------------------------------------------

    return(list(spec_qual = spec_qual, data_trends = data_trends, data_main = data_main))

  }
    
}

# load appropriate data and filter for species qualified for plotting ------------------

plot_load_filter_data <- function(fn_plot_type, fn_cur_trend, fn_cur_mask = "none") {
  
  # metadata and paths --------------------------------------------------

  if (fn_plot_type == "single") {
    
    cur_metadata <- get_metadata("none") %>% 
      mutate(CUR.OUT.PATH = PLOT.SINGLE.FOLDER) # path (folder) to write to
    
  } else if (fn_plot_type == "multi") {
    
    cur_metadata <- get_metadata("none") %>% 
      mutate(CUR.OUT.PATH = PLOT.MULTI.FOLDER) # path (folder) to write to
    
  } else {
    
    # process mask data
    cur_metadata <- get_metadata() %>% 
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
  
  if (fn_plot_type %in% c("single", "single_mask")) {
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
  #
  # UPDATE (March 2024): removing this criterion for "single_mask", i.e., non-country masks
  # (but retaining for composites), because we want to show the Inconclusive Trend graphs
  # for subnational masks 
  # Requirement arose from species that are Insufficient Data at national level but have
  # data for some mask(s)
  
  if (fn_plot_type == "composite" & fn_cur_mask != "none") {
    
    spec_qual <- map(data_processed, pluck, "spec_qual") %>% bind_rows()
    
    spec_qual_country <- spec_qual %>% filter(MASK == "none") %>% pull(eBird.English.Name.2024)
    spec_qual_masks <- spec_qual %>% filter(MASK != "none") %>% pull(eBird.English.Name.2024)
    
    spec_qual <- intersect(spec_qual_country, spec_qual_masks)
    
    data_trends <- data_trends %>% filter(COMMON.NAME %in% spec_qual)
    data_main <- data_main %>% filter(eBird.English.Name.2024 %in% spec_qual)
    
  } else {
    
    spec_qual <- map(data_processed, pluck, "spec_qual") %>% 
      bind_rows() %>% 
      {if (fn_plot_type == "single_mask") {
        filter(., MASK != "none") 
      } else {
        .
      }} %>% 
      pull(eBird.English.Name.2024)
    
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
        str_flatten_comma(c("Tibetan Sand-Plover","Terek Sandpiper","Whimbrel","Curlew Sandpiper",
                            "Eurasian Curlew")),
        str_flatten_comma(c("Oriental Honey-buzzard","Black Kite","Western Marsh Harrier",
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
        Habitat.Specialization == "Non-specialized" ~ "No Specialisation",
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
        TRUE ~ Endemic.Region
      )) %>% 
      filter(!is.na(GROUP)) %>% # all islands removed
      distinct(GROUP, Endemic.Region) %>% 
      mutate(PLOT.NAME = "Endemic Regions")
    
    
    # shorebirds composite
    data4 <- data_main %>% 
      mutate(GROUP = fct_collapse(
        
        eBird.English.Name.2024,
        
        "Near Resident or Palearctic Migrant" = c(
          "Indian Thick-knee","Great Thick-knee","Beach Thick-knee","Black-winged Stilt",
          "Red-wattled Lapwing","Little Ringed Plover","Greater Painted-Snipe",
          "Pheasant-tailed Jacana","Bronze-winged Jacana","Solitary Snipe",
          "Barred Buttonquail","Indian Courser","Jerdon's Courser","Small Pratincole"
        ),
        
        "Near Resident or Palearctic Migrant" = c(
          "Pied Avocet","Ibisbill","Eurasian Oystercatcher","Northern Lapwing",
          "Gray-headed Lapwing","Sociable Lapwing","White-tailed Lapwing",
          "Tibetan Sand-Plover","Greater Sand-Plover","Caspian Plover",
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
      distinct(GROUP, eBird.English.Name.2024) %>% 
      mutate(PLOT.NAME = "Shorebird Migratory Behaviours")
    
    data5 <- data_main %>% 
      filter(Order %in% c("Accipitriformes", "Falconiformes")) %>% 
      mutate(GROUP = case_when(
        Habitat.Specialization %in% c("Grassland", "Wetland", 
                                      "Alpine & Cold Desert") ~ "Open Habitat",
        Habitat.Specialization == "Forest" ~ "Forest & Plantation",
        Habitat.Specialization == "Non-specialized" ~ "No Specialisation",
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
      distinct(GROUP, eBird.English.Name.2024) %>% 
      mutate(PLOT.NAME = "Woodland")
    
    
    #plot_load_filter_data(plot_type, cur_trend, c("ONEland", "cropland"))
    #getting an error here so removing this step for now
    
    data7 <- data_main %>% 
      rename(GROUP = MASK.TITLE) %>% 
      filter(!is.na(GROUP)) %>% 
      distinct(GROUP, eBird.English.Name.2024) %>% 
      mutate(PLOT.NAME = "Open Natural Ecosystems & Cropland")

    
    plot_load_filter_data(plot_type, cur_trend, "PA")
    
    data8 <- data_main %>% 
      rename(GROUP = MASK.TITLE) %>% 
      filter(!is.na(GROUP)) %>% 
      distinct(GROUP, eBird.English.Name.2024) %>% 
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

# fetch metadata for current systematic monitoring case --------------------------------

fetch_sysmon_metadata <- function(cur_case) {
  
  if (cur_case == "full") {
    
    load("00_data/sysmon_metadata.RData")
    list2env(list(sysmon_metadata = sysmon_metadata), envir = .GlobalEnv)
    
  } else {
    
    load("00_data/sysmon_metadata.RData")
    
    if (!(cur_case %in% unique(sysmon_metadata$CASE))) {
      return("Select valid case study!")
    }
    
    cur_metadata <- sysmon_metadata %>% filter(CASE == cur_case)
    
    path_data <- cur_metadata$PATH.DATA
    path_out <- cur_metadata$PATH.OUT
    
    if (cur_case %in% c("spiti", "vembanad")) {
      path_data_extra <- cur_metadata$PATH.DATA.EXTRA
      path_out_extra <- cur_metadata$PATH.OUT.EXTRA
    }
    
    plot_ylab <- cur_metadata$METRIC
    
    
    # assign to environment
    
    obj_list <- list(cur_metadata = cur_metadata,
                     path_data = path_data, 
                     path_out = path_out,
                     plot_ylab = plot_ylab)
    
    if (cur_case %in% c("spiti", "vembanad")) {
      obj_list <- c(obj_list, 
                    list(path_data_extra = path_data_extra,
                         path_out_extra = path_out_extra))
    }
    
    list2env(obj_list, envir = .GlobalEnv)
    
  }
  
}

# generate summary of composites ----------------------------------------------------

create_composite_summary <- function(metadata, init_obj) {

  summary_composite <- metadata %>% 
    rename(COMPOSITE.NO = PLOT.NO, COMPOSITE.NAME = PLOT.NAME) %>% 
    # converting to India Checklist names
    mutate(PLOT.SPEC = specname_to_india_checklist(PLOT.SPEC),
           SoIB.Latest.Priority.Status = case_when(SoIB.Latest.Priority.Status == "Moderate" ~ "MOD",
                                              TRUE ~ str_to_upper(SoIB.Latest.Priority.Status))) %>% 
    mutate(SoIB.Latest.Priority.Status = factor(SoIB.Latest.Priority.Status,
                                           levels = c("HIGH", "MOD", "LOW"))) %>% 
    group_by(COMPOSITE.NO, COMPOSITE.NAME, GROUP, SoIB.Latest.Priority.Status) %>% 
    dplyr::summarise(NO.SPEC = n_distinct(PLOT.SPEC),
                     LIST.SPEC = str_flatten_comma(PLOT.SPEC)) %>% 
    ungroup()
  
  init_obj <- init_obj %>% left_join(summary_composite)
  
  return(init_obj)

}

# ### PLOT: SoIB 2023 trend -------------------------------------------------------------

soib_trend_plot <- function(plot_type, cur_trend, cur_spec,
                            data_trends, data_main, path_write,
                            cur_plot_metadata, haki = FALSE) {
  
  # don't try to plot if species is not qualified for current trend-mask combo
  if (!(cur_spec %in% spec_qual)) {
    stop("Selected species is not qualified for current trend analysis for current mask!")
  }
  
  require(tidyverse)
  require(ggpubr) # geom_bracket
  require(extrafont)
  require(glue)
  require(ggrepel) # text repel
  require(tictoc)
  require(magick) # for stamp
  
  analysis_type <- "ebird"
  
  if (plot_type == "stamp") {
    cur_trend <- "LTT"
    stamp <- image_convert(image_read("stamp_insufficient_data.png"), matte = T)
  }
  
  # convert to India Checklist names --------------------------------------------------
  
  if (!plot_type %in% c("composite", "stamp")) { 
    # because composite doesn't have any species names at this stage
    data_trends <- data_trends %>% 
      mutate(COMMON.NAME = specname_to_india_checklist(COMMON.NAME, already_show = FALSE))
    data_main <- data_main %>% 
      mutate(eBird.English.Name.2024 = specname_to_india_checklist(eBird.English.Name.2024, already_show = FALSE))
    cur_spec <- cur_spec %>% 
      specname_to_india_checklist(already_show = FALSE)
  }
  
  if (!haki & plot_type != "stamp") {
    if (!(plot_type %in% c("multi", "composite"))) {
      tic(glue("Finished plotting {plot_type} ({cur_trend}) for {cur_spec}"))
    } else {
      tic(glue("Finished plotting {cur_plot_metadata$FILE.NAME}"))
    }
  }
  
  # setup -----------------------------------------------------------------------------
  
  # output file name
  if (plot_type == "stamp") {
    
    path_write_file <- "20_website/graphs/insufficient_data.png"
    
  } else if (!(plot_type %in% c("multi", "composite"))) {

    # for website (originated that way, but now PNG works for both)
    source("00_scripts/20_functions.R")
    cur_plot_metadata <- join_mask_codes(cur_plot_metadata)
    web_folder <- cur_plot_metadata$WEB.PLOTS.FOLDER
    web_spec <- str_replace_all(cur_spec, c(" " = "-", "'" = "_"))
    web_mask <- cur_plot_metadata$MASK.CODE
    path_write_file <- glue("{path_write}{web_spec}_{web_mask}_{cur_trend}_trend.png")

  } else {
    path_write_file <- glue("{path_write}{unique(cur_plot_metadata$FILE.NAME)}.png")
  }
  
  
  if (plot_type != "stamp") {
    
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
        filter(eBird.English.Name.2024 %in% cur_spec,
               MASK == "none") %>% 
        {if (cur_trend == "LTT") {
          pull(., SoIB.Latest.Long.Term.Status)
        } else if (cur_trend == "CAT") {
          pull(., SoIB.Latest.Current.Status)
        }}
      
    }
    
  } else {
    cur_data_trends <- NA
  }
  
  # plot/theme settings -----------------------------------------------------
  
  palette_plot_elem <- "#56697B"
  palette_plot_title <- "#A13E2B"
  palette_trend_groups <- c("#869B27", "#E49B36", "#436b74", "#CC6666", 
                            "#B69AC9", "#319cc0","#31954E","#493F3D",
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
                        "", "2014", "", "2016", "", "2018", "", 
                        "2020", "", "2022", "", "2024")
    timegroups_bracket_min <- c(1999, 2006, 2010, 2012, seq(2013, 2023)) + 0.5
    timegroups_bracket_max <- c(2006, 2010, 2012, seq(2013, 2024)) + 0.5
    plot_ytitle_margin <- margin(0, -0.6, 0, 0.4, "cm")
    plot_ylab_size = 22
    plot_xlimits <- c(1999.5, 2024.5)
    plot_gridline_x <- 2024.3
    plot_baseline_lab <- "Pre-2000\nbaseline"
    plot_repel_nudge <- -1.5
    plot_xmin_minus <- 0.22
    
  } else if (cur_trend == "CAT") {
    
    timegroups_lab <- c("2015", "2016", "2017", "2018", "2019", "2020", "2021", "2022")
    timegroups_bracket_min <- seq(2015, 2023) + 0.5
    timegroups_bracket_max <- seq(2016, 2024) + 0.5
    plot_ytitle_margin <- margin(0, 0.6, 0, 0.4, "cm")
    plot_ylab_size = 22
    plot_xlimits <- c(2015.09, 2024.1)
    plot_gridline_x <- 2023.7
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
      filter(timegroups == soib_year_info("latest_year")) %>%
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
  
  if (plot_type == "stamp") {
    
    plot_xmin <- 2003
    timegroups_bracket_min <- c(1999 - (2003 - plot_xmin), 
                                2006, 2010, 2012, seq(2013, 2021)) + 0.5
    plot_xlimits <- c(1999.5 - (2003 - plot_xmin), 
                      2024.5)
    
  } else if (plot_type != "single_mask") {
    
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
    
  } else {
    
    plot_xmin <- cur_data_trends %>%
      distinct(., MASK, COMMON.NAME, timegroups) %>%
      arrange(., MASK, COMMON.NAME, timegroups) %>%
      group_by(., MASK, COMMON.NAME) %>% 
      slice(2) %>% # because 1st is the baseline
      ungroup() %>%
      pull(timegroups) %>%
      min() 
    
    if (cur_trend == "LTT") {
      # in some masks, minimum year is < 2003, which reduces the margin for labels
      timegroups_bracket_min <- c(1999 - (2003 - plot_xmin), 
                                  2006, 2010, 2012, seq(2013, 2021)) + 0.5
      plot_xlimits <- c(1999.5 - (2003 - plot_xmin), 
                        2024.5)
    }
    
  }
  
  
  if (plot_type != "stamp") {  
    
    # saving non-rounded values for later use in plotting
    if (plot_type == "single") {
      plot_ymax0 <- cur_data_trends %>%
        filter(!is.na(rci_std)) %>%
        pull(rci_std) %>%
        max()
      
      plot_ymin0 <- cur_data_trends %>%
        filter(!is.na(lci_std)) %>%
        pull(lci_std) %>%
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
  
}
    
  # determining y-axis breaks for current plot ----------------------------------------
  
  if (plot_type == "stamp") {
    
    plot_ybreaks <- c(0, 50, 100, 150, 200)
    plot_ymin <- min(plot_ybreaks)
    plot_ymin0 <- plot_ymin
    plot_ymax <- max(plot_ybreaks)
    plot_ymax0 <- plot_ymax
    plot_range_max <- plot_ymax - plot_ymin
    
  } else {

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
    
    # to standardize the y-range
    if (plot_range_max == 100)
      plot_range_max = 125
    if (plot_range_max == 200)
      plot_range_max = 250
    
  }
  
  
  # fixing the Status reference grid line -------------------------------------------------
  
  if (plot_type != "stamp") {
    
    # depending on the present-day trend value (and which Status), we want its nearest
    # grid line to act as a reference for the Status threshold.
    
    ref_line <- cur_data_trends %>%
      filter(timegroups == soib_year_info("latest_year")) %>%
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
    
    # updating breaks based on each of 2024 trend values plotted (one in single, multiple in others)
    plot_ybreaks <- reduce(ref_line, update_breaks, .init = plot_ybreaks)
    
  }
  
  
  plot_ybreaks_df <- data.frame(breaks = plot_ybreaks) %>%
    # labels for each line/break
    mutate(breaks_eff = breaks - 100) %>% # convert to + and - values
    mutate(labs = case_when(breaks_eff > 0 ~ glue("+{breaks_eff}%"),
                            breaks == 100 & plot_type != "stamp" ~ glue(""), # baseline blank
                            breaks == 100 & plot_type != "stamp" ~ glue("0%"), 
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
  
  obj_list <- list(analysis_type = analysis_type,
                   path_write_file = path_write_file,
                   cur_data_trends = cur_data_trends,
                   palette_plot_elem = palette_plot_elem,
                   palette_plot_title = palette_plot_title,
                   palette_trend_groups = palette_trend_groups,
                   plot_fontfamily = plot_fontfamily,
                   timegroups_lab = timegroups_lab,
                   timegroups_bracket_min = timegroups_bracket_min,
                   timegroups_bracket_max = timegroups_bracket_max,
                   plot_ytitle_margin = plot_ytitle_margin,
                   plot_ylab_size = plot_ylab_size,
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
  
  if (plot_type == "stamp") {
    
    plot_base <- ggplot(data = data.frame(X = cur_data_trends))
    
  } else if (plot_type == "single_mask") {
    
    # don't plot full country trend line, point, label if Inconclusive
    # same if Insufficient Data, but there data_trends will not have MASK == "none" at all 
    if (plot_full_country == "Trend Inconclusive") {
      cur_data_trends <- cur_data_trends %>% 
        mutate(mean_std = case_when(MASK == "none" ~ NA_real_, TRUE ~ mean_std),
               MASK.TITLE = case_when(MASK == "none" ~ NA_character_, TRUE ~ MASK.TITLE))
    } else if (plot_full_country == "Insufficient Data") {
      cur_data_trends <- cur_data_trends %>% 
        mutate(mean_std = NA_real_,
               MASK = "none",
               MASK.TITLE = NA_character_,
               MASK.TITLE.WRAP = NA_character_) %>% 
        bind_rows(cur_data_trends)
    }
    
    # align labels when first year mismatch
    min_years <- cur_data_trends %>% 
      distinct(COMMON.NAME, timegroups, MASK) %>% 
      group_by(COMMON.NAME, MASK) %>% 
      slice(2) %>% 
      ungroup() %>% 
      distinct(MASK, timegroups) %>% 
      mutate(DIFF = max(timegroups) - min(timegroups),
             MIN.YEAR = min(timegroups)) %>% 
      filter(timegroups == MIN.YEAR)
    
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
      geom_line(linewidth = 1, lineend = "round") +
      geom_point(size = 3) +
      geom_text_repel(nudge_x = ifelse(cur_data_trends$MASK == min_years$MASK, 
                                       plot_repel_nudge, plot_repel_nudge - min_years$DIFF), 
                      direction = "y", 
                      hjust = 0.5, size = 4, force_pull = 0,
                      xlim = c(plot_xlimits[1] - 0.3, plot_xmin - plot_xmin_minus),
                      family = plot_fontfamily, min.segment.length = Inf) +
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
      {if (plot_type != "stamp") {
        geom_gridline(baseline = TRUE)
      } else {
        annotation_raster(stamp, 
                          ymin = 40, ymax = 160,
                          xmin = plot_xlimits[1] + 1.25*diff(plot_xlimits)/5, 
                          xmax = plot_xlimits[2] - 0.75*diff(plot_xlimits)/5)
      }} +
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

  # removing objects from global environment ------------------------------------------
  
  rm(list = names(obj_list), envir = .GlobalEnv)
  
  if (!haki) {
    toc()
  }
  
}


# ### PLOT: SoIB 2023 trend (sysmon) -------------------------------------------------------------

# needs to be integrated with above; leaving separate for want of time


soib_trend_plot_sysmon <- function(plot_type, cur_data_trends,
                                   analysis_type = "sysmon") {
  
  # is it a single-species plot
  if (plot_type %in% c("nannaj", "spiti", "vembanad") & 
      n_distinct(cur_data_trends$COMMON.NAME) == 1) {
    sysmon_single <- TRUE
  } else {
    sysmon_single <- FALSE
  }
  
  # plot/theme settings -----------------------------------------------------
  
  palette_plot_elem <- "#56697B"
  palette_plot_title <- "#A13E2B"
  palette_trend_groups <- c("#869B27", "#E49B36", "#436b74", "#CC6666", 
                            "#B69AC9", "#319cc0","#31954E","#493F3D",
                            "#EA5599", "#9999CC", "#A13E2B", "#66CC99")
  if (plot_type == "nannaj" & sysmon_single == TRUE) {
    palette_trend_groups <- palette_trend_groups[1]
  } 
  
  plot_fontfamily <- "Gandhi Sans"
  
  
  # other plot settings ###
  
  if (plot_type == "bustards") {
    
    geom_pos_setting <- "identity"
    
    # defining and adjusting timegroups
    timegroups_num <- cur_data_trends %>%
      distinct(timegroups) %>% 
      arrange(timegroups) %>% 
      # if two years are consecutive, take the later one
      mutate(diff_tg = lead(timegroups, default = 0) - timegroups) %>%
      filter(diff_tg >= 2 | diff_tg < 0) %>% # the last year will give negative diff_tg
      distinct(timegroups) %>% 
      pull(timegroups) %>% 
      c(1968, .) # add first year
    
  } else if (plot_type == "hornbills") {
    
    # for those plots with errorbars
    geom_pos_setting <- position_dodge(0.3)
    timegroups_num <- 2015:2024
    
  } else if (plot_type == "nannaj") {
    
    geom_pos_setting <- position_dodge(0.3)
    timegroups_num <- 2012:2023
    
  } else if (plot_type == "spiti" & sysmon_single == TRUE) {
    
    geom_pos_setting <- position_dodge(0.3)
    timegroups_num <- 2002:2024
    
  } else if (plot_type == "vembanad") {
    
    geom_pos_setting <- position_dodge(0.3)
    timegroups_num <- 2001:2023
    
  } else {
    
    geom_pos_setting <- position_dodge(0.3)
    timegroups_num <- min(cur_data_trends$timegroups):max(cur_data_trends$timegroups)
    
  }
  
  timegroups_lab <- timegroups_num %>% as.character()
  
  
  if (plot_type == "bustards") {
    
    # to sort out problematic timeline and resultant x brackets
    temp1 = timegroups_num[-1] + 0.5
    temp1 = temp1[-length(temp1)]
    
    temp2 = timegroups_num[-1] - 0.5
    temp2 = temp2[-1]
    
    timegroups_bracket_min <- sort(c(timegroups_num[-1] - 0.5, temp1))
    timegroups_bracket_max <- sort(c(timegroups_num[-1] + 0.5, temp2))
    
    temp3 <- character(length(timegroups_lab[-1]) * 2 - 1)
    temp3[seq(1, length(temp3), 2)] <- timegroups_lab[-1]
    timegroups_lab <- temp3
    
    
    plot_ytitle_margin <- margin(0, -0.6, 0, 0.4, "cm")
    plot_ylab_size = 22
    plot_xlimits <- c(head(timegroups_num, 1) - 8, tail(timegroups_num, 1) + 8)
    plot_gridline_x <- tail(timegroups_num, 1) + 2
    plot_repel_nudge <- -2.5
    plot_xmin_minus <- 0.7
    
  } else if (plot_type == "spiti") {
    
    timegroups_bracket_min <- c(seq(2001, 2023)) + 0.5
    timegroups_bracket_max <- c(seq(2002, 2024)) + 0.5
    
    temp3 <- seq(2,length(timegroups_lab),2)
    timegroups_lab[temp3] = ""
    
    plot_ytitle_margin <- margin(0, 0.6, 0, 0.4, "cm")
    plot_ylab_size = 22
    plot_xlimits <- c(1999.1, 2023.5)
    plot_gridline_x <- tail(timegroups_bracket_max, 1) + 0.5
    plot_repel_nudge <- -1.7
    plot_xmin_minus <- 0.2
    
  } else if (plot_type %in% c("hornbills")) {
    
    timegroups_bracket_min <- tail(timegroups_num, -1) - 0.5
    timegroups_bracket_max <- tail(timegroups_num, -1) + 0.5
    
    plot_ytitle_margin <- margin(0, 0.6, 0, 0.4, "cm")
    plot_ylab_size = 22
    plot_xlimits <- c(head(timegroups_num, 1) - 0.128, tail(timegroups_num, 1) + 1)
    plot_gridline_x <- tail(timegroups_bracket_max, 1) + 0.2
    plot_repel_nudge <- -0.742
    plot_xmin_minus <- 0.275
    
  } else if (plot_type %in% c("nannaj")) {
    
    timegroups_bracket_min <- tail(timegroups_num, -1) - 0.5
    timegroups_bracket_max <- tail(timegroups_num, -1) + 0.5
    
    plot_ytitle_margin <- margin(0, 0.6, 0, 0.4, "cm")
    plot_ylab_size = 25
    plot_xlimits <- c(head(timegroups_num, 1) - 0.128, tail(timegroups_num, 1) + 1)
    plot_gridline_x <- tail(timegroups_bracket_max, 1) + 0.2
    plot_repel_nudge <- -0.742
    plot_xmin_minus <- 0.275
    
    bracket_labelsize <- 6
    
  } else if (plot_type == "vembanad") {
    
    timegroups_bracket_min <- timegroups_num - 0.5
    timegroups_bracket_max <- timegroups_num + 0.5
    
    temp3 <- seq(2,length(timegroups_lab),2)
    timegroups_lab[temp3] = ""
    
    plot_ytitle_margin <- margin(0, 0.6, 0, 0.4, "cm")
    plot_ylab_size = 22
    plot_xlimits <- c(head(timegroups_num, 1) - 3.5, tail(timegroups_num, 1) + 3)
    plot_gridline_x <- tail(timegroups_bracket_max, 1) + 1
    plot_repel_nudge <- -2.2
    plot_xmin_minus <- 0.7
    
  }
  
  
  # wrap names and reorder species based on final year value ###
  
  wrap_nchar <- 15 
  
  # for each species, arranging different (latest) mask trend values in desc. order
  # (mainly useful for mask comparison graphs)
  spec_order <- cur_data_trends %>%
    group_by(COMMON.NAME) %>% 
    filter(timegroups == max(timegroups)) %>%
    arrange(desc(mean_std)) %>% 
    distinct(COMMON.NAME)
  
  cur_data_trends <- cur_data_trends %>% 
    mutate(COMMON.NAME = factor(COMMON.NAME, levels = spec_order$COMMON.NAME)) %>% 
    group_by(COMMON.NAME) %>% 
    # wrapping the labels for each trend line based on number of characters
    # we will be plotting the label as a geom, not as a label of a geom, so only need for first x value
    mutate(COMMON.NAME.WRAP = case_when(
      timegroups == min(timegroups) ~ str_wrap(COMMON.NAME, width = wrap_nchar),
      TRUE ~ ""
    )) %>% 
    ungroup()
  
  
  # determining limits & breaks for current plot -----------------------------------------------
  
  # x lim
  plot_xmin <- cur_data_trends %>%
    distinct(COMMON.NAME, timegroups) %>%
    arrange(COMMON.NAME, timegroups) %>%
    group_by(COMMON.NAME) %>% 
    slice(1) %>% # because no baseline in sysmon graphs
    ungroup() %>%
    pull(timegroups)
  
  if (plot_type == "bustards" |
      plot_type == "spiti" & sysmon_single == FALSE |
      plot_type == "vembanad") {
    
    plot_xmin <- min(plot_xmin)
    
    # ylim
    plot_ymin <- cur_data_trends %>%
      filter(!is.na(mean_std)) %>%
      pull(mean_std) %>%
      min()
    plot_ymax <- cur_data_trends %>%
      filter(!is.na(mean_std)) %>%
      pull(mean_std) %>%
      max()
    
  } else {
    
    plot_xmin <- max(plot_xmin)
    
    # ylim
    plot_ymin <- cur_data_trends %>%
      filter(!is.na(lci_std)) %>%
      pull(lci_std) %>%
      min()
    plot_ymax <- cur_data_trends %>%
      filter(!is.na(rci_std)) %>%
      pull(rci_std) %>%
      max()
    
  }
  
  
  plot_range_max <- plot_ymax - plot_ymin
  
  plot_ymin <- plot_ymin - 0.1 * plot_range_max
  if (plot_ymin < 0) {
    plot_ymin <- 0
  }
  plot_ymax <- plot_ymax + 0.1 * plot_range_max
  
  # for plotting
  plot_ymax0 <- plot_ymax 
  plot_ymin0 <- plot_ymin 
  
  
  # y breaks
  if (plot_type == "bustards") { # estimate
    round_deci <- -1
  } else if (plot_type %in% c("nannaj", "spiti") & sysmon_single == TRUE) {
    round_deci <- 2
  } else if (plot_type == "vembanad") { # high counts
    round_deci <- -2
    if ("Total" %in% unique(cur_data_trends$COMMON.NAME) |
        sysmon_single == FALSE){
      round_deci <- -3
    }
  } else {
    round_deci <- 1
  }
  
  plot_ybreaks <- seq(plot_ymin, plot_ymax, length.out = 5) %>% round(round_deci)
  plot_ybreaks_lab <- plot_ybreaks %>% round(round_deci)
  
  # assigning objects to environment --------------------------------------------------
  
  obj_list <- list(analysis_type = analysis_type,
                   plot_type = plot_type,
                   sysmon_single = sysmon_single,
                   path_write_file = path_out,
                   cur_data_trends = cur_data_trends,
                   palette_plot_elem = palette_plot_elem,
                   palette_plot_title = palette_plot_title,
                   palette_trend_groups = palette_trend_groups,
                   plot_fontfamily = plot_fontfamily,
                   timegroups_num = timegroups_num,
                   timegroups_lab = timegroups_lab,
                   timegroups_bracket_min = timegroups_bracket_min,
                   timegroups_bracket_max = timegroups_bracket_max,
                   plot_ytitle_margin = plot_ytitle_margin,
                   plot_ylab_size = plot_ylab_size,
                   plot_xlimits = plot_xlimits,
                   plot_gridline_x = plot_gridline_x,
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
  
  plot_base <- ggplot(cur_data_trends, 
                      aes(x = timegroups, y = mean_std, 
                          col = COMMON.NAME, label = COMMON.NAME.WRAP)) +
    geom_line(linewidth = 1, lineend = "round", position = geom_pos_setting) +
    {if (sysmon_single == FALSE) {
      geom_text_repel(nudge_x = plot_repel_nudge, direction = "y", 
                      hjust = 0.5, size = 4, force_pull = 0,
                      xlim = c(plot_xlimits[1] - 0.1, plot_xmin - plot_xmin_minus),
                      family = plot_fontfamily, min.segment.length = Inf)
    }} +
    geom_point(size = 3, position = geom_pos_setting) +
    {if (plot_type %in% c("hornbills", "nannaj") |
         plot_type == "spiti" & sysmon_single == TRUE) {
      geom_errorbar(aes(ymin = lci_std, ymax = rci_std), 
                    linewidth = 1, width = 0.2, position = geom_pos_setting)    
    }} +
    scale_colour_manual(values = palette_trend_groups) +
    scale_fill_manual(values = palette_trend_groups) 
  
  # completing and writing the plot -----------------------------------------------------
  
  # joining plot base with other constant aesthetic features of graph
  
  cur_plot <- plot_base +
    # timegroup brackets
    geom_axisbracket("time") + 
    # manual grid lines with labels because we want empty space before the first timegroup
    geom_gridline(1) +
    geom_gridline(2) +
    geom_gridline(3) +
    geom_gridline(4) +
    geom_gridline(5) +
    coord_cartesian(ylim = c(plot_ymin0 - 0.1 * plot_range_max,
                             plot_ymax0 + 0.1 * plot_range_max),
                    clip = "off") +
    scale_x_continuous(expand = c(0, 0), limits = plot_xlimits) +
    scale_y_continuous(expand = c(0, 0)) +
    # ggtitle(cur_spec) +
    labs(x = "Time-steps", y = plot_ylab) +
    guides(colour = "none", fill = "none") +
    # theme
    ggtheme_soibtrend()
  
  ggsave(filename = path_write_file, plot = cur_plot,
         dpi = 500, bg = "transparent",
         width = 11, height = 7.5, units = "in")

  # removing objects from global environment ------------------------------------------
  
  rm(list = names(obj_list), envir = .GlobalEnv)
  
}


# ### PLOT: SoIB 2023 range map -------------------------------------------------------------

soib_rangemap <- function(which_spec = "all", cur_mask = "none") {
  
  # which_spec either "all" or eBird name
  
  # setup -----------------------------------------------------------------------------
  
  require(tidyverse)
  require(sf)
  require(tictoc)
  require(glue)
  require(ggnewscale) # to allow us to specify new scale_fill for filling occupancy grids
  require(grid) # for custom key_glyph (for  legend)
  require(rlang) # for custom key_glyph (for legend)
  require(furrr)
  require(parallel)
  
  sf_use_s2(FALSE) # not using spherical geometries
  
  tic(glue("Finished creating range map [mask: {cur_mask}] for [species: {str_flatten_comma(which_spec)}]"))
  
  
  source("00_scripts/00_functions.R")
  source("00_scripts/20_functions.R")
  
  cur_metadata <- get_metadata(cur_mask) %>% join_mask_codes()
  
  if (!cur_metadata$MASK.TYPE %in% c("country", "state")) {
    return("Select either a country or a state!")
  }
  
  load("00_data/maps_sf.RData")
  # load DEM
  load("00_data/map_DEM.RData")
  
  
  if (cur_metadata$MASK.TYPE == "country") {
    admin_sf <- states_sf # which admin outlines to plot over basemap
    cur_g1_sf <- g1_in_sf # which grid polygon to use
  } else if (cur_metadata$MASK.TYPE == "state") {
    admin_sf <- dists_sf %>% filter(STATE.NAME == cur_mask)
    cur_state_sf <- states_sf %>% filter(STATE.NAME == cur_mask)
    
    load("00_data/grids_st_sf.RData")
    rm(g2_st_sf, g3_st_sf, g4_st_sf)
    cur_g1_sf <- g1_st_sf %>% filter(STATE.NAME == cur_mask)
    
    # for filtering data later
    cur_g1_filt <- cur_g1_sf %>% distinct(GRID.G1) %>% pull(GRID.G1)
  }
  
  
  # input paths
  path_main <- cur_metadata$SOIBMAIN.PATH
  path_speclists <- cur_metadata$SPECLISTDATA.PATH
  path_toplot <- cur_metadata$MAP.DATA.OCC.PATH
  path_vagrants <- cur_metadata$MAP.DATA.VAG.PATH
  
  
  # load data -------------------------------------------------------------------------
  
  # data
  
  load(path_speclists)
  
  occ_final = read.csv(path_toplot) %>% 
    mutate(gridg1 = as.character(gridg1)) %>% 
    filter(COMMON.NAME %in% specieslist$COMMON.NAME) # mostly needed for states
  
  vagrant_presence = read.csv(path_vagrants) %>% 
    # for spatial join later
    rownames_to_column("ID")
  
  main = read.csv(path_main)
  
  
  # for states, we want to filter this data appropriately
  if (cur_metadata$MASK.TYPE == "state") {
    
    # 1. filter "to plot" data for valid grids in current state
    occ_final <- occ_final %>% filter(gridg1 %in% cur_g1_filt)
    
    
    # 2. spatial filter for vagrant data
    vagrant_filt <- vagrant_presence %>% 
      st_as_sf(coords = c("LONGITUDE", "LATITUDE")) %>% 
      st_set_crs(st_crs(states_sf))
    
    vagrant_filt <- vagrant_filt[unlist(st_intersects(cur_state_sf, vagrant_filt)),] %>% 
      st_drop_geometry() %>% 
      distinct(ID)
    
    vagrant_presence <- vagrant_presence %>% 
      filter(ID %in% vagrant_filt$ID) %>% 
      mutate(ID = NULL)
    
    rm(vagrant_filt)
    
    
    # 3. remove false presences (edge cases: reported from same cell but point is outside current state)
    occ_final <- occ_final %>% rangemap_rm_falsepres(cur_mask, specieslist)
    
  }
  
  
  # error check for species name
  if (str_flatten_comma(which_spec) != "all") {
    
    correct_specnames <- specieslist %>% 
      distinct(COMMON.NAME) %>% 
      pull(COMMON.NAME)
    
    if (any(!which_spec %in% correct_specnames)) {
      return(c("Range map can only be plotted for valid species! (Check appropriate 'specieslist'.)",
               "Or, incorrect species name provided! Use the correct eBird taxonomy name."))
    }
    
    rm(correct_specnames)
    
  }
  
  
  # plot/theme settings -----------------------------------------------------
  
  palette_plot_elem <- "#56697B"
  palette_plot_title <- "#A13E2B"
  
  palette_range_groups <- data.frame(
    LABEL = c("Year-round", "Summer", "Spring/Autumn", "Winter"),
    COLOUR = c("#562377", "#dc6f42", "#e4b73e", "#00858f"),
    # useful for joining later
    LABEL.CODE = c("YR", "S", "P", "W"),
    LABEL.ALT = c("Year-round", "Summer", "Passage", "Winter")
  ) %>% 
    mutate(across(everything(), ~ fct_inorder(.)))
  
  plot_fontfamily <- "Gandhi Sans"
  
  
  # assigning objects to environment --------------------------------------------------
  
  obj_list <- list(palette_plot_elem = palette_plot_elem,
                   palette_plot_title = palette_plot_title,
                   palette_range_groups = palette_range_groups,
                   plot_fontfamily = plot_fontfamily)
  
  list2env(obj_list, envir = .GlobalEnv)
  
  # creating the plot base ------------------------------------------
  
  if (cur_metadata$MASK.TYPE == "country") {
    
    plot_base <- ggplot() +
      geom_raster(data = map_dem_in, aes(x = x, y = y, fill = codes), 
                  alpha = 0.3) +
      scale_fill_grey(na.value = "transparent", guide = "none")
    
  } else if (cur_metadata$MASK.TYPE == "state") {
    
    # the DEM resolution is not high enough; becomes very pixelated for small states
    # plus, doesn't give much insight. So, using uniform grey fill instead.
    
    plot_base <- ggplot() +
      geom_sf(data = cur_state_sf, col = NA, fill = "grey80", alpha = 0.6)
    
  }
  
  plot_base <- plot_base +
    scale_x_continuous(expand = c(0,0)) +
    scale_y_continuous(expand = c(0,0))
  
  
  # completing and writing the plot -----------------------------------------------------
  
  if (str_flatten_comma(which_spec) != "all") {
    cur_spec <- which_spec
  } else {
    cur_spec <- specieslist %>% distinct(COMMON.NAME) %>% pull(COMMON.NAME)
  }
  
  # deciding whether to walk or future-walk (parallel) based on number of iterations required
  # (name: https://onepiece.fandom.com/wiki/Haki/Kenbunshoku_Haki#Future_Vision)
  # thresholds set based on testing done
  min_tresh <- ifelse(cur_metadata$MASK.TYPE == "state", 25, 5)
  advanced_kenbunshoku <- if (length(cur_spec) >= min_tresh) TRUE else FALSE
  
  # extra error catch step
  if (any(!cur_spec %in% specieslist$COMMON.NAME)) {
    
    return("Range map can only be plotted for valid species! (Check appropriate 'specieslist'.)")
    
  } else {
    
    # walking creation of maps over all specified species
    to_walk <- function(.x, haki = FALSE) {
      
      if (!haki) {
        tic("Walked over one species")
      }
      
      # filtering data for species
      cur_data_occ = occ_final %>% 
        filter(COMMON.NAME == .x) %>% 
        left_join(palette_range_groups, by = c("status" = "LABEL.CODE")) %>% 
        right_join(cur_g1_sf, by = c("gridg1" = "GRID.G1")) %>% 
        st_as_sf()
      
      cur_data_vag = vagrant_presence %>% 
        filter(COMMON.NAME == .x) %>% 
        left_join(palette_range_groups, by = c("status" = "LABEL.CODE"))
      
      # in some cases, one migratory status present in one of the two (occupied vs vagrant)
      # we want legend to reflect all present
      all_statuses <- bind_rows(st_drop_geometry(cur_data_occ), cur_data_vag) %>% 
        distinct(LABEL, COLOUR)
      
      
      # output paths (one structured into subfolders, one without structure for website)
      
      # convert to India Checklist names 
      converted_spec <- specname_to_india_checklist(.x, already_show = FALSE)
        
      
      web_spec <- str_replace_all(converted_spec, c(" " = "-", "'" = "_"))

      # for structured
      path_map <- glue("{cur_metadata$MAP.FOLDER}{web_spec}_{cur_metadata$MASK.CODE}_map_2023.png")
      
      # for website
      path_map_web <- glue("{cur_metadata$WEB.MAP.FOLDER}{web_spec}_{cur_metadata$MASK.CODE}_map_2023.jpg")
      
      
      # joining plot base with other constant aesthetic features of graph
      cur_plot <- plot_base +
        new_scale_fill() +
        geom_rangemap_occ(cur_data_occ, admin_sf, cur_data_vag) +
        # using identity scale since we have specified the column of hexcodes in aes of geom
        scale_alpha_identity() +
        scale_fill_identity(guide = guide_legend(title = NULL),
                            labels = levels(all_statuses$LABEL),
                            breaks = levels(all_statuses$COLOUR)) +
        scale_colour_identity() +
        # theme
        ggtheme_soibrangemap()
      
      
      # writing maps
      ggsave(filename = path_map, plot = cur_plot,
             dpi = 1000, bg = "white",
             width = 7, height = 7, units = "in")
      
      ggsave(filename = path_map_web, plot = cur_plot,
             dpi = 1000, bg = "white",
             width = 7, height = 7, units = "in")
      
      if (!haki) {
        toc()
      }
      
    }
    
    # deciding whether to walk or future-walk (parallel) based on number of iterations required
    if (advanced_kenbunshoku) {
      
      print(glue("Activated future-walking using advanced Kenbunshoku Haki!"))
      
      tic(glue("Future-walked over {length(cur_spec)} species"))

      # start multiworker parallel session
      plan(multisession, workers = parallel::detectCores()/2)
      
      future_walk(cur_spec, .progress = TRUE, ~ to_walk(.x, advanced_kenbunshoku))
      
      # end multiworker parallel session
      plan(sequential)
      
      toc()
      
    } else {
      
      walk(cur_spec, ~ to_walk(.x))
      
    }
      
  }
  
  
  # removing objects from global environment ------------------------------------------
  
  rm(list = names(obj_list), envir = .GlobalEnv)
  
  toc()
  
}
