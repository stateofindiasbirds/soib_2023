# various trends plots --------------------------------------------------------------

# this function has a heavy focus on plot_type, not much on cur_spec
# (even Kenbunshoku Haki is based on number of species within plot_type & cur_trend)
# plot_load_filter_data() does not offer species argument

gen_trend_plots <- function(plot_type = "single", 
                            cur_trend = NULL, cur_spec = NULL) {
  
  # "cur_spec" in eBird name
  
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
  require(ggrepel) # text repel
  require(tictoc)
  require(furrr)
  require(parallel)
  
  source('00_scripts/00_functions.R')
  source('00_scripts/00_plot_functions.R')
  
  # assigning objects to environment --------------------------------------------------
  
  if (!(plot_type %in% c("multi", "composite"))) {
    obj_list <- list(plot_type = plot_type, 
                     cur_trend = cur_trend,
                     cur_spec = cur_spec,
                     analyses_metadata = get_metadata())
  } else {
    obj_list <- list(plot_type = plot_type, 
                     analyses_metadata = get_metadata())
  }
  
  list2env(obj_list, envir = .GlobalEnv)
  
  
  # import metadata and data, filter for qual. species, generate plots -----------------
  
  if (plot_type == "single") {
    
    plot_load_filter_data(plot_type, cur_trend)
    
    # generating plots if there are any qualified
    
    # skip species-mask combo if species is specified but not qualified for trend
    skip_spec_dq <- if (!is.null(cur_spec) & cur_spec != "all") {
      !(cur_spec %in% spec_qual)
    } else FALSE
    
    if (length(spec_qual) != 0 & skip_spec_dq == FALSE) {
      
      # deciding whether to walk or future-walk (parallel) based on number of iterations required
      # (name: https://onepiece.fandom.com/wiki/Haki/Kenbunshoku_Haki#Future_Vision)
      advanced_kenbunshoku <- if (length(spec_qual) >= 5) TRUE else FALSE
      
      if (cur_spec == "all") {
        
        to_walk <- function(.x, advanced_kenbunshoku) {
          soib_trend_plot(plot_type = plot_type, 
                          cur_trend = cur_trend, 
                          cur_spec = .x,
                          data_trends = data_trends,
                          data_main = data_main,
                          path_write = path_write,
                          cur_plot_metadata = web_metadata, 
                          haki = advanced_kenbunshoku)
        }
        
        if (advanced_kenbunshoku) {
          print(glue("Activated future-walking using advanced Kenbunshoku Haki!"))
          tic(glue("Future-walked over {length(spec_qual)} species (plotted {plot_type} for all species)"))
          plan(multisession, workers = parallel::detectCores()/2)
          future_walk(spec_qual, .progress = TRUE, ~ to_walk(.x, advanced_kenbunshoku))
          plan(sequential)
          toc()
        } else {
          tic(glue("Walked over {length(spec_qual)} species (plotted {plot_type} for all species)"))
          walk(spec_qual, ~ to_walk(.x, advanced_kenbunshoku))
          toc()
        }
        
      } else {
        soib_trend_plot(plot_type = plot_type, 
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
             
             # skip species-mask combo if species is specified but not qualified for trend
             skip_spec_dq <- if (!is.null(cur_spec) & cur_spec != "all") {
               !(cur_spec %in% spec_qual)
             } else FALSE
               
             if (length(spec_qual) != 0 & skip_spec_dq == FALSE) {
               
               advanced_kenbunshoku <- if (length(spec_qual) >= 5) TRUE else FALSE
               
               cur_mask <- .x
               
               if (cur_spec == "all") {
                 
                 to_walk <- function(.x, advanced_kenbunshoku) {
                   soib_trend_plot(plot_type = plot_type,
                                   cur_trend = cur_trend,
                                   cur_spec = .x,
                                   data_trends = data_trends,
                                   data_main = data_main,
                                   path_write = path_write,
                                   cur_plot_metadata = web_metadata, 
                                   haki = advanced_kenbunshoku)
                 }

                 if (advanced_kenbunshoku) {
                   print(glue("Activated future-walking using advanced Kenbunshoku Haki!"))
                   tic(glue("Future-walked over {length(spec_qual)} species (plotted {plot_type} for all species in {cur_mask})"))
                   plan(multisession, workers = parallel::detectCores()/2)
                   future_walk(spec_qual, .progress = TRUE, ~ to_walk(.x, advanced_kenbunshoku))
                   plan(sequential)
                   toc()
                 } else {
                   tic(glue("Walked over {length(spec_qual)} species (plotted {plot_type} for all species in {cur_mask})"))
                   walk(spec_qual, ~ to_walk(.x, advanced_kenbunshoku))
                   toc()
                 }
                 
               } else {
                 soib_trend_plot(plot_type = plot_type,
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
      
      soib_trend_plot(plot_type = plot_type,
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
                             eBird.English.Name.2024, SoIB.Latest.Priority.Status) %>% 
               filter(eBird.English.Name.2024 %in% spec_qual) %>% 
               rename(PLOT.SPEC = eBird.English.Name.2024)
             
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
             
             soib_trend_plot(plot_type = plot_type,
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
  
  # cleaning environment ------------------------
  
  rm(list = setdiff(ls(envir = .GlobalEnv), "gen_trend_plots"), 
     envir = .GlobalEnv)
  
}


# various trend plots (sysmon) ------------------------------------------------------

# needs to be integrated with above; leaving separate for want of time

gen_trend_plots_sysmon <- function(cur_case) {
  
  # setup -------------------------------------------------------
  
  require(tidyverse)
  require(ggpubr) # geom_bracket
  require(extrafont)
  require(glue)
  require(ggrepel) # text repel
  require(tictoc)
  
  source('00_scripts/00_functions.R')
  source('00_scripts/00_plot_functions.R')
  
  # import and prepare data for plot --------------------------------------------------
  
  fetch_sysmon_metadata(cur_case)
  
  # caching output path because it gets overwritten in some steps
  path_out_cache <- path_out
  
  
  if (cur_case == "bustards") {
    
    data_trends <- read.csv(path_data) %>% 
      # rename columns to match eBird data names
      rename(mean_std = mean) %>% 
      # creating CI columns even though we are not plotting it
      mutate(lci_std = mean_std, rci_std = mean_std) %>% 
      # convert to India Checklist name
      mutate(COMMON.NAME = specname_to_india_checklist(COMMON.NAME))

  } else if (cur_case == "vembanad") {
    
    data_trends <- read.csv(path_data) %>% 
      rename(mean_std = mean) %>% 
      # convert to India Checklist name
      mutate(COMMON.NAME = specname_to_india_checklist(COMMON.NAME))
    
    data_trends_extra <- read.csv(path_data_extra) %>% 
      rename(mean_std = mean) %>% 
      mutate(COMMON.NAME = "Total")
    
  } else {
    
    data_trends <- read.csv(path_data) %>% 
      rename(mean_std = mean, lci_std = cil, rci_std = cir) %>% 
      # eBird names changed in the meantime
      mutate(COMMON.NAME = case_when(
        COMMON.NAME == "Black-headed Mountain-Finch" ~ "Black-headed Mountain Finch",
        COMMON.NAME == "Plain Mountain-Finch" ~ "Plain Mountain Finch",
        TRUE ~ COMMON.NAME
      )) %>% 
      # convert to India Checklist name
      mutate(COMMON.NAME = specname_to_india_checklist(COMMON.NAME))
    
    if (cur_case == "spiti") {
      
      data_trends <- data_trends %>% 
        filter(!is.na(mean_std), 
               !((rci_std - mean_std) > 0.3 & rci_std / mean_std > 2))
      
      # habitats are already called COMMON.NAME
      data_trends_extra <- read.csv(path_data_extra) %>% 
        rename(mean_std = mean, lci_std = cil, rci_std = cir) %>% 
        filter(!is.na(mean_std), 
               !((rci_std - mean_std) > 0.3 & rci_std / mean_std > 2))
      
    }
    
  }
  
  # plotting composite  -----------------------------------------------------------
  
  if (!cur_case %in% c("spiti", "vembanad")) {
    
    soib_trend_plot_sysmon(plot_type = cur_case, cur_data_trends = data_trends,
                    analysis_type = "sysmon")
    
  } else if (cur_case == "vembanad") {
    
    data_trends_filt <- data_trends %>%
      filter(COMMON.NAME %in% c("Black-headed Ibis", "Glossy Ibis", "Whiskered Tern"))
    
    soib_trend_plot_sysmon(plot_type = cur_case, cur_data_trends = data_trends_filt,
                    analysis_type = "sysmon")
    
  } else {
    
    soib_trend_plot_sysmon(plot_type = cur_case, cur_data_trends = data_trends_extra,
                    analysis_type = "sysmon")
    
  }
  
  
  # plotting single-species in some cases ---------------------------------------------

  if (cur_case %in% c("nannaj", "spiti", "vembanad")) {
    
    assign("path_all", path_out, envir = .GlobalEnv)
    
    data_trends %>% 
      distinct(COMMON.NAME) %>% 
      pull(COMMON.NAME) %>% 
      walk(., ~ {
        
        data_trends_filt <- data_trends %>% filter(COMMON.NAME == .x)
        
        name_to_save <- .x %>% 
          specname_to_india_checklist() %>% 
          str_replace_all(c(" " = "-"))
        
        # single species out path
        path_out <- str_replace(path_all, ".png", glue("_{name_to_save}.png"))
        assign("path_out", path_out, envir = .GlobalEnv)
        
        soib_trend_plot_sysmon(plot_type = cur_case, cur_data_trends = data_trends_filt,
                        analysis_type = "sysmon")
      })
    
  }
  

  # vembanad plot of total counts -----------------------------------------------------

  if (cur_case == "vembanad") {
    
    path_out <- str_replace(path_out_cache, ".png", glue("_tot.png"))
    assign("path_out", path_out, envir = .GlobalEnv)
    
    soib_trend_plot_sysmon(plot_type = cur_case, cur_data_trends = data_trends_extra,
                    analysis_type = "sysmon")
    
  }
  

  # cleaning environment ------------------------
  
  rm(list = setdiff(ls(envir = .GlobalEnv), "gen_trend_plots_sysmon"), 
     envir = .GlobalEnv)
  
  
}


# range maps --------------------------------------------------------------

gen_range_maps <- function(mask_type = "country", which_mask = NULL, which_spec = "all") {
  
  require(tidyverse)
  require(glue)
  require(tictoc)

  source('00_scripts/00_functions.R')
  source('00_scripts/00_plot_functions.R')

      
  # error checks ----------------------------------------------------------------------
  
  which_metadata <- get_metadata() %>% filter(MASK.TYPE == mask_type)
  
  if (!mask_type %in% c("country", "state")) {
    return("Select either a country or a state! Range maps cannot be created for habitat or CA masks.")
  }
  
  # if both mask type and mask specified, avoid mismatches
  if (!is.null(which_mask)) {
    
    list_states <- get_metadata() %>% filter(MASK.TYPE == "state") %>% pull(MASK)
    
    if ((mask_type == "country" & which_mask != "none") |
        (mask_type == "state" & !(which_mask %in% list_states))) {
      return("Incorrect 'which_mask' specification! Leave as default (NULL), or: Mask type 'country' needs to be paired with mask 'none'. Mask type 'state' needs to be paired with a valid state mask.")
    }
    
  }
  
  
  # setup + map creation --------------------------------------------------------------

  if (is.null(which_mask)) {
    
    which_mask <- which_metadata %>% 
      distinct(MASK) %>% 
      pull(MASK)
    
    tic(glue("Finished plotting {mask_type} maps for all relevant masks."))
    
  } else {
    
    tic(glue("Finished plotting {mask_type} maps for specified mask {which_mask}."))
    
  }
  
  walk(which_mask, ~ {

    # setup data (only if don't exist) --------------------------------------------------

    # setting up data for range maps 
    # (this only runs when the output CSVs don't already exist!)
    
    which_metadata <- get_metadata(.x)
    
    # input paths
    path_speclists <- which_metadata$SPECLISTDATA.PATH
    path_main <- which_metadata$SOIBMAIN.PATH
    path_toplot <- which_metadata$MAP.DATA.OCC.PATH
    path_vagrants <- which_metadata$MAP.DATA.VAG.PATH
    path_occu_pres <- which_metadata$OCCU.PRES.PATHONLY
    path_occu_mod <- which_metadata$OCCU.MOD.PATHONLY
    
    
    if (!(!file.exists(path_toplot) | !file.exists(path_vagrants))) {
      
      print(glue("[Mask: {.x}] Data required to plot range maps already exists. Skipping setup steps."))
      
    } else {
      # load data -------------------------------------------------------------------------
      
      load(path_speclists)
      load("00_data/vagrantdata.RData")
      load("00_data/dataforanalyses_extra.RData")
      
      load("00_data/grids_sf_nb.RData")
      our_neighbours <- g1_nb_q
      rm(g1_nb_r, g2_nb_q, g2_nb_r, g3_nb_q, g3_nb_r, g4_nb_q, g4_nb_r)
      
      
      list_mig = read.csv(path_main) %>% 
        dplyr::select(eBird.English.Name.2024, Migratory.Status.Within.India) %>%
        filter(Migratory.Status.Within.India != "Resident", 
               eBird.English.Name.2024 %in% specieslist$COMMON.NAME) %>%
        distinct(eBird.English.Name.2024) %>% 
        pull(eBird.English.Name.2024)
      
      
      # setup -----------------------------------------------------------------------------

      # later for plotting state-level range maps, we need info on which grids each species 
      # has been reported from. saving that information here, to be read in in plotting.
      info_state_spec_grid <- data0 %>% 
        distinct(ST_NM, COMMON.NAME, gridg1) %>% 
        arrange(ST_NM, COMMON.NAME, gridg1)
      
      save(info_state_spec_grid, file = "01_analyses_full/data_rangemap_info4state.RData")
      
      
      
      data0 = data0 %>% 
        filter(COMMON.NAME %in% list_mig, 
               year > (soib_year_info("latest_year") - 5)) %>% 
        dplyr::select(COMMON.NAME, day, gridg1)
      
      # defining summer, winter, passage
      datas = data0 %>% 
        filter(day > 145 & day <= 215) %>% 
        distinct(COMMON.NAME, gridg1) %>%
        mutate(status = "S")
      dataw = data0 %>% 
        filter(day <= 60 | day > 325) %>% 
        distinct(COMMON.NAME, gridg1) %>%
        mutate(status = "W")
      datap = data0 %>% 
        filter((day > 60 & day <= 145) | (day > 215 & day <= 325)) %>% 
        distinct(COMMON.NAME, gridg1) %>%
        mutate(status = "P")
      
      data_presence = datas %>% 
        bind_rows(dataw, datap) %>%
        dplyr::select(COMMON.NAME, status, gridg1) %>%
        mutate(prop_nb = NA, 
               occupancy = 1,
               gridg1 = as.numeric(gridg1),
               status = factor(status, levels = c("S","W","P")))
      
      
      # vagrants

      d = d %>% 
        filter(COMMON.NAME %in% list_mig, 
               year > (soib_year_info("latest_year") - 5)) %>% 
        # subset for state when required
        dplyr::select(COMMON.NAME, day, LATITUDE, LONGITUDE)
      
      ds = d %>% 
        filter(day > 145 & day <= 215) %>% 
        distinct(COMMON.NAME, LATITUDE, LONGITUDE) %>%
        mutate(status = "S")
      dw = d %>% 
        filter(day <= 60 | day > 325) %>% 
        distinct(COMMON.NAME, LATITUDE, LONGITUDE) %>%
        mutate(status = "W")
      dp = d %>% 
        filter((day > 60 & day <= 145) | (day > 215 & day <= 325)) %>% 
        distinct(COMMON.NAME, LATITUDE, LONGITUDE) %>%
        mutate(status = "P")
      
      vagrant_presence = ds %>% bind_rows(dw, dp)
      
      
      # the following code is to create a file that provides the proportional occupancy
      # for each cell in each relevant season for each species
      # occ.model files 
      occ.model <- list.files(path = path_occu_mod, full.names = T) %>% 
        map_df(read.csv)
      
      occ.model.resident = occ.model %>%
        filter(prop_nb != 0, 
               presence == 0, 
               !COMMON.NAME %in% list_mig) %>%
        dplyr::select(COMMON.NAME, status, gridg1, prop_nb, occupancy)
      
      occ.model.migrant = occ.model %>%
        filter(prop_nb != 0, 
               presence == 0, 
               COMMON.NAME %in% list_mig) %>%
        dplyr::select(COMMON.NAME, status, gridg1, prop_nb, occupancy) %>%
        mutate(status = NA)
      
      
      # occ.presence files 
      occ.presence <- list.files(path = path_occu_pres, full.names = T) %>% 
        map_df(read.csv)
      
      listM = occ.presence %>%
        filter(status == "M") %>% 
        distinct(COMMON.NAME) %>%
        pull(COMMON.NAME)
      
      occ.presence.resident = occ.presence %>%
        filter(!COMMON.NAME %in% list_mig) %>%
        dplyr::select(COMMON.NAME, status, gridg1) %>%
        mutate(prop_nb = NA, occupancy = 1)
      
      
      # combining presence-based and modelled for residents
      occ.resident = occ.model.resident %>% bind_rows(occ.presence.resident)
      
      # combining presence-based and modelled for migrants
      # this is to determine the migratory status of each cell without confirmed presence
      # the most frequent neighbouring status will be the assigned status for that cell
      for (i in list_mig) {
        
        temp = occ.model.migrant %>% filter(COMMON.NAME == i)
        temp.dat = data_presence %>% filter(COMMON.NAME == i)
        
        if (length(temp$COMMON.NAME) > 0) {
          
          for (j in unique(temp$gridg1))
          {
            nbs = our_neighbours[[j]]
            
            stats = temp.dat %>%
              filter(gridg1 %in% nbs) %>%
              count(status, sort = TRUE) %>%
              slice_max(n, with_ties = FALSE) %>%
              pull(status)
            
            occ.model.migrant$status[occ.model.migrant$COMMON.NAME == i & 
                                       occ.model.migrant$gridg1 == j] = as.character(stats)
          }
          
        }
        
        print(i)
        
      }
      
      occ.migrant = occ.model.migrant %>% bind_rows(data_presence)
      
      
      occ.full = occ.resident %>%
        bind_rows(occ.migrant) %>%
        # remove duplicates that come in from incidentals
        group_by(COMMON.NAME, status, gridg1) %>%
        arrange(desc(occupancy), .by_group = T) %>% 
        slice(1) %>% 
        ungroup() %>%
        arrange(COMMON.NAME, status, desc(occupancy))
      
      # Change everything to the four statuses of interest
      occ_final = occ.full %>%
        group_by(COMMON.NAME, gridg1) %>%
        reframe(occupancy = max(occupancy),
                status = case_when(
                  any(status == "R") ~ "YR",
                  any(status == "S") & any(status == "W") & !COMMON.NAME %in% listM ~ "YR",
                  any(status == "S") ~ "S",
                  any(status == "W") ~ "W",
                  TRUE ~ "P"
                )) %>% 
        distinct(COMMON.NAME, gridg1, status, occupancy)
      
      
      # write
      write.csv(occ_final, path_toplot, row.names = FALSE)
      write.csv(vagrant_presence, path_vagrants, row.names = FALSE)
      
    }
    
    
    # generate maps --------------------------------------------------------------------

    soib_rangemap(which_spec = which_spec, cur_mask = .x)
    
  })
  
  toc()
  
  
  # cleaning environment ------------------------
  
  rm(list = setdiff(ls(envir = .GlobalEnv), "gen_range_maps"), 
     envir = .GlobalEnv)
  
}

