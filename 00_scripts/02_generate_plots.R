
# various trends plots --------------------------------------------------------------

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
  
  source('00_scripts/00_functions.R')
  source('00_scripts/00_plot_functions.R')
  
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
        tic(glue("Finished plotting {plot_type} for all species"))
        walk(spec_qual, ~ soib_trend_plot(plot_type = plot_type, 
                                          cur_trend = cur_trend, 
                                          cur_spec = .x,
                                          data_trends = data_trends,
                                          data_main = data_main,
                                          path_write = path_write,
                                          cur_plot_metadata = web_metadata))
        toc()
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
             if (length(spec_qual) != 0) {
               
               if (cur_spec == "all") {
                 tic(glue("Finished plotting {plot_type} for all species"))
                 walk(spec_qual, ~ soib_trend_plot(plot_type = plot_type,
                                                   cur_trend = cur_trend,
                                                   cur_spec = .x,
                                                   data_trends = data_trends,
                                                   data_main = data_main,
                                                   path_write = path_write,
                                                   cur_plot_metadata = web_metadata))
                 toc()
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
      mutate(lci_std = mean_std, rci_std = mean_std)

  } else if (cur_case == "vembanad") {
    
    data_trends <- read.csv(path_data) %>% rename(mean_std = mean)
    data_trends_extra <- read.csv(path_data_extra) %>% 
      rename(mean_std = mean) %>% 
      mutate(COMMON.NAME = "Total")
    
  } else {
    
    data_trends <- read.csv(path_data) %>% 
      rename(mean_std = mean, lci_std = cil, rci_std = cir)
    
    if (cur_case == "spiti") {
      
      data_trends <- data_trends %>% 
        filter(!is.na(mean_std), 
               !((rci_std - mean_std) > 0.3 & rci_std / mean_std > 2)) %>% 
        # eBird names changed in the meantime
        mutate(COMMON.NAME = case_when(
          COMMON.NAME == "Black-headed Mountain-Finch" ~ "Black-headed Mountain Finch",
          COMMON.NAME == "Plain Mountain-Finch" ~ "Plain Mountain Finch",
          TRUE ~ COMMON.NAME
        ))
      
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

