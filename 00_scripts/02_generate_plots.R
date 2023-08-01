
# various trends plots --------------------------------------------------------------

gen_trend_plots <- function(plot_type = "single", 
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
  source('00_scripts/00_plot_functions')
  
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

