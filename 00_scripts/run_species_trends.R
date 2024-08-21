# preparing data for specific mask (this is the only part that changes, but automatically)
cur_metadata <- get_metadata(cur_mask)
speclist_path <- cur_metadata$SPECLISTDATA.PATH
databins_path <- cur_metadata$DATA.PATH # for databins

# don't run if no species selected
load(speclist_path)
to_run <- (1 %in% specieslist$ht) | (1 %in% specieslist$rt) |
  (1 %in% restrictedspecieslist$ht) | (1 %in% restrictedspecieslist$rt)


# singleyear = interannualupdate
singleyear = FALSE
# not using single year modelling approach, since test runs showed that
# single year models produce notably higher estimates than full-year models


if (to_run == TRUE) {

  
  # for the full country analysis, runs are split among multiple systems, and use
  # separate subsampled datasets. We need to ensure this information exists.
  # else, all 1000 runs are on one system.
  if (cur_mask == "none") {
    
    if (!exists("my_assignment")) {
      return("'my_assignment' is empty! Please specify IDs of data files assigned to you.")
    }
    
    cur_assignment <- my_assignment
    
  } else {
    
    if (!exists("my_assignment")) {
      cur_assignment <- 1:1000
    } else {
      cur_assignment <- my_assignment
    }
    
  }
  
  ###
  
  require(tidyverse)
  require(lme4)
  require(VGAM)
  require(parallel)
  require(foreach)

  # from https://stackoverflow.com/a/38371601/13000254
  # also see Linux-specific cases https://stackoverflow.com/q/37750937/13000254
  if(Sys.info()["sysname"]=="Windows"){

    sys_windows <- TRUE

    if (!("doParallel" %in% installed.packages()[,"Package"])) {
      install.packages("doParallel")
    }

    require(doParallel)

  } else {

    sys_windows <- FALSE

    if (!("doMC" %in% installed.packages()[,"Package"])) {
      install.packages("doMC")
    }

    require(doMC)

  }

  source('00_scripts/00_functions.R')
  
  
  load(speclist_path)
  load(databins_path)
  rm(data)
  
  lsa = specieslist %>% filter(!is.na(ht) | !is.na(rt))
  listofspecies = c(lsa$COMMON.NAME, restrictedspecieslist$COMMON.NAME)
  speclen = length(listofspecies)
  
  # creating new directory if it doesn't already exist
  if (!dir.exists(cur_metadata$TRENDS.PATHONLY)) {
    dir.create(cur_metadata$TRENDS.PATHONLY, 
               recursive = T)
  }
  
  for (k in cur_assignment)
  {
    
    # file names for individual files
    write_path <- cur_metadata %>% 
      dplyr::summarise(TRENDS.PATH = glue("{TRENDS.PATHONLY}trends_{k}.csv")) %>% 
      as.character()
    
    data_path = cur_metadata %>% 
      dplyr::summarise(SIMDATA.PATH = glue("{SIMDATA.PATHONLY}data{k}.RData")) %>% 
      as.character()
    
    
    tictoc::tic(glue("Species trends for {cur_mask}: {k}/{max(cur_assignment)}"))
    
    # read data files
    load(data_path)
    
    cols_temp <- if (singleyear == FALSE) {
      c("gridg1", "gridg2", "gridg3", "gridg4", "month", "timegroups")
    } else if (singleyear == TRUE) {
      c("gridg1", "gridg2", "gridg3", "gridg4", "month")
    }

    data <- data_filt %>% 
      mutate(across(.cols = all_of(cols_temp), ~ as.factor(.))) %>% 
      mutate(gridg = gridg3)

    rm(cols_temp)
    
    
    
    # start parallel
    n.cores = parallel::detectCores()/2


    # create the cluster
    if (sys_windows == TRUE) {

      my.cluster = parallel::makeCluster(
        n.cores, 
        type = "PSOCK"
      )
      # register it to be used by %dopar%
      doParallel::registerDoParallel(cl = my.cluster)

    } else {
      doMC::registerDoMC(n.cores)
    }
    
    # # check if it is registered (optional)
    # foreach::getDoParRegistered()
    # # how many workers are available? (optional)
    # foreach::getDoParWorkers()
    
    trends0 = foreach(i = listofspecies, 
                      # .verbose = TRUE,
                      .combine = 'cbind', .errorhandling = 'remove') %dopar%
      singlespeciesrun(data = data, 
                       species = i, 
                       specieslist = specieslist, 
                       restrictedspecieslist = restrictedspecieslist,
                       singleyear = singleyear)
    
    if (sys_windows == TRUE) {
      parallel::stopCluster(cl = my.cluster)
    }
    

    trends = data.frame(trends0) %>% 
      # converting first row of species names (always true) to column names
      magrittr::set_colnames(.[1,]) %>% 
      slice(-1) %>% 
      {if (singleyear == FALSE) {
        
        mutate(.,
                timegroupsf = rep(databins$timegroups, 2),
                timegroups = rep(databins$year, 2),
                type = rep(c("freq", "se"), 
                            # will always have 2*N.YEAR rows (freq, se)
                            each = length(soib_year_info("timegroup_lab"))),
                sl = k) %>%  # sim number
        # pivoting species names longer
        pivot_longer(-c(timegroups, timegroupsf, sl, type), 
                      values_to = "value", names_to = "COMMON.NAME") %>% 
        pivot_wider(names_from = type, values_from = value) %>% 
        # numerical ID for species names, for arranging
        mutate(sp = row_number(), .by = timegroupsf) %>%
        arrange(sl, sp) %>%
        dplyr::select(-sp) %>% 
        # reordering
        relocate(sl, COMMON.NAME, timegroupsf, timegroups, freq, se)
            
      } else if (singleyear == TRUE) {

        mutate(.,
                type = rep(c("freq", "se"), 
                            each = 1),
                sl = k) %>%  # sim number
        # pivoting species names longer
        pivot_longer(-c(sl, type), 
                      values_to = "value", names_to = "COMMON.NAME") %>% 
        pivot_wider(names_from = type, values_from = value) %>% 
        # numerical ID for species names, for arranging
        mutate(sp = row_number()) %>%
        arrange(sl, sp) %>%
        dplyr::select(-sp) %>% 
        # reordering
        relocate(sl, COMMON.NAME, freq, se) |> 
        # bringing back timegroups columns
        mutate(timegroups = soib_year_info("latest_year"),
               timegroupsf = as.character(soib_year_info("latest_year")))

      }} %>% 
      # make sure freq and se are numerical
      mutate(across(c("freq", "se"), ~ as.numeric(.)))
    
    
    # if full run, overwrite the CSV
    # else append single year results to all previous year results
    if (singleyear == FALSE) {

      write.csv(trends, file = write_path, row.names = FALSE)

    } else if (singleyear == TRUE) {

      trends_old <- read.csv(write_path, header = TRUE) |> 
        filter(timegroups != soib_year_info("latest_year"))

      trends_new <- trends_old |> 
        bind_rows(trends) |> 
        left_join(specieslist |> 
                    mutate(order = row_number(),
                            ht = NULL,
                            rt = NULL), 
                  by = "COMMON.NAME") |> 
        arrange(sl, order, timegroups) |> 
        dplyr::select(-order)

      write.csv(trends_new, file = write_path, row.names = FALSE)

    }

    
    tictoc::toc() 
    
    # maybe unnecessary time-consuming step:
    # worth trying the profiling mentioned here http://adv-r.had.co.nz/memory.html
    # https://stackoverflow.com/questions/1467201/forcing-garbage-collection-to-run-in-r-with-the-gc-command
    # gc()
    
    }
  
} else {
  
  print(glue("Skipping running species trends for {cur_mask}"))
  
}