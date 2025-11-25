library(tidyverse)
library(lme4)
library(VGAM)
library(parallel)

hostname <- paste0(Sys.info()["nodename"],"")

# preparing data for specific mask (this is the only part that changes, but automatically)
cur_metadata <- get_metadata(cur_mask, container)
if(container) {
  data_prefix = "data/"
} else {
  data_prefix = ""
}
speclist_path <- paste0(data_prefix, cur_metadata$SPECLISTDATA.PATH)
databins_path <- paste0(data_prefix, cur_metadata$DATA.PATH) # for databins

get_free_ram <- function() {
#               total        used        free      shared  buff/cache   available
#Mem:        65572748     1544972    62672624        2976     1995520    64027776
#Swap:        8388604           0     8388604
#"available" is how much more RAM we can be use without swapping
  ram <- system("free | awk '/Mem:/ {print $7}'", intern = TRUE)
  ram <- as.integer(ram)*1024
  return(ram)
}

# don't run if no species selected
message(paste("Loading:",speclist_path))
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

  source('00_scripts/00_functions.R')
  
  databins_path_metadata <- paste0(databins_path,'-metadata')
  message(paste("Loading:",databins_path_metadata))
  load(paste(databins_path_metadata))

  databins_path_data <- paste0(databins_path,'-data_opt')
  tic(paste("Loading:",databins_path_data))
  load(paste(databins_path_data)) # will create "data"
  toc()

  lsa = specieslist %>% filter(!is.na(ht) | !is.na(rt))
  listofspecies = c(lsa$COMMON.NAME, restrictedspecieslist$COMMON.NAME)
  speclen = length(listofspecies)
  
  # creating new directory if it doesn't already exist
  if (!dir.exists(cur_metadata$TRENDS.PATHONLY)) {
    dir.create(cur_metadata$TRENDS.PATHONLY, 
               recursive = T)
  }

  # Load common data
  basedir <- dirname(databins_path)
  species_names_path <- paste0(basedir, "/species_names.RData")
  timegroups_path <- paste0(basedir, "/timegroups.RData")
  message("Loading: ", species_names_path)
  load(species_names_path)
  message("Loading: ", timegroups_path)
  load(timegroups_path)

  run_stats_path <- paste0(dirname(databins_path),'/species_run_stats.RData')
  have_run_stats <- FALSE
  if(file.exists(run_stats_path)) {
    message(paste("Loading:", run_stats_path))
    load(run_stats_path)
    have_run_stats <- TRUE
  } else {
    message("No run stats, can't optimize")
  }

  # delete coulumns gridg2 and gridg4
  data$OBSERVER.ID <- NULL

  for (k in cur_assignment)
  {
    message("========================================")
    message(paste("Starting assignment:", k))
    message("========================================")
    if(container) {
      trends_species_dir <- paste0("output/", cur_mask, "/", hostname,"/", k, "/species")
    } else {
      trends_species_dir <- cur_metadata %>%
        dplyr::summarise(TRENDS.PATH = glue("{TRENDS.PATHONLY}/species_{k}")) %>%
        as.character()
    }
    trends_stats_dir <- paste0(trends_species_dir,"/stats")
    # creating new directory if it doesn't already exist
    if (!dir.exists(trends_stats_dir)) {
      dir.create(trends_stats_dir,
                 recursive = T)
    }
    
    # file names for individual files
    if(container) {
      write_path <- cur_metadata %>%
        dplyr::summarise(TRENDS.PATH = glue("output/{cur_mask}/{hostname}/{k}/trends_{k}.csv")) %>%
        as.character()
    } else {
      write_path <- cur_metadata %>%
        dplyr::summarise(TRENDS.PATH = glue("{TRENDS.PATHONLY}trends_{k}.csv")) %>%
        as.character()
    }

    if(file.exists(write_path)) {
      if(!force_trends_computation) {
        message("Result ", write_path, " exists. Skipping computation.")
        next # skip this iteration
      } else {
        message("Result ", write_path, " exists. Computing again.")
      }
    }
    data_path = cur_metadata %>% 
      dplyr::summarise(SIMDATA.PATH = glue("{data_prefix}{SIMDATA.PATHONLY}data{k}.RData_opt")) %>%
      as.character()
    
    tictoc::tic(glue("Species trends for {cur_mask}: {k}/{max(cur_assignment)}"))
    
    # read data files for this step
    rgid_path <- paste0(dirname(databins_path_data),"/rgids-", k, ".RData")
    message(paste("Loading", rgid_path))
    load(rgid_path) # loads randomgroupids

    # Subset data to match assignment
    data_filt <- data[data$group.id %in% randomgroupids, ]

    # map timegroups to strings
    data_filt$timegroups <- timegroups_names$timegroups[data_filt$timegroups]

    cols_temp <- if (singleyear == FALSE) {
      c("gridg1", "gridg3", "month", "timegroups")
    } else if (singleyear == TRUE) {
      c("gridg1", "gridg3", "month")
    }

    data <- data_filt %>% 
      mutate(across(.cols = all_of(cols_temp), ~ as.factor(.)))

    rm(cols_temp)
    
    # start parallel
    n.cores = worker_procs # From command line

    if(have_run_stats) {
      run_stats <- species_run_stats
    }
    species_todo <- length(species_to_process)
    if (species_todo==0) {
      species_todo <- length(listofspecies)
    } else {
      if(have_run_stats) {
        run_stats <- run_stats %>%
	                   filter(species_name %in% species_to_process)
      }
    }
    message(paste("Processing", species_todo, "species..."))

    species_threads <- min(n.cores, species_todo) # if very few species then can't engage all cores
    species_threads_active <- 0
    species_done <- 0
    species_failed <- 0
    trends0 <- NULL
    
    # run_stats is ordered in descending order of runtime.
    # longest running species typically consume max memory as well
    # so peak memory consumption should happen in the beginning
    # then this will taper.  Doing this also ensures that the job
    # will fail in the beginning rather than later due to OOM
    # scenarios.
    #
    # the table has species name, time to run, and peakRAM usage
    # for that run.  Having all this as data makes it possible to
    # "schedule" intelligently later

    free_ram <- get_free_ram() - ram_safety_margin*1000*1024

    if(have_run_stats) {
      message("rs = ", have_run_stats)
      # Minimum we can run is 1 species at a time. If we don't have memory
      # for that, better to exit now!
      min_ram_needed <- max(run_stats$peakRAM)*1000*1024

      if (min_ram_needed > free_ram) {
        message("Not enough RAM to fit even single species.")
        message(paste("Min reqd:", min_ram_needed, "Available:", free_ram))
        message("Increase container memory limits and try again.")
        quit()
      }
    }

    message("Free RAM at start ", as.integer(free_ram/1000000), " MB")
    if(!have_run_stats) {
      message("No run stats. Running jobs in FCFS order.")
    } else if(ram_interleave) {
      message("Running jobs with runtime length, combined with RAM interleave scheduling")
      # bucket based on RAM. Mix of jobs with different RAM (3+2+1)=6 = 2x3 cores
      # or 2 GB ram per core
      bucket1 <- subset(run_stats, peakRAM < 1000)
      bucket2 <- subset(run_stats, peakRAM >= 1000 & peakRAM < 2000)
      bucket3 <- subset(run_stats, peakRAM >= 2000)

      # clear
      run_stats <- run_stats[0, ]
      # combine buckets with interleaving
      iters <- max(nrow(bucket1),nrow(bucket2),nrow(bucket3))
      for (i in 1:iters) {
        if(i<=nrow(bucket3)) {
          run_stats[nrow(run_stats)+1,] <- bucket3[i,]
        }
        if(i<=nrow(bucket2)) {
          run_stats[nrow(run_stats)+1,] <- bucket2[i,]
        }
        if(i<=nrow(bucket1)) {
          run_stats[nrow(run_stats)+1,] <- bucket1[i,]
        }
      }
    } else {
      message("Running jobs with runtime length scheduling")
    }

    try_thread_start <- TRUE

    launched <- list()
    if(have_run_stats) {
      species_pending_list <- as.vector(run_stats$species_name)
    } else {
      if(length(species_to_process)==0) {
        species_pending_list <- listofspecies
      } else {
        species_pending_list <- species_to_process
      }
    }

    # Keep going as long as we have species to run, or threads active
    while((length(species_pending_list)>0) || (species_threads_active>0)) {
      # start as many threads as we have (remaining) capacity for
      started <- 0
      if(try_thread_start) {
        while((length(species_pending_list) > 0) && (species_threads_active < species_threads)) {
	  launch_species <- species_pending_list[1]
          species_pending_list <- species_pending_list[-1] # Remove it for now

	  if(have_run_stats) {
	    launch_species_idx <- which(run_stats$species_name == launch_species)
	    min_ram_needed <- run_stats$peakRAM[launch_species_idx]*1000*1024
            if(min_ram_needed > free_ram) {
	       message("Won't start ", species_threads-species_threads_active,
	               " threads due to insufficient RAM (needed for 1 more: ",
	               as.integer(min_ram_needed/1000000), " MB free:",
		       as.integer(free_ram/1000000), " MB)")
	       break
	    }
          } else {
	    min_ram_needed <- 0
	  }
	  species_index <- which(species_names$COMMON.NAME==launch_species)

	  # assume job will consume this much
	  free_ram <- free_ram - min_ram_needed

	  proc <- mcparallel(
		    singlespeciesrun(
		       container = container,
		       reproducible = reproducible_run,
		       stats_dir = trends_stats_dir,
		       species_dir = trends_species_dir,
		       data = data,
		       species_index = species_index,
                       species = launch_species,
                       specieslist = specieslist, 
                       restrictedspecieslist = restrictedspecieslist,
                       singleyear = singleyear)
	          )
	  # keep track of which PID is which species
	  if(have_run_stats) {
	    run_duration <-run_stats$time[launch_species_idx]
	  } else {
	    run_duration <- 0
	  }
	  launched[[as.character(proc$pid)]] <- c(launch_species,
					          proc.time()[3],
						  run_duration)

	  if(have_run_stats) {
	    message("Started: ",
                  launch_species, ", estimated peakRAM: ",
                  as.integer(min_ram_needed/1000000), " MB,",
                  " runtime: ", run_duration, " seconds")
          } else {
	    message("Started: ", launch_species)
	  }

	  species_threads_active <- species_threads_active + 1
          started <- started + 1
        }
      }

#      if(started > 0) {
#        message(paste("Threads started:", started))
#      }

      if(have_run_stats) {
        message("T=",proc.time()[3],
		  " Estmated Peak Free RAM:", as.integer(free_ram/1000000), " MB",
		  " Threads:", species_threads_active,
		  " Done:", species_done,
		  " Pending:", length(species_pending_list),
                  " Failed:", species_failed)
      } else {
        message("T=",proc.time()[3],
		  " Threads:", species_threads_active,
		  " Done:", species_done,
		  " Pending:", length(species_pending_list),
                  " Failed:", species_failed)
      }

      done <- 0
      result_set <- mccollect(timeout = 1000, wait=FALSE)
      if (!is.null(result_set)) {
        finished_pids <- names(result_set)
        # iterate over the results
        for (idx in 1:length(finished_pids)) {
	  result <- result_set[[finished_pids[idx]]]
	  pid_str <- as.character(finished_pids[idx])
	  this_species <- launched[[pid_str]][1]
	  time_taken <- proc.time()[3] - as.numeric(launched[[pid_str]][2])
	  time_expected <- as.numeric(launched[[pid_str]][3])
          launched[[pid_str]] <- NULL # remove
	  if(length(result)==0) {
	      message("Failed: ", this_species, " Time taken:", time_taken, " secs")
	      # Move this failed to pending
	      species_pending_list <- append(species_pending_list, this_species)
	      species_failed <- species_failed + 1
	  } else {
	      # show 100% without run stats
	      if(!have_run_stats) {
	        time_expected <- time_taken
	      }
	      percent <- (time_taken/time_expected)*100.0
	      percent_str <- format(round(percent, 2), nsmall = 2)
	      message("Finished: ", this_species, " Time taken:", time_taken, " secs (", percent_str, " %)")
              trends0 <- cbind(trends0, result)
	      species_done <- species_done + 1
	  }

	  if(have_run_stats) {
	    done_index <- which(run_stats$species_name==this_species)
	    done_mem <- run_stats$peakRAM[done_index]*1000*1024
	    free_ram <- free_ram + done_mem
	  }
	  species_threads_active <- species_threads_active - 1
          done <- done + 1
        }
	# Thread status changed, so may need to launch next
        try_thread_start <- TRUE
      } else {
        try_thread_start <- FALSE
      }

#      if (done > 0) {
#        message(paste("Threads finished:", done))
#      }

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
                            each = length(soib_year_info("timegroup_lab", container))),
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
        mutate(timegroups = soib_year_info("latest_year", container),
               timegroupsf = as.character(soib_year_info("latest_year", container)))

      }} %>% 
      # make sure freq and se are numerical
      mutate(across(c("freq", "se"), ~ as.numeric(.)))

    # reorder species to reflect the list order prior to RAM optimizations
    # this makes it easy to compare trends_N.csv files with older ones side
    # by side
    trends_reorder <- trends[0, ]
    for (sp_name in listofspecies) {
      entries <- subset(trends, COMMON.NAME==sp_name)
      rows <- nrow(entries)
      if(rows>1) {
        for (idx in 1:rows) {
          trends_reorder[nrow(trends_reorder)+1,] <- entries[idx,]
        }
      }
    }
    trends <- trends_reorder

    # if full run, overwrite the CSV
    # else append single year results to all previous year results
    if (singleyear == FALSE) {

      write.csv(trends, file = write_path, row.names = FALSE)

    } else if (singleyear == TRUE) {

      trends_old <- read.csv(write_path, header = TRUE) |> 
        filter(timegroups != soib_year_info("latest_year", container))

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
