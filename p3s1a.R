#
# Script to run Part 3 Step 1a (variant of original)
#
# i.e. computing species trends
# for whole country
# and first random set (my_assignment = 1:1)
#
# Primarily used for 2 purposes:
# 1. Ensure sanity of setup (compare trends1.csv with reference)
# 2. Benchmark execution performance
#

# We're managing threads ourselves. Disable OMP threads
# as that can reduce performance due to contention.
Sys.setenv(OMP_NUM_THREADS = 1)

suppressPackageStartupMessages({
   library(tidyverse)
   library(Matrix)
   library(VGAM)
   library(unmarked)
   library(reshape2)
   library(data.table)
   library(arm)
   library(MASS)
   library(tictoc)
})

hostname <- paste0(Sys.info()["nodename"],"")

show_help <- function() {
  message("Supported arguments:")
  message("-h            : show help")
  message("-version      : show version of scripts")
  message("-dep-versions : show version of dependencies (packaged in container)")
  quit()
}

show_version <- function() {
  fileName <- "VERSION"
  if(file.exists(fileName)) {
    version <- readChar(fileName, file.info(fileName)$size)
  } else {
    version <- "(check source tree)"
  }
  message("Version: ", version)
  quit()
}

show_dep_versions <- function() {
  message("R version: ", version)
  message("Installed packages are:")
  ip <- as.data.frame(installed.packages()[,c(1,3:4)])
  rownames(ip) <- NULL
  ip <- ip[is.na(ip$Priority),1:2,drop=FALSE]
  print(ip, row.names=FALSE)
  if(file.exists('/etc/alpine-release')) {
    message("Alpine linux (container) info:")
    file_content <- readLines("/etc/os-release", warn = FALSE) # Reads lines from the file
    cat(file_content, sep = "\n") # Prints each line to the console, separated by newlines
  }
  quit()
}

config_filename <- 'config.R'
args = commandArgs(trailingOnly=TRUE)
if(length(args)==1) {
  arg1 <- args[1]
  if(arg1 == "-h") {
    show_help()
  } else if(arg1 == "-version") {
    show_version()
  } else if(arg1 == "-dep-versions") {
    show_dep_versions()
  }
  # else !
  config_filename <- arg1
  message("Using config file: ", config_filename)
} else if (length(args)>1) {
  message("Bad number of parameters")
  show_help()
}

# If localhost config file exists, pick that first
config_path = paste0("config/localhost/", config_filename)
if (!file.exists(config_path)) {
  config_path = paste0("config/",hostname,"/", config_filename)
}

# Source config file to get runtime parameters
# Unsupported values are ignored.
source(config_path)

if(!exists('container')) {
  container <- FALSE;
}

# We pass results back to host using the "output" directory.
# The container doesn't have it. It has to be bound by the user
# while starting the container. We check this exists
# Ideally we'd have to check whether it's writable, and doesn't
# have any funky permissions that break our scripts
if (container && (!dir.exists("output"))) {
  message(paste("Output directory does not exist. Please ensure it is mounted"))
  quit()
}

library(parallel)

# threads may be defined in config file
if(!exists('threads')) {
  # we may get odd number of cores! e.g. in Mac
  worker_procs <- as.integer(parallel::detectCores()/2)
  message("Using autodetected threads: ", worker_procs)
} else {
  message("Using configured threads: ", threads)
  worker_procs <- as.integer(threads)
}

if(!exists('ram_safety_margin')) {
  ram_safety_margin <- 0
}

if(!exists('reproducible_run')) {
  reproducible_run <- FALSE
}

if(!exists('ram_interleave')) {
  ram_interleave <- TRUE
}

if(!exists('species_to_process')) {
  message("Processing ALL species")
  species_to_process <- c()
} else {
  message("Processing species from config:")
  for(sp in species_to_process) {
    message("  ", sp)
  }
}

# By default, run trends calculations even if the
# result file exists. Good for devs
if(!exists('force_trends_computation')) {
  force_trends_computation <- TRUE
}

# necessary packages, functions/scripts, data
library(tidyverse)
library(glue)
library(tictoc)

source("00_scripts/00_functions.R")

if(container) {
  load("data/00_data/analyses_metadata.RData")
} else {
  load("00_data/analyses_metadata.RData")
}

# full country runs -------------------------------------------------------

if(!exists('my_assignment')) {
  my_assignment <- 1:1
}

# PART 3 (run) ------------------------------------------------------------------

# STEP 1: Run trends models for all selected species
# Requires:
# - tidyverse, tictoc, lme4, VGAM, parallel, foreach, doParallel
# - data files:
#   - "dataforanalyses.RData-data_opt"
#   - "dataforanalyses.RData-metadata"
#   - "specieslists.RData"
# Outputs:
# - "trends/trendsX.csv" files

message('Running Part 3, Step 1')
if(!exists('cur_mask')) {
  cur_mask <- "none" # analysis for full country (not masks)
}
tic(glue("Species trends for mask {cur_mask} (sims {min(my_assignment)}:{max(my_assignment)})"))
source("00_scripts/run_species_trends_container.R")
toc()
