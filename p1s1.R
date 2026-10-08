library(tidyverse)
library(glue)
library(tictoc)

source("00_scripts/00_functions.R")

interannual_update = TRUE

source("00_scripts/01_create_metadata.R")

tic("Reading and cleaning raw data")
readcleanrawdata(rawpath = "00_data/ebd_IN_unv_smp_relJun-2026.txt",
                 sensitivepath = "00_data/ebd_sensitive_relJun-2026.txt",
                 centroidspath = "00_data/centroids_sanitized_relJun-2026.rds")
toc()