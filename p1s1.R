library(tidyverse)
library(glue)
library(tictoc)

source("00_scripts/00_functions.R")

interannual_update = TRUE

source("00_scripts/01_create_metadata.R")

tic("Reading and cleaning raw data")
readcleanrawdata(rawpath = "00_data/ebd_IN_unv_smp_relAug-2025.txt",
                 sensitivepath = "00_data/ebd_sensitive_relAug-2025_IN.txt")
toc()