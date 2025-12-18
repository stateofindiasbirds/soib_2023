library(tidyverse)
library(glue)
library(tictoc)

source("00_scripts/00_functions.R")

interannual_update = TRUE

tic("Adding map and grid variables to dataset")
addmapvars()
toc()
