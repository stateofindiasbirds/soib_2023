library(stringr)
library(dplyr)

scriptpath <- "00_scripts/iucn/"
resultspath <- "01_analyses_full/results"

# Configurations for the algorithm
source(file.path(scriptpath, "config.R"))

# Common utility functions
source(file.path(scriptpath, "utils.R"))

# Reads ebd, filters interesting columns, stores the data in ebd.RDS
source(file.path(scriptpath, "ebd.R"))

# Generates all grids of all sizes using the shapefile of the region. Run this only once.
source(file.path(scriptpath, "gridgen.R"))

# Calculates and maps eoo using ebd
source(file.path(scriptpath, "eoo.R"))

# Makes a list of species for which there has been an EOO change
source(file.path(scriptpath, "eoodiff.R"))

# Uses EOO maps to filter out only the grids that overlap with EOO 
source(file.path(scriptpath, "eooGrids.R"))

# Use the grids from EOO and calculate AOO
source(file.path(scriptpath, "aooprefilter.R"))

# Use the grids from EOO and calculate AOO
source(file.path(scriptpath, "aoo.R"))

# Maps the AOO Grids (which is the EOO area) with occupancy and also shows the AOO/EOO values
source(file.path(scriptpath, "maps.R"))

EOO <- readRDS(file.path(resultspath, "eoo.RDS")) %>% filter (EOOEndYear == 2025)
EOODiff <- readRDS(file.path(resultspath, "eoodiff.RDS")) %>% filter(str_detect(EOOYearBandChange, "to .*2025$"))
AOO <- readRDS (file.path(resultspath, "aoo.RDS"))

EOOAOO <- AOO %>% left_join(EOO, by = c("Species")) %>% left_join(EOODiff, c("Species"))
EOOAOO <- EOOAOO %>%
  mutate(
    MinAOO = round(MinAOO),
    MaxAOO = round(MaxAOO),
    LikelyEOO = round(LikelyEOO),
    MaxEOO = NA,
    EOOChange = round(EOOChange),
  ) %>% 
  select (Species, MinAOO, MaxAOO, MinEstimate_2km, EOOStartYear, LikelyEOO, MaxEOO, EOOYearBandChange, EOOChange, EOOChangePercent)
write.csv(EOOAOO, file.path(resultspath, "eooaoo.csv"))