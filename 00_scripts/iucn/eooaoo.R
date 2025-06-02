# Configurations for the algorithm
source("config.R")

# Common utility functions
source("utils.R")

# Reads ebd, filters interesting columns, stores the data in ebd.RDS
source("ebd.R")

# Generates all grids of all sizes using the shapefile of the region
source("gridgen.R")

# Calculates and maps eoo using ebd
source("eoo.R")

# Makes a list of species for which there has been an EOO change
source("eoodiff.R")

# Uses EOO maps to filter out only the grids that overlap with EOO 
source("eooGrids.R")

# Use the grids from EOO and calculate AOO
source("aoo.R")

# Maps the AOO Grids (which is the EOO area) with occupancy and also shows the AOO/EOO values
source("maps.R")

EOO <- readRDS("eoo.rds")
EOODiff <- readRDS("eoodiff.rds")
AOO <- readRDS ("aoo.rds")

EOOAOO <- AOO %>% left_join(EOO, by = c("Species")) %>% left_join(EOODiff, c("Species"))
EOOAOO <- EOOAOO %>%
  mutate(
    MinAOO = round(MinAOO),
    MaxAOO = round(MaxAOO),
    LikelyEOO = round(LikelyEOO),
    MaxEOO = 0,
    EOOChange = round(PercentChange),
  ) %>% 
  select (Species, MinAOO, MaxAOO, MinEstimate_2km, EOOStartYear, LikelyEOO, MaxEOO, YearBandChange, EOOChange)
write.csv(EOOAOO, "eooaoo.csv")