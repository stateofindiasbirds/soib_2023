source("00_scripts/00_functions.R")

library(conflicted) #Issue with MASS package from 00_fuctions.R
conflict_prefer("filter", "dplyr")
conflict_prefer("select", "dplyr")
conflict_prefer("first", "dplyr")

source("00_scripts/iucn/criteriaAEngine.R")
source("00_scripts/iucn/criteriaBEngine.R")
source("00_scripts/iucn/criteriaCEngine.R")
source("00_scripts/iucn/criteriaDEngine.R")

source("00_scripts/iucn/nrl_compile_csv_gen.R")
source("00_scripts/iucn/regional_redist_htmlgen.R")