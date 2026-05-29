source("00_scripts/00_functions.R")

library(conflicted) #Issue with MASS package from 00_fuctions.R
conflict_prefer("filter", "dplyr")
conflict_prefer("select", "dplyr")

source("00_scripts/iucn/CriteriaAEngine.R")
source("00_scripts/iucn/CriteriaBEngine.R")
source("00_scripts/iucn/CriteriaCEngine.R")
source("00_scripts/iucn/CriteriaDEngine.R")

source("00_scripts/iucn/nrl_compile_csv_gen.R")
source("00_scripts/iucn/regional_redist_htmlgen.R")