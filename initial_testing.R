## initial tests

#library(tidyverse)
load("dataforanalyses.RData")

source('SoIB_v2 functions.R')
species = "Forest Wagtail"
start = Sys.time()
tre = freqtrends(data,species,specieslist,error=T,nsim=1000)
end = Sys.time()
print(end-start)

