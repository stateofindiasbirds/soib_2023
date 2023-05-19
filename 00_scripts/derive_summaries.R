# Derive relevant results and summaries

library(tidyverse)

main = read.csv("trends_results/full_results/SoIB_main.csv")
temp = main %>% filter(!IUCN.Category %in% c("Least Concern","Not Recognised") & 
                         SOIBv2.Priority.Status %in% c("Low"))


main = read.csv("trends_results/full_results/SoIB_main.csv")
temp = main %>% filter(Long.Term.Analysis == "X" & 
                         !SOIBv2.Long.Term.Status %in% c("eBird Data Deficient","eBird Data Indecisive") &
                         Order == "Piciformes") %>%
  select(eBird.English.Name.2022,SOIBv2.Long.Term.Status,SOIBv2.Current.Status,SOIBv2.Priority.Status)
