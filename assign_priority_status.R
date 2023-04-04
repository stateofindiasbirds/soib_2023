## assign categories to trends and occupancy

source('SoIB_v2 functions.R')
library(tidyverse)
load("specieslists.RData")


main = read.csv("SoIB_main.csv")
#soiblist = read.csv("fullspecieslist.csv")
#main = left_join(main,soiblist,by=c("eBird.English.Name.2022"="COMMON.NAME"))

# change this method later
speclist = specieslist$COMMON.NAME

main = main %>%
  mutate(SOIBv2.Long.Term.Status = 
           case_when(is.na(longtermmean) ~ "Data Deficient",
                     longtermrci <= 50 ~ "Strong Decline",
                     longtermrci <= 75 ~ "Moderate Decline",
                     longtermlci >= 150 ~ "Strong Increase",
                     longtermlci >= 125 ~ "Moderate Increase",
                     (longtermrci-longtermlci) > 25 ~ "Uncertain",
                     TRUE ~ "Stable")
  ) %>%
  mutate(SOIBv2.Current.Status = 
           case_when(is.na(currentslopemean) ~ "Data Deficient",
                     currentsloperci <= -2.7 ~ "Strong Decline",
                     currentsloperci <= -1.1 ~ "Moderate Decline",
                     currentslopelci >= 1.6 ~ "Strong Increase",
                     currentslopelci >= 0.9 ~ "Moderate Increase",
                     (currentsloperci-currentslopelci) > 2 ~ "Uncertain",
                     TRUE ~ "Stable")
  )

main$SOIBv2.Long.Term.Status[!main$eBird.English.Name.2022 %in% speclist] = NA
main$SOIBv2.Current.Status[!main$eBird.English.Name.2022 %in% speclist] = NA
main$SOIB.Range.Status[!main$eBird.English.Name.2022 %in% speclist] = NA



trendcats = c("Strong Decline","Moderate Decline","Data Deficient","Uncertain",
              "Stable","Moderate Increase","Strong Increase")
rangecats = c("Data Deficient","Very Restricted","Restricted","Moderate",
              "Large","Very Large")


priorityrules = read.csv("priorityclassificationrules.csv")
main = left_join(main,priorityrules)


unce = c("Data Deficient","Uncertain")
rest = c("Very Restricted","Restricted")
decl = c("Moderate Decline","Strong Decline")

main = main %>%
  mutate(SOIBv2.Priority.Status = as.character(SOIBv2.Priority.Status)) %>%
  mutate(SOIBv2.Priority.Status = 
           case_when(SOIBv2.Long.Term.Status %in% unce & SOIBv2.Current.Status %in% unce &
                       IUCN.Category %in% c("Endangered","Critically Endangered") ~ "High",
                     SOIBv2.Long.Term.Status %in% decl & SOIBv2.Current.Status %in% decl &
                       IUCN.Category %in% c("Endangered","Critically Endangered") ~ "High",
                     SOIBv2.Long.Term.Status %in% unce & SOIBv2.Current.Status %in% unce &
                       SOIB.Range.Status %in% rest &
                       IUCN.Category %in% c("Vulnerable") ~ "High",
                     SOIBv2.Long.Term.Status %in% unce & SOIBv2.Current.Status %in% unce &
                       IUCN.Category %in% c("Near Threatened","Vulnerable") & SOIBv2.Priority.Status == "Low" ~ "Moderate",
                     TRUE ~ SOIBv2.Priority.Status))

write.csv(main,"SoIB_main.csv",row.names=F)

