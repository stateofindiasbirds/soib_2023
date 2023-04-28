## assign categories to trends and occupancy

source('SoIB_v2 functions.R')
library(tidyverse)

main = read.csv("SoIB_main.csv")


main = main %>%
  mutate(SOIBv2.Long.Term.Status = 
           case_when(is.na(longtermmean) ~ "eBird Data Deficient",
                     longtermrci <= 50 ~ "Rapid Decline",
                     longtermrci <= 75 ~ "Declining",
                     longtermlci >= 150 ~ "Rapid Increase",
                     longtermlci >= 125 ~ "Increasing",
                     ((longtermrci-longtermlci) > 25 & longtermlci > 100 & longtermrci > 100) ~ "eBird Data Indecisive",
                     ((longtermrci-longtermlci) > 25 & longtermlci < 100 & longtermrci < 100) ~ "eBird Data Indecisive",
                     (longtermrci-longtermlci) > 50 ~ "eBird Data Indecisive",
                     TRUE ~ "Stable")
  ) %>%
  mutate(SOIBv2.Current.Status = 
           case_when(is.na(currentslopemean) ~ "eBird Data Deficient",
                     currentsloperci <= -2.7 ~ "Rapid Decline",
                     currentsloperci <= -1.1 ~ "Declining",
                     currentslopelci >= 1.6 ~ "Rapid Increase",
                     currentslopelci >= 0.9 ~ "Increasing",
                     ((currentsloperci-currentslopelci) > 2 & currentslopelci > 0 & currentsloperci > 0) ~ "eBird Data Indecisive",
                     ((currentsloperci-currentslopelci) > 2 & currentslopelci < 0 & currentsloperci < 0) ~ "eBird Data Indecisive",
                     (currentsloperci-currentslopelci) > 6 ~ "eBird Data Indecisive",
                     TRUE ~ "Stable")
  )

main$SOIBv2.Long.Term.Status[main$Selected.SOIB != "X"] = NA
main$SOIBv2.Current.Status[main$Selected.SOIB != "X"] = NA
main$SOIB.Range.Status[main$Selected.SOIB != "X"] = NA



trendcats = c("Rapid Decline","Declining","eBird Data Deficient","eBird Data Indecisive",
              "Stable","Increasing","Rapid Increase")
rangecats = c("eBird Data Deficient","Very Restricted","Restricted","Moderate",
              "Large","Very Large")


priorityrules = read.csv("priorityclassificationrules.csv")
main = left_join(main,priorityrules)


unce = c("eBird Data Deficient","eBird Data Indecisive")
rest = c("Very Restricted","Restricted")
decl = c("Declining","Rapid Decline")

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

## small investigations

summary_status = data.frame(Category = trendcats)
a = data.frame(table(main$SOIBv2.Long.Term.Status))
names(a) = c("Category","Long.Term")
b = data.frame(table(main$SOIBv2.Current.Status))
names(b) = c("Category","Current")
summary_status = left_join(summary_status,a)
summary_status = left_join(summary_status,b)

priority_summary = data.frame(table(main$SOIBv2.Priority.Status))
names(priority_summary) = c("Category","N.species")

species_summary = data.frame(Category = c("Selected for SoIB","Long-term Analysis","Current Analysis"),
                             N.species = c(as.vector(table(main$Selected.SOIB))[2],
                                           as.vector(table(main$Long.Term.Analysis))[2],
                                           as.vector(table(main$Current.Analysis))[2]))

write.csv(summary_status,"summary_status.csv",row.names=F)
write.csv(priority_summary,"priority_status.csv",row.names=F)
write.csv(species_summary,"species_status.csv",row.names=F)


