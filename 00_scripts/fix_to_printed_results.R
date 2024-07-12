library(tidyverse)

old = read.csv("01_analyses_full/results/SOIB_main_print.csv")
new = read.csv("01_analyses_full/results/SOIB_main.csv")

old_ltt = old %>% dplyr::select(eBird.English.Name.2023,longtermlci,longtermmean,longtermrci,SOIBv2.Long.Term.Status)
new_ltt = new %>% dplyr::select(eBird.English.Name.2023,longtermlci,longtermmean,longtermrci,SOIBv2.Long.Term.Status)

ltt = old_ltt %>% left_join(new_ltt, by = c("eBird.English.Name.2023"))
ltt_mis = ltt %>% filter(SOIBv2.Long.Term.Status.x != SOIBv2.Long.Term.Status.y)

old_ctt = old %>% dplyr::select(eBird.English.Name.2023,currentslopelci,currentslopemean,currentsloperci,SOIBv2.Current.Status)
new_ctt = new %>% dplyr::select(eBird.English.Name.2023,currentslopelci,currentslopemean,currentsloperci,SOIBv2.Current.Status)

ctt = old_ctt %>% left_join(new_ctt, by = c("eBird.English.Name.2023"))
ctt_mis = ctt %>% filter(SOIBv2.Current.Status.x != SOIBv2.Current.Status.y)

ltt_rep = ltt_mis %>% dplyr::select(eBird.English.Name.2023,SOIBv2.Long.Term.Status.x) %>%
  rename(SOIBv2.Long.Term.Status = SOIBv2.Long.Term.Status.x)
write.csv(ltt_mis,"01_analyses_full/results/ltt_changes.csv",row.names = F)

ctt_rep = ctt_mis %>% dplyr::select(eBird.English.Name.2023,SOIBv2.Current.Status.x) %>%
  rename(SOIBv2.Current.Status = SOIBv2.Current.Status.x)
write.csv(ctt_mis,"01_analyses_full/results/cat_changes.csv",row.names = F)



print_fix = old %>% dplyr::select(eBird.English.Name.2023,longtermlci,longtermmean,longtermrci,currentslopelci,
                                currentslopemean,currentsloperci,rangelci,rangemean,rangerci,SOIBv2.Long.Term.Status,
                                SOIBv2.Current.Status,SOIBv2.Range.Status,SOIBv2.Priority.Status)
write.csv(print_fix,"print_fix.csv",row.names = F)

setdiff(new$eBird.English.Name.2023,old$eBird.English.Name.2023)
