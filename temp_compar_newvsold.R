# trends ###

trends_old <- read.csv("01_analyses_full/full_results/trends.csv")


additions <- anti_join(trends %>% rownames_to_column("ID"), 
                       trends_old %>% rownames_to_column("ID"),
                       by = "ID")
write.csv(additions, file = "01_analyses_full/trends_newthistime.csv", row.names = FALSE)


difference <- trends %>% 
  anti_join(additions) %>% 
  mutate(across(c(everything(), -timegroups, -COMMON.NAME, -timegroupsf),
                ~  (. - trends_old[[cur_column()]]) %>% format(scientific = FALSE), 
                .names = "{.col}"))
write.csv(difference, file = "01_analyses_full/trends_newminusold.csv", row.names = FALSE)


# sens ###

sens_old <- read.csv("01_analyses_full/full_results/current_sensitivity.csv")

difference <- sens %>% 
  mutate(across(c(everything(), -eBird.English.Name.2022),
                ~  (. - sens_old[[cur_column()]]) %>% format(scientific = FALSE), 
                .names = "{.col}"))

write.csv(difference, file = "01_analyses_full/current_sensitivity_newminusold.csv", row.names = FALSE)


# main wo cats ###

wocats_names_old <- names(read.csv("01_analyses_full/full_results/SoIB_main_wocats.csv"))
wocats_names <- names(read.csv("01_analyses_full/SoIB_main_wocats.csv"))
wocats_names == wocats_names_old

names(read.csv("01_analyses_full/full_results/SoIB_main.csv")) == temp


temp <- c("eBird.English.Name.2022", "eBird.Scientific.Name.2022", "eBird.Code", "Order", "Family", 
          "SOIB.Concern.Status", "SOIB.Long.Term.Status", "SOIB.Current.Status", "SOIB.Range.Status", 
          "Breeding.Activity.Period", "Non.Breeding.Activity.Period",
          "Diet.Guild", "India.Endemic", "Subcontinent.Endemic", "Himalayas.Endemic", "Endemic.Region", 
          "Habitat.Specialization", "Migratory.Status.Within.India", "Essential", "Discard",
          "India.Checklist.Common.Name", "India.Checklist.Scientific.Name", 
          "BLI.Common.Name", "BLI.Scientific.Name", "IUCN.Category", "WPA.Schedule",
          "CITES.Appendix", "CMS.Appendix", "Onepercent.Estimates", 
          "Long.Term.Analysis", "Current.Analysis", "Selected.SOIB", 
          "totalrange25km", "proprange25km2000", "proprange25km.current", "proprange25km2022", "mean5km", "ci5km", 
          "longtermlci", "longtermmean", "longtermrci", "currentslopelci", "currentslopemean", "currentsloperci", 
          "proj2023.lci", "proj2023.mean", "proj2023.rci", "proj2024.lci", "proj2024.mean", "proj2024.rci", 
          "proj2025.lci", "proj2025.mean", "proj2025.rci", "proj2026.lci", "proj2026.mean", "proj2026.rci", 
          "proj2027.lci", "proj2027.mean", "proj2027.rci", "proj2028.lci", "proj2028.mean", "proj2028.rci", 
          "proj2029.lci", "proj2029.mean", "proj2029.rci", 
          "SOIBv2.Long.Term.Status", "SOIBv2.Current.Status", "SOIBv2.Priority.Status")

