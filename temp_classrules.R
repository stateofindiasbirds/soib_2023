# old format

class_old = read.csv("01_analyses_full/SoIB_main_wocats.csv") %>%
  mutate(
    SOIBv2.Long.Term.Status = case_when(
      is.na(longtermmean) ~ "eBird Data Deficient",
      # for declines
      (longtermrci-longtermmean)/longtermmean > 0.5 ~ "eBird Data Inconclusive",
      longtermrci <= 50 ~ "Rapid Decline",
      longtermrci <= 75 ~ "Decline",
      # for increases
      longtermlci >= 150 ~ "Rapid Increase",
      longtermlci >= 125 ~ "Increase",
      longtermrci < 100 ~ "eBird Data Inconclusive",
      longtermlci <= 50 ~ "eBird Data Inconclusive",
      longtermlci > 100 ~ "eBird Data Inconclusive",
      longtermrci >= 150 ~ "eBird Data Inconclusive",
      TRUE ~ "Stable")
    )

class_new = read.csv("01_analyses_full/SoIB_main_wocats.csv") %>%
  mutate(
    SOIBv2.Long.Term.Status = case_when(
      is.na(longtermmean) ~ "eBird Data Deficient",
      # for declines
      (longtermrci-longtermmean)/longtermmean > 0.5 ~ "eBird Data Inconclusive",
      # else
      longtermrci <= 50 ~ "Rapid Decline",
      longtermrci %in% 51:75 ~ "Decline",
      # for increases
      longtermlci >= 150 ~ "Rapid Increase",
      longtermlci %in% 125:149 ~ "Increase",
      longterm < 100 ~ "eBird Data Inconclusive"
      # longtermrci %in% 100:150 & longtermlci %in% 50:100 ~ "Stable",
      # TRUE ~ "eBird Data Inconclusive",
      )
  )

class_old$SOIBv2.Long.Term.Status == class_new$SOIBv2.Long.Term.Status
