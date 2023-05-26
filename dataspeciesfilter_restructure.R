
# <annotation_pending_AV> elaborate below
# select species for State of India's Birds, and species for historical and recent trends
# includes all diurnal endemics (endemicity) and essential species (SelectSpecies)

dataspeciesfilter = function(
    
  # thresholds for species to be considered in each analysis
  locationlimit = 15, # individual locations
  gridlimit = 4, # grid cells
  listlimit = 50, # checklists
  cur_mask = "none"
  
) {
  
  # ensuring only valid cur_mask names are provided
  if (!(cur_mask %in% unique(dataspeciesfilt_metadata$MASK))) {
    return('Invalid mask! Please provide valid mask name, one of: c("none","woodland","cropland","ONEland","PA").')
  }
  
  
  # preparing data for mask ###
  
  cur_metadata <- dataspeciesfilt_metadata %>% filter(MASK == cur_mask)
  
  if (cur_mask == "none"){
    data0 = data_base
  } else if (cur_mask == "woodland"){
    data0 = data_base %>% filter(maskWdl == 1)
  } else if (cur_mask == "cropland"){
    data0 = data_base %>% filter(maskCrp == 1)
  } else if (cur_mask == "ONEland"){
    data0 = data_base %>% filter(maskOne == 1)
  } else if (cur_mask == "PA"){
    data0 = data_base %>% filter(!is.na(pa.name))
  } 
  

  # processing dataspeciesfilter --------------------------------------------

  data = data0 %>% 
    dplyr::select(-CATEGORY,-REVIEWED,-APPROVED,-ST_NM,-DISTRICT,
                  -LOCALITY.TYPE,-LOCALITY.ID,-pa.name,-maskWdl,-maskCrp,-maskOne,
                  -LATITUDE,-LONGITUDE,-PROTOCOL.TYPE,-EXOTIC.CODE,-day,-cyear,
                  -DURATION.MINUTES,-TIME.OBSERVATIONS.STARTED,-EFFORT.DISTANCE.KM)
  
  stats7 = paste(nrow(data[data$ALL.SPECIES.REPORTED == 1,]),
                 "filter 1 usable observations")
  stats8 = paste(length(unique(data[data$ALL.SPECIES.REPORTED == 1,]$group.id)),
                 "filter 2 unique complete checklists")
  stats9 = paste(length(unique(data[data$timegroups == "before 2000" &
                                      data$ALL.SPECIES.REPORTED == 1,]$group.id)),
                 "pre-2000 checklists")
  
  # summary for each timegroup
  databins = data %>%
    filter(ALL.SPECIES.REPORTED == 1) %>%
    group_by(timegroups) %>% 
    reframe(lists = n_distinct(group.id), 
            year = round(median(year))) %>%
    arrange(year)
  
  
  # historical data (data from before 2000 onwards), used for long-term trends
  # gives list of species for which we have enough data and this analysis can be done
  datah = data0 %>%
    filter(ALL.SPECIES.REPORTED == 1, 
           CATEGORY %in% c("species", "issf")) %>%
    group_by(COMMON.NAME, timegroups) %>%
    reframe(locs = n_distinct(LOCALITY.ID), 
            cells = n_distinct(gridg4)) %>%
    group_by(COMMON.NAME, timegroups) %>%
    filter(locs > locationlimit, cells > gridlimit) %>%
    group_by(COMMON.NAME) %>% 
    reframe(years = n()) %>%
    group_by(COMMON.NAME) %>%
    filter(years == 14) %>%
    mutate(ht = 1) %>% 
    dplyr::select(COMMON.NAME, ht)
  
  # recent data (data from 2015 onwards), used for recent trends
  # gives list of species for which we have enough data and this analysis can be done
  datar = data0 %>%
    filter(ALL.SPECIES.REPORTED == 1, 
           CATEGORY %in% c("species", "issf"), 
           year > 2014) %>%
    group_by(COMMON.NAME, year) %>%
    reframe(locs = n_distinct(LOCALITY.ID), 
            cells = n_distinct(gridg4)) %>%
    group_by(COMMON.NAME, year) %>%
    filter(locs > locationlimit, cells > gridlimit) %>%
    group_by(COMMON.NAME) %>% 
    reframe(years = n()) %>%
    group_by(COMMON.NAME) %>%
    filter(years == 8) %>%
    mutate(rt = 1) %>% 
    dplyr::select(COMMON.NAME, rt)
  
  
  # for other species that don't qualify simple rules above (restricted range)
  dataresth1 = data0 %>%
    filter(ALL.SPECIES.REPORTED == 1, 
           CATEGORY %in% c("species", "issf")) %>%
    group_by(COMMON.NAME, timegroups) %>%
    reframe(cells = n_distinct(gridg4)) %>%
    group_by(COMMON.NAME, timegroups) %>%
    filter(cells <= gridlimit) %>%
    group_by(COMMON.NAME) %>% 
    reframe(years = n()) %>%
    group_by(COMMON.NAME) %>%
    filter(years == 14) %>%
    dplyr::select(COMMON.NAME)
  
  speciesresth = data.frame(species = intersect(unique(dataresth1$COMMON.NAME),
                                                spec_resident),
                            validh = NA_real_)
  
  # if the grids in which species has been reported a few times have sufficient lists
  # from enough years, still consider for analysis
  for (i in 1:length(speciesresth$species))
  {
    tempresth1 = data0 %>%
      filter(COMMON.NAME == speciesresth$species[i]) %>%
      distinct(gridg1) %>%
      left_join(data0) %>%
      group_by(timegroups) %>% 
      reframe(n = n_distinct(group.id)) %>%
      group_by(timegroups) %>%
      filter(n > listlimit)
    
    if (length(tempresth1$timegroups) == 14)
      speciesresth$validh[speciesresth$species == speciesresth$species[i]] = 1
    
  }
  
  datarestr1 = data0 %>%
    filter(ALL.SPECIES.REPORTED == 1, 
           CATEGORY %in% c("species", "issf"), 
           year > 2014) %>%
    group_by(COMMON.NAME, timegroups) %>%
    reframe(cells = n_distinct(gridg4)) %>%
    group_by(COMMON.NAME, timegroups) %>%
    filter(cells <= gridlimit) %>%
    group_by(COMMON.NAME) %>% 
    reframe(years = n()) %>%
    group_by(COMMON.NAME) %>%
    filter(years == 8) %>%
    dplyr::select(COMMON.NAME)
  
  speciesrestr = data.frame(species = intersect(unique(datarestr1$COMMON.NAME),
                                                spec_resident),
                            validr = NA_real_)
  
  for (i in 1:length(unique(datarestr1$COMMON.NAME)))
  {
    temprestr1 = data0 %>%
      filter(COMMON.NAME == speciesrestr$species[i]) %>%
      distinct(gridg1) %>%
      left_join(data0) %>%
      filter(year > 2014) %>%
      group_by(timegroups) %>% 
      reframe(n = n_distinct(group.id)) %>%
      group_by(timegroups) %>%
      filter(n > listlimit)
    
    if (length(temprestr1$timegroups) == 8)
      speciesrestr$validr[speciesrestr$species == speciesrestr$species[i]] = 1
    
  }
  
  
  # full species list (historical + recent) ###
  
  dataf = fullmap
  names(dataf)[1:2] = c("COMMON.NAME","SCIENTIFIC.NAME")
  
  # joining info for normal species (non-range-restricted)
  dataf = dataf %>% 
    left_join(datah, by = c("COMMON.NAME")) %>% 
    left_join(datar, by = c("COMMON.NAME"))
  
  specieslist = dataf %>%
    # <annotation_pending_AV> what does each variable mean? (e.g., Essential)
    # what are we finally filtering for?
    filter((Essential == 1 | Endemic.Region != "None" | ht == 1 | rt == 1) & 
             (Breeding.Activity.Period != "Nocturnal" | 
                Non.Breeding.Activity.Period != "Nocturnal" | 
                COMMON.NAME == "Jerdon's Courser") & 
             (is.na(Discard))) %>%
    dplyr::select(COMMON.NAME, ht, rt)
  
  # <annotation_pending_AV> why filtering dataf also? (instead of specieslist above)
  dataf <- dataf %>% 
    mutate(ht = case_when(Breeding.Activity.Period == "Nocturnal" &
                            Non.Breeding.Activity.Period == "Nocturnal" ~ NA_real_,
                          TRUE ~ ht),
           rt = case_when(Breeding.Activity.Period == "Nocturnal" &
                            Non.Breeding.Activity.Period == "Nocturnal" ~ NA_real_,
                          TRUE ~ rt))
  
  # ignoring species that are frequently misIDd
  specieslist <- specieslist %>% 
    mutate(ht = case_when(COMMON.NAME %in% spec_misid ~ NA_real_,
                          TRUE ~ ht),
           rt = case_when(COMMON.NAME %in% spec_misid ~ NA_real_,
                          TRUE ~ rt))
  
  
  # <annotation_pending_AV> 
  # why left_joining (then removing misIDd specs) separately again? 
  restrictedspecieslist = data.frame(species = specieslist$COMMON.NAME) %>% 
    left_join(speciesresth) %>% 
    left_join(speciesrestr) %>%
    # valid for at least 1 of 2 analyses
    filter(!is.na(validh) | !is.na(validr)) %>% 
    magrittr::set_colnames(c("COMMON.NAME", "ht", "rt")) %>% 
    mutate(ht = case_when(COMMON.NAME %in% spec_misid ~ NA_real_,
                          TRUE ~ ht),
           rt = case_when(COMMON.NAME %in% spec_misid ~ NA_real_,
                          TRUE ~ rt))  
  
  
  # filtering for only species in certain masks ###
  if (cur_mask == "woodland") {
    
    specieslist <- specieslist %>% 
      mutate(ht = case_when(!(COMMON.NAME %in% spec_woodland) ~ NA_real_
                            TRUE ~ ht),
             rt = case_when(!(COMMON.NAME %in% spec_woodland) ~ NA_real_,
                            TRUE ~ rt))
    
    restrictedspecieslist = restrictedspecieslist %>% 
      mutate(ht = case_when(!(COMMON.NAME %in% spec_woodland) ~ NA_real_
                            TRUE ~ ht),
             rt = case_when(!(COMMON.NAME %in% spec_woodland) ~ NA_real_,
                            TRUE ~ rt))
    
  } else if (cur_mask %in% c("cropland", "ONEland")) {
    
    specieslist <- specieslist %>% 
      mutate(ht = case_when(!(COMMON.NAME %in% spec_openland) ~ NA_real_
                            TRUE ~ ht),
             rt = case_when(!(COMMON.NAME %in% spec_openland) ~ NA_real_,
                            TRUE ~ rt))
    
    restrictedspecieslist = restrictedspecieslist %>% 
      mutate(ht = case_when(!(COMMON.NAME %in% spec_openland) ~ NA_real_
                            TRUE ~ ht),
             rt = case_when(!(COMMON.NAME %in% spec_openland) ~ NA_real_,
                            TRUE ~ rt))
    
  }

  
  # <annotation_pending_AV> what exactly are we checking?
  check1 = restrictedspecieslist %>% 
    filter(!is.na(ht)) %>% 
    dplyr::select(COMMON.NAME) %>% as.vector() %>% list_c()
  check2 = restrictedspecieslist %>% 
    filter(!is.na(rt)) %>% 
    dplyr::select(COMMON.NAME) %>% as.vector() %>% list_c()
  
  
  # <annotation_pending_AV> 
  randomcheck_a = data0 %>% 
    filter(ALL.SPECIES.REPORTED == 1, 
           COMMON.NAME %in% restrictedspecieslist$COMMON.NAME) %>%
    group_by(COMMON.NAME) %>% 
    reframe(n = n_distinct(gridg1)) %>%
    group_by(COMMON.NAME) %>% 
    filter(n > 7)
  
  randomcheck_b = data0 %>% 
    filter(ALL.SPECIES.REPORTED == 1, 
           COMMON.NAME %in% restrictedspecieslist$COMMON.NAME) %>%
    group_by(COMMON.NAME) %>% 
    reframe(n = n_distinct(gridg1)) %>%
    group_by(COMMON.NAME) %>% 
    filter(n <= 7)
  
  
  # <annotation_pending_AV> 
  restrictedspecieslist_a = restrictedspecieslist %>% 
    filter(COMMON.NAME %in% randomcheck_a$COMMON.NAME) %>% 
    mutate(mixed = 1)
  restrictedspecieslist_b = restrictedspecieslist %>% 
    filter(COMMON.NAME %in% randomcheck_b$COMMON.NAME) %>% 
    mutate(mixed = 0)
  
  restrictedspecieslist = rbind(restrictedspecieslist_a,restrictedspecieslist_b)
  
  
  # <annotation_pending_AV> 
  t1 = dataf %>%
    filter((ht == 1 | rt == 1) &
             (Breeding.Activity.Period != "Nocturnal" |
                Non.Breeding.Activity.Period != "Nocturnal"))
  t2 = dataf %>%
    filter((Endemic.Region != "None" | ht == 1 | rt == 1) & 
             (Breeding.Activity.Period != "Nocturnal" |
                Non.Breeding.Activity.Period != "Nocturnal"))
  t3 = dataf %>%
    filter((Essential == 1 | Endemic.Region != "None" | ht == 1 | rt == 1) &
             (Breeding.Activity.Period != "Nocturnal" |
                Non.Breeding.Activity.Period != "Nocturnal"))
  
  stats10 = paste(length(t1$COMMON.NAME),"filter 1 number of species")
  stats11 = paste(length(t2$COMMON.NAME),"filter 2 number of species")
  stats12 = paste(length(t3$COMMON.NAME),"filter 3 number of species")
  
  
  # <annotation_pending_AV> 
  specieslist1 = specieslist %>% 
    mutate(selected = 1) %>% 
    dplyr::select(COMMON.NAME, selected)
  
  dataf = dataf %>%
    dplyr::select(COMMON.NAME, SCIENTIFIC.NAME, ht, rt) %>% 
    left_join(specieslist1) %>% 
    magrittr::set_colnames(c("COMMON.NAME","SCIENTIFIC.NAME",
                             "Long.Term.Analysis","Current.Analysis",
                             "Selected.SOIB")) %>%  
    # converting to report table-style with blanks for NAs and Xs for 1s
    mutate(across(everything(), ~ as.character(.))) %>% 
    mutate(across(everything(), ~ replace_na(., replace = ""))) %>% 
    mutate(across(everything(), ~ str_replace(., pattern = "1", replacement = "X"))) %>% 
    # also including species in checks
    mutate(Long.Term.Analysis = if_else(COMMON.NAME %in% check1, "X", Long.Term.Analysis),
           Current.Analysis = if_else(COMMON.NAME %in% check2, "X", Current.Analysis)) %>%
    dplyr::select("COMMON.NAME","SCIENTIFIC.NAME","Long.Term.Analysis","Current.Analysis",
                  "Selected.SOIB")
  
  
  # filtering for only species in certain masks ###
  if (cur_mask == "woodland") {
    
    dataf <- dataf %>% 
      mutate(Long.Term.Analysis = if_else(!(COMMON.NAME %in% spec_woodland), "", Long.Term.Analysis),
             Current.Analysis = if_else(!(COMMON.NAME %in% spec_woodland), "", Current.Analysis),
             Selected.SOIB = if_else(!(COMMON.NAME %in% spec_woodland), "", Selected.SOIB))
    
  } else if (cur_mask %in% c("cropland", "ONEland")) {
    
    dataf <- dataf %>% 
      mutate(Long.Term.Analysis = if_else(!(COMMON.NAME %in% spec_openland), "", Long.Term.Analysis),
             Current.Analysis = if_else(!(COMMON.NAME %in% spec_openland), "", Current.Analysis),
             Selected.SOIB = if_else(!(COMMON.NAME %in% spec_openland), "", Selected.SOIB))
    
  }

  
  # number of sampled grid cell at each resolution
  sampledcells = c(length(unique(data0$gridg0)),
                   length(unique(data0$gridg1)),
                   length(unique(data0$gridg2)),
                   length(unique(data0$gridg3)),
                   length(unique(data0$gridg4)))
  
  stats = c(stats1, stats2, stats3, stats4, stats5, stats6,
            stats7, stats8, stats9, stats10, stats11, stats12)
  
  
  
  # additional filtering safeguards - proportion of range sampled during every timegroup
  
  temp = data %>%
    filter(COMMON.NAME %in% dataf$COMMON.NAME, 
           ALL.SPECIES.REPORTED == 1)
  
  totalrange = temp %>%
    group_by(COMMON.NAME) %>% 
    reframe(totalrange25km = n_distinct(gridg1))
  
  proprange2000 = temp %>%
    filter(timegroups == "before 2000") %>%
    group_by(COMMON.NAME) %>% 
    reframe(proprange25km2000 = n_distinct(gridg1))
  
  proprange2022 = temp %>%
    filter(timegroups == "2022") %>%
    group_by(COMMON.NAME) %>% 
    reframe(proprange25km2022 = n_distinct(gridg1))
  
  proprange.current = temp %>%
    filter(timegroups %in% as.character(2015:2022)) %>%
    group_by(COMMON.NAME, timegroups) %>% 
    reframe(proprange25km.current = n_distinct(gridg1)) %>%
    group_by(COMMON.NAME) %>% 
    reframe(proprange25km.current = mean(proprange25km.current))
  
  range25km = totalrange %>% 
    left_join(proprange2000) %>% 
    left_join(proprange.current) %>% 
    left_join(proprange2022) %>%
    mutate(proprange25km2000 = proprange25km2000/totalrange25km,
           proprange25km.current = proprange25km.current/totalrange25km,
           proprange25km2022 = proprange25km2022/totalrange25km)
  
  
  # additional filtering safeguards - proportional sampling within each 25km grid cell
  
  samp5km = data %>%
    filter(ALL.SPECIES.REPORTED == 1) %>%
    group_by(gridg1) %>% 
    reframe(n = n_distinct(gridg0))
  
  spec25km = data %>%
    filter(ALL.SPECIES.REPORTED == 1,
           COMMON.NAME %in% dataf$COMMON.NAME) %>%
    distinct(COMMON.NAME, gridg1)
  
  samp25km5km = spec25km %>% 
    left_join(samp5km) %>%
    group_by(COMMON.NAME) %>% 
    reframe(mean5km = mean(n), 
            ci5km = 1.96*sd(n)/sqrt(n()))
  
  dataf = dataf %>% left_join(range25km) %>% left_join(samp25km5km)
  

  # writing filtered data files ---------------------------------------------

  # <annotation_pending_AV> short, about each file saved
  
  
  # <annotation_pending_AV>
  write.csv(dataf, row.names = F, 
            file = cur_metadata$FULLSPECLIST.PATH)
  
  
  # <annotation_pending_AV>
  locs_write = data0 %>% 
    filter(ALL.SPECIES.REPORTED == 1) %>%
    distinct(LOCALITY.ID, group.id, month, timegroups)
  
  write.csv(locs_write, row.names = F, 
            file = cur_metadata$LOCS.PATH)
  
  
  # <annotation_pending_AV>
  save(specieslist, restrictedspecieslist, databins, 
       file = cur_metadata$SPECLISTDATA.PATH)
  
  save(data, sampledcells, databins, stats, 
       file = cur_metadata$DATA.PATH)
  
}

  