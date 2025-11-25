# determine sets of 4 and then 20 (if possible) key species for each state

library(tidyverse)

# Load data
load("00_data/dataforanalyses_extra.RData")
main <- read.csv("01_analyses_full/results/SoIB_main.csv")

range.status = main %>% select(eBird.English.Name.2024, SoIB.Latest.Range.Status)
india.checklist.map = main %>% select(eBird.English.Name.2024, India.Checklist.Common.Name)

###

# limiting the set of species to a set of criteria
specs.states = main %>%
  filter(Selected.SoIB == "X",
         SoIB.Latest.Range.Status != "Historical",
         ((IUCN.Category %in% c("Critically Endangered","Endangered","Vulnerable") &
             SoIB.Latest.Priority.Status == "Moderate") |
            (SoIB.Latest.Priority.Status == "High"))) %>%
  distinct(eBird.English.Name.2024) %>% 
  pull(eBird.English.Name.2024)

# set of endangered IUCN species, to use separately in further decision making
IUCN.end.species = main %>%
  filter(eBird.English.Name.2024 %in% specs.states, 
         IUCN.Category %in% c("Critically Endangered","Endangered")) %>%
  distinct(eBird.English.Name.2024) %>% 
  pull(eBird.English.Name.2024)

# calculate proportional range within each state for each of the selected species
key.state.species0 = data0 %>%
  filter(year > (soib_year_info("latest_year") - 5)) %>%
  filter(COMMON.NAME %in% specs.states) %>%
  dplyr::select(ST_NM,COMMON.NAME,gridg1) %>%
  group_by(COMMON.NAME) %>% 
  mutate(tot.range = n_distinct(gridg1)) %>%
  group_by(ST_NM,COMMON.NAME) %>% 
  reframe(prop.range = n_distinct(gridg1)/max(tot.range)) %>%
  left_join(main, by = c("COMMON.NAME" = "eBird.English.Name.2024")) %>%
  mutate(
    Current.Sort = case_when(SoIB.Latest.Current.Status %in% c("Insufficient Data","Trend Inconclusive") ~ 1,
                             TRUE ~ 0),
    Long.Term.Sort = case_when(SoIB.Latest.Long.Term.Status %in% c("Insufficient Data","Trend Inconclusive") ~ 1,
                               TRUE ~ 0)
  ) %>%
  group_by(ST_NM) %>% 
  arrange(ST_NM, desc(prop.range), Current.Sort, Long.Term.Sort, rangemean) %>%
  dplyr::select(ST_NM, COMMON.NAME, prop.range, SoIB.Latest.Current.Status, Current.Sort, 
                SoIB.Latest.Long.Term.Status, Long.Term.Sort, rangemean)

key.state.species = key.state.species0

# IUCN endangered species with porortional ranges
IUCN.end.state.species = key.state.species %>%
  filter(COMMON.NAME %in% IUCN.end.species)

# find out which states already do not have an IUCN endangered species included
IUCN.end.st = unique(IUCN.end.state.species$ST_NM)
st.list = setdiff(unique(data0$ST_NM), IUCN.end.st)
# set a flag that determines how many species from the 4 key species need to be 
# determined for each state because having one IUCN endangered species is 
# mandatory if possible (so only 3 from other means if one already exists)
IUCN.flag = data.frame(ST_NM = c(IUCN.end.st,st.list),
                       flag = c(rep(3, length(IUCN.end.st)), rep(4, length(st.list))))


# species with trend data available - these would be the base for determining the top 3-4
# based on proportional range
key.state.species.trends = key.state.species %>%
  filter(Long.Term.Sort == 0 | Current.Sort == 0) %>%
  group_by(ST_NM) %>% 
  slice(1) %>% 
  ungroup()

key.state.species.range = key.state.species %>%
  filter(Long.Term.Sort == 1 & Current.Sort == 1)

# select 3-4 top species based on proportional range
key.state.species.report = key.state.species.trends %>%
  bind_rows(key.state.species.range) %>%
  left_join(IUCN.flag) %>%
  arrange(desc(prop.range), .by_group=TRUE) %>%
  group_by(ST_NM) %>% 
  slice(1:max(flag)) %>% 
  ungroup() %>%
  select(-flag)

to.rem = key.state.species.report %>%
  distinct(ST_NM,COMMON.NAME)

# select the top IUCN species based on proportional range to add to the previous set
IUCN.add = key.state.species %>%
  filter(ST_NM %in% IUCN.end.st,
         COMMON.NAME %in% IUCN.end.species) %>%
  anti_join(to.rem) %>%
  group_by(ST_NM) %>% 
  arrange(desc(prop.range), .by_group=TRUE) %>%
  slice(1)

# combine!
key.state.species.report = key.state.species.report %>%
  bind_rows(IUCN.add) %>%
  group_by(ST_NM) %>% 
  arrange(desc(prop.range), .by_group=TRUE)

# check how many states have less than 4 species
check = key.state.species.report %>%
  group_by(ST_NM) %>% 
  reframe(n = n()) %>%
  filter(n < 4)

# find out how many are remaining for each state and get the remaining species
key.state.species.trends.extra = key.state.species %>%
  filter(ST_NM %in% check$ST_NM) %>%
  left_join(check) %>%
  mutate(n = 4 - n + 1) %>%
  filter(Long.Term.Sort == 0 | Current.Sort == 0) %>%
  group_by(ST_NM) %>% 
  arrange(desc(prop.range), .by_group=TRUE) %>%
  slice(2:max(n)) %>% 
  ungroup() %>%
  select(-n)

# combine!
key.state.species.report = key.state.species.report %>%
  bind_rows(key.state.species.trends.extra) %>%
  group_by(ST_NM) %>%
  arrange(desc(prop.range), .by_group=TRUE) %>%
  slice(1:4) %>% 
  ungroup()


## update IUCN flag to identify states that still do not have IUCN species

IUCN.rem = key.state.species.report %>% distinct(ST_NM,COMMON.NAME)

IUCN.end.state.species = key.state.species %>%
  filter(COMMON.NAME %in% IUCN.end.species) %>%
  anti_join(IUCN.rem)

IUCN.end.st = unique(IUCN.end.state.species$ST_NM)
st.list = setdiff(unique(data0$ST_NM), IUCN.end.st)
IUCN.flag = data.frame(ST_NM = c(IUCN.end.st,st.list),
                       flag = c(rep(3, length(IUCN.end.st)), rep(4, length(st.list))))

###


# again filter to 3 or 4 as required from the previous set of 4
key.state.species.report = key.state.species.report %>%
  left_join(IUCN.flag) %>%
  group_by(ST_NM) %>% 
  arrange(desc(prop.range), .by_group=TRUE) %>%
  slice(1:max(flag)) %>% 
  ungroup() %>%
  select(-flag)

# set of state/species combinations to exclude from the IUCN set
to.rem = key.state.species.report %>%
  distinct(ST_NM,COMMON.NAME)

# get IUCN species to add
IUCN.add = key.state.species %>%
  filter(ST_NM %in% IUCN.end.st,
         COMMON.NAME %in% IUCN.end.species) %>%
  anti_join(to.rem) %>%
  group_by(ST_NM) %>% 
  arrange(desc(prop.range), .by_group=TRUE) %>%
  slice(1)

# combine!
key.state.species.report = key.state.species.report %>%
  bind_rows(IUCN.add) %>% 
  group_by(ST_NM) %>% 
  arrange(desc(prop.range), .by_group=TRUE)

# this is now the starting set of 4 species that will be iteratively changed to
# determine the best set of 4 species per state so that repetitions between states are minimized
initial.report = key.state.species.report %>%
  select(-Current.Sort, -Long.Term.Sort, -rangemean) %>%
  left_join(range.status, by = c("COMMON.NAME" = "eBird.English.Name.2024"))

flag = 0

# get a set of species that have repeated across states and have a relatively
# low proportional contribution to range
check1 = key.state.species.report %>%
  filter(!COMMON.NAME %in% IUCN.end.species) %>%
  group_by(COMMON.NAME) %>% 
  reframe(n = n(), p = min(prop.range)) %>% 
  filter(n > 1 & p < 0.35) %>%
  distinct(COMMON.NAME) %>% 
  pull(COMMON.NAME)
check0 = check1

# remove repeated species and repeat the algorithm 10 times to iteratively reduce
# the number of repeated species that are not IUCN endangered

for(i in 1:10)
{
  comb.to.remove = key.state.species.report %>%
    filter(COMMON.NAME %in% check0) %>%
    group_by(COMMON.NAME) %>% 
    arrange(prop.range, .by_group=TRUE) %>%
    slice(1) %>%
    distinct(ST_NM, COMMON.NAME)
  
  key.state.species.report = key.state.species.report %>%
    anti_join(comb.to.remove)
  
  check = key.state.species.report %>%
    group_by(ST_NM) %>% 
    reframe(n = n()) %>%
    filter(n < 4)
  
  combs.to.avoid = key.state.species.report %>%
    distinct(ST_NM, COMMON.NAME)
  
  
  if (length(check$ST_NM) > 0)
  {
    key.state.species.extra = key.state.species %>%
      anti_join(combs.to.avoid) %>%
      filter(ST_NM %in% check$ST_NM) %>%
      left_join(check) %>% 
      mutate(n = 4 - n + i - 1) %>%
      group_by(ST_NM) %>% 
      arrange(desc(prop.range), .by_group=TRUE) %>%
      slice(i:max(n)) %>% 
      ungroup() %>% 
      select(-n)
    
    key.state.species.report = key.state.species.report %>%
      bind_rows(key.state.species.extra) %>%
      group_by(ST_NM) %>% 
      arrange(desc(prop.range), .by_group=TRUE) %>%
      slice(1:4) %>% 
      ungroup()
  }
  
  check1.old = check1
  
  check0 = key.state.species.report %>%
    filter(!COMMON.NAME %in% IUCN.end.species) %>%
    group_by(COMMON.NAME) %>% 
    reframe(n = n(), p = min(prop.range)) %>% 
    filter(n > 1 & p < 0.35) %>%
    distinct(COMMON.NAME) %>% 
    pull(COMMON.NAME)
  
  check1 = union(check0, check1.old)
  
  print(length(check0))
  
  if (length(check0) == 0)
    break
}

combs.to.avoid = key.state.species.report %>%
  distinct(ST_NM,COMMON.NAME)

# check to see which states have less than 4 species
check = key.state.species.report %>%
  group_by(ST_NM) %>% 
  reframe(n = n()) %>%
  filter(n < 4)

# add the remainder
key.state.species.extra = key.state.species %>%
  anti_join(combs.to.avoid) %>%
  filter(ST_NM %in% check$ST_NM) %>%
  left_join(check) %>% 
  mutate(n = 4 - n) %>%
  group_by(ST_NM) %>% 
  arrange(desc(prop.range), .by_group=TRUE) %>%
  slice(1:max(n)) %>% 
  ungroup() %>% 
  select(-n)

key.state.species.report = key.state.species.report %>%
  bind_rows(key.state.species.extra) %>%
  group_by(ST_NM) %>% 
  arrange(desc(prop.range), .by_group=TRUE) %>%
  slice(1:4) %>% 
  ungroup()

key.state.species.report = key.state.species.report %>%
  select(-Current.Sort, -Long.Term.Sort, -rangemean) %>%
  left_join(range.status, by = c("COMMON.NAME" = "eBird.English.Name.2024"))



state.species = key.state.species.report %>%
  distinct(ST_NM,COMMON.NAME)

# get a list of state/species combinations from the initial list that have relatively 
# high proportional contributions to range - only for species that don't figure in
# the latest list
initial.report.range = initial.report %>%
  filter(SoIB.Latest.Current.Status %in% c("Insufficient Data","Trend Inconclusive"),
         SoIB.Latest.Long.Term.Status %in% c("Insufficient Data","Trend Inconclusive")) %>%
  anti_join(state.species) %>%
  filter(prop.range > 0.05)

# for those states alone, get a set of species that can be removed from the 
# latest list because there may be good substitutes available
key.state.species.report.temp = key.state.species.report %>%
  filter(prop.range < 0.08, 
         ST_NM %in% initial.report.range$ST_NM) %>%
  group_by(ST_NM) %>% 
  arrange(prop.range, .by_group=TRUE) %>%
  slice(1) %>%
  distinct(ST_NM, COMMON.NAME)

# state/species combinations to remove
count.rem = key.state.species.report.temp %>%
  group_by(ST_NM) %>% 
  reframe(n = n())

# get combinations to substitute that with
initial.report.range.temp = initial.report.range %>% 
  left_join(count.rem) %>%
  filter(ST_NM %in% key.state.species.report.temp$ST_NM) %>%
  group_by(ST_NM) %>%
  slice(1:max(n)) %>%
  dplyr::select(-n)

# remove and add
key.state.species.report = key.state.species.report %>%
  anti_join(key.state.species.report.temp) %>%
  bind_rows(initial.report.range.temp) %>%
  group_by(ST_NM) %>% 
  arrange(desc(prop.range), .by_group=TRUE) %>% 
  ungroup()

# remove any state/species combination where the proportional range is too small
low.prob.comb = key.state.species.report %>%
  filter(prop.range < 0.01) %>%
  distinct(ST_NM, COMMON.NAME)
  
low.prob.check = low.prob.comb %>%
  group_by(ST_NM) %>%
  reframe(n = n())

state.species = key.state.species.report %>%
  distinct(ST_NM, COMMON.NAME)

initial.report.rem = initial.report %>%
  anti_join(state.species) %>%
  filter(ST_NM %in% low.prob.check$ST_NM) %>%
  left_join(low.prob.check) %>%
  group_by(ST_NM) %>% 
  arrange(desc(prop.range), .by_group=TRUE) %>%
  slice(1:max(n)) %>%
  select(-n)

# get an updated set of 4 species
key.state.species.report = key.state.species.report %>%
  bind_rows(initial.report.rem) %>%
  group_by(ST_NM) %>% 
  arrange(desc(prop.range), .by_group=TRUE) %>% 
  slice(1:4) %>%
  rename(eBird.English.Name.2024 = COMMON.NAME)

# add important species case by case that have not made it to the list of 4
miss.spec = data.frame(
  ST_NM = c(
    "Manipur","Andhra Pradesh","Uttarakhand","Himachal Pradesh","Gujarat","Kerala",
    "Andaman and Nicobar Islands","Bihar","Maharashtra","West Bengal","Assam","Assam","Goa"
  ),
  eBird.English.Name.2024 = c(
    "Manipur Bush-Quail","Jerdon's Courser","Finn's Weaver","Ruddy Shelduck","Greater Flamingo",
    "Garganey","Nicobar Megapode","Northern Pintail","Indian Courser","Rufous-necked Hornbill",
    "White-winged Duck","Bengal Florican","Black-capped Kingfisher"
  )
)

add.final = main %>% 
  filter(eBird.English.Name.2024 %in% c(
    "Manipur Bush-Quail","Jerdon's Courser","Finn's Weaver","Ruddy Shelduck","Greater Flamingo",
    "Garganey","Nicobar Megapode","Northern Pintail","Indian Courser","Rufous-necked Hornbill",
    "White-winged Duck","Bengal Florican","Black-capped Kingfisher"
  )) %>%
  left_join(miss.spec) %>%
  dplyr::select(ST_NM, eBird.English.Name.2024, 
                SoIB.Latest.Current.Status, SoIB.Latest.Long.Term.Status, SoIB.Latest.Range.Status)

key.state.temp = key.state.species %>% 
  dplyr::select(ST_NM, COMMON.NAME, prop.range) %>%
  rename(eBird.English.Name.2024 = COMMON.NAME)
  
prop.to.add = add.final %>%
  left_join(key.state.temp) %>%
  dplyr::select(ST_NM, eBird.English.Name.2024, prop.range,
                SoIB.Latest.Current.Status, SoIB.Latest.Long.Term.Status, SoIB.Latest.Range.Status)

key.state.species.report = key.state.species.report %>%
  filter((ST_NM != "Manipur" | eBird.English.Name.2024 != "Yellow-breasted Bunting") &
           (ST_NM != "Andhra Pradesh" | eBird.English.Name.2024 != "Yellow-throated Bulbul") &
           (ST_NM != "Uttarakhand" | eBird.English.Name.2024 != "Rufous-vented Tit") &
           (ST_NM != "Himachal Pradesh" | eBird.English.Name.2024 != "Tibetan Blackbird") &
           (ST_NM != "Gujarat" | eBird.English.Name.2024 != "Forest Owlet") &
           (ST_NM != "Kerala" | eBird.English.Name.2024 != "Nilgiri Laughingthrush") &
           (ST_NM != "Andaman and Nicobar Islands" | eBird.English.Name.2024 != "Nicobar Bulbul") &
           (ST_NM != "Bihar" | eBird.English.Name.2024 != "Great Slaty Woodpecker") &
           (ST_NM != "Maharashtra" | eBird.English.Name.2024 != "Great Knot") &
           (ST_NM != "West Bengal" | eBird.English.Name.2024 != "Mangrove Pitta") &
           (ST_NM != "Assam" | eBird.English.Name.2024 != "Greater Adjutant") &
           (ST_NM != "Assam" | eBird.English.Name.2024 != "Finn's Weaver") &
           (ST_NM != "Goa" | eBird.English.Name.2024 != "Sanderling")) %>%
  bind_rows(prop.to.add) %>%
  group_by(ST_NM) %>% 
  arrange(desc(prop.range), .by_group=TRUE)


# check to see if there are any states that are missing species
check3 = key.state.species0 %>%
  select(-Current.Sort, -Long.Term.Sort, -rangemean) %>%
  left_join(range.status, by = c("COMMON.NAME" = "eBird.English.Name.2024")) %>%
  group_by(ST_NM) %>% 
  arrange(desc(prop.range), .by_group = TRUE) %>%
  slice(1:4) %>% 
  ungroup() %>%
  rename(eBird.English.Name.2024 = COMMON.NAME)

check3.small = check3 %>%
  filter(ST_NM %in% c("Chhattisgarh","Puducherry"))

key.state.species.report = key.state.species.report %>%
  anti_join(check3.small) %>%
  bind_rows(check3.small) %>%
  group_by(ST_NM) %>% 
  arrange(desc(prop.range), .by_group=TRUE) %>% 
  slice(1:4)

# get a final set of 4 species
key.state.species.report.final = key.state.species.report %>%
  left_join(india.checklist.map) %>%
  mutate(India.Checklist.Common.Name = factor(India.Checklist.Common.Name, levels = unique(india.checklist.map$India.Checklist.Common.Name))) %>%
  select(ST_NM,India.Checklist.Common.Name,prop.range,SoIB.Latest.Long.Term.Status,SoIB.Latest.Current.Status,SoIB.Latest.Range.Status) %>%
  group_by(ST_NM) %>% arrange(India.Checklist.Common.Name, .by_group = T)

# write
write.csv(key.state.species.report.final, "01_analyses_full/results/key_state_species_top4.csv", row.names = F)






# creating the larger list that will include any wetland species that meet the
# 1 % criterion in any state


one.perc = main %>% 
  filter(!is.na(Onepercent.Estimates)) %>%
  dplyr::select(eBird.English.Name.2024, Onepercent.Estimates)

full.list.one.perc = data0 %>%
  rename(eBird.English.Name.2024 = COMMON.NAME) %>%
  filter(year > (soib_year_info("latest_year") - 5)) %>%
  left_join(one.perc) %>%
  filter(!is.na(Onepercent.Estimates), 
         OBSERVATION.COUNT != "X") %>%
  mutate(OBSERVATION.COUNT = as.numeric(OBSERVATION.COUNT)) %>%
  filter(OBSERVATION.COUNT >= Onepercent.Estimates, 
         eBird.English.Name.2024 %in% specs.states) %>%
  distinct(ST_NM, eBird.English.Name.2024) %>%
  arrange(ST_NM)

full.list.one.perc.prop = key.state.species0 %>%
  rename(eBird.English.Name.2024 = COMMON.NAME) %>%
  select(ST_NM, eBird.English.Name.2024, prop.range) %>%
  semi_join(full.list.one.perc) %>%
  group_by(ST_NM) %>% 
  arrange(desc(prop.range), .by_group = TRUE) %>%
  ungroup()

full.list.4 = key.state.species.report %>%
  anti_join(full.list.one.perc) %>%
  distinct(ST_NM, eBird.English.Name.2024) %>%
  arrange(ST_NM) %>%
  bind_rows(full.list.one.perc)

full.list.4.prop = key.state.species0 %>%
  rename(eBird.English.Name.2024 = COMMON.NAME) %>%
  select(ST_NM, eBird.English.Name.2024, prop.range) %>%
  semi_join(full.list.4) %>%
  group_by(ST_NM) %>% 
  arrange(desc(prop.range), .by_group = TRUE) %>%
  ungroup()

ext = full.list.4 %>%
  anti_join(full.list.4.prop[, 1:2]) %>%
  mutate(prop.range = 0)
  
full.list.4.prop = full.list.4.prop %>%
  bind_rows(ext)

num.spec = full.list.4 %>%
  group_by(ST_NM) %>%
  reframe(n = n())

# in addition to the 4 and 1 % species, include any species in a state when the proportional
# contribution to range > 0.35
full.list.prop.rem = data0 %>%
  rename(eBird.English.Name.2024 = COMMON.NAME) %>%
  filter(year > (soib_year_info("latest_year") - 5)) %>%
  filter(eBird.English.Name.2024 %in% specs.states) %>%
  group_by(eBird.English.Name.2024) %>%
  mutate(tot.range = n_distinct(gridg1)) %>%
  group_by(ST_NM, eBird.English.Name.2024) %>%
  reframe(prop.range = n_distinct(gridg1) / max(tot.range)) %>%
  dplyr::select(ST_NM, eBird.English.Name.2024, prop.range) %>%
  anti_join(full.list.4) %>%
  left_join(num.spec) %>%
  mutate(n = case_when(is.na(n) ~ 0, TRUE ~ n)) %>%
  group_by(ST_NM) %>% 
  arrange(desc(prop.range), .by_group = TRUE) %>%
  slice(1:(20 - max(n))) %>%
  dplyr::select(-n) %>%
  filter(prop.range >= 0.035)

full.list.prop.comb = full.list.prop.rem %>%
  bind_rows(full.list.4.prop) %>%
  group_by(ST_NM) %>% 
  arrange(desc(prop.range), .by_group = TRUE) %>%
  ungroup()

# final full list
key.state.species.full.list = full.list.prop.comb %>%
  left_join(india.checklist.map) %>%
  mutate(India.Checklist.Common.Name = factor(India.Checklist.Common.Name, levels = unique(india.checklist.map$India.Checklist.Common.Name))) %>%
  select(ST_NM,India.Checklist.Common.Name,prop.range) %>%
  group_by(ST_NM) %>% arrange(India.Checklist.Common.Name, .by_group = T)
 
# write
write.csv(key.state.species.full.list, "01_analyses_full/key_state_species_full_list.csv", row.names = F)
  select(ST_NM, India.Checklist.Common.Name, prop.range)

write.csv(key.state.species.full.list, "01_analyses_full/results/key_state_species_full.csv", row.names = F)

# making comma separated lists
delim.state.list = key.state.species.full.list %>%
  group_by(ST_NM) %>%
  summarise(India.Checklist.Common.Name = toString(India.Checklist.Common.Name)) %>%
  ungroup()

write.csv(delim.state.list, "01_analyses_full/results/key_state_species_delim.csv", row.names = F)


rm(list = ls()[!ls() %in% c("data0", "key.state.species0")])

