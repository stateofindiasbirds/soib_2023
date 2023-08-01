# <annotation_pending_AV> !

library(tidyverse)

# Load data
load("00_data/dataforanalyses_extra.RData")
main <- read.csv("01_analyses_full/results/SoIB_main.csv")

range.status = main %>% select(eBird.English.Name.2022, SOIBv2.Range.Status)
india.checklist.map = main %>% select(eBird.English.Name.2022, India.Checklist.Common.Name)

###

specs.states = main %>%
  filter(Selected.SOIB == "X",
         SOIBv2.Range.Status != "Historical",
         ((IUCN.Category %in% c("Critically Endangered","Endangered","Vulnerable") &
             SOIBv2.Priority.Status == "Moderate") |
            (SOIBv2.Priority.Status == "High"))) %>%
  distinct(eBird.English.Name.2022) %>% 
  pull(eBird.English.Name.2022)

IUCN.end.species = main %>%
  filter(eBird.English.Name.2022 %in% specs.states, 
         IUCN.Category %in% c("Critically Endangered","Endangered")) %>%
  distinct(eBird.English.Name.2022) %>% 
  pull(eBird.English.Name.2022)

key.state.species0 = data0 %>%
  filter(year > 2017) %>%
  filter(COMMON.NAME %in% specs.states) %>%
  dplyr::select(ST_NM,COMMON.NAME,gridg1) %>%
  group_by(COMMON.NAME) %>% 
  mutate(tot.range = n_distinct(gridg1)) %>%
  group_by(ST_NM,COMMON.NAME) %>% 
  reframe(prop.range = n_distinct(gridg1)/max(tot.range)) %>%
  left_join(main, by = c("COMMON.NAME" = "eBird.English.Name.2022")) %>%
  mutate(
    Current.Sort = case_when(SOIBv2.Current.Status %in% c("Insufficient Data","Trend Inconclusive") ~ 1,
                             TRUE ~ 0),
    Long.Term.Sort = case_when(SOIBv2.Long.Term.Status %in% c("Insufficient Data","Trend Inconclusive") ~ 1,
                               TRUE ~ 0)
  ) %>%
  group_by(ST_NM) %>% 
  arrange(ST_NM, desc(prop.range), Current.Sort, Long.Term.Sort, rangemean) %>%
  dplyr::select(ST_NM, COMMON.NAME, prop.range, SOIBv2.Current.Status, Current.Sort, 
                SOIBv2.Long.Term.Status, Long.Term.Sort, rangemean)

key.state.species = key.state.species0

IUCN.end.state.species = key.state.species %>%
  filter(COMMON.NAME %in% IUCN.end.species)

IUCN.end.st = unique(IUCN.end.state.species$ST_NM)
st.list = setdiff(unique(data0$ST_NM), IUCN.end.st)
IUCN.flag = data.frame(ST_NM = c(IUCN.end.st,st.list),
                       flag = c(rep(3, length(IUCN.end.st)), rep(4, length(st.list))))



key.state.species.trends = key.state.species %>%
  filter(Long.Term.Sort == 0 | Current.Sort == 0) %>%
  group_by(ST_NM) %>% 
  slice(1) %>% 
  ungroup()

key.state.species.range = key.state.species %>%
  filter(Long.Term.Sort == 1 & Current.Sort == 1)

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
IUCN.add = key.state.species %>%
  filter(ST_NM %in% IUCN.end.st,
         COMMON.NAME %in% IUCN.end.species) %>%
  anti_join(to.rem) %>%
  group_by(ST_NM) %>% 
  arrange(desc(prop.range), .by_group=TRUE) %>%
  slice(1)
key.state.species.report = key.state.species.report %>%
  bind_rows(IUCN.add) %>%
  group_by(ST_NM) %>% 
  arrange(desc(prop.range), .by_group=TRUE)


check = key.state.species.report %>%
  group_by(ST_NM) %>% 
  reframe(n = n()) %>%
  filter(n < 4)

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

key.state.species.report = key.state.species.report %>%
  bind_rows(key.state.species.trends.extra) %>%
  group_by(ST_NM) %>%
  arrange(desc(prop.range), .by_group=TRUE) %>%
  slice(1:4) %>% 
  ungroup()


## update IUCN flag

IUCN.rem = key.state.species.report %>% distinct(ST_NM,COMMON.NAME)

IUCN.end.state.species = key.state.species %>%
  filter(COMMON.NAME %in% IUCN.end.species) %>%
  anti_join(IUCN.rem)

IUCN.end.st = unique(IUCN.end.state.species$ST_NM)
st.list = setdiff(unique(data0$ST_NM), IUCN.end.st)
IUCN.flag = data.frame(ST_NM = c(IUCN.end.st,st.list),
                       flag = c(rep(3, length(IUCN.end.st)), rep(4, length(st.list))))

###





key.state.species.report = key.state.species.report %>%
  left_join(IUCN.flag) %>%
  group_by(ST_NM) %>% 
  arrange(desc(prop.range), .by_group=TRUE) %>%
  slice(1:max(flag)) %>% 
  ungroup() %>%
  select(-flag)
to.rem = key.state.species.report %>%
  distinct(ST_NM,COMMON.NAME)
IUCN.add = key.state.species %>%
  filter(ST_NM %in% IUCN.end.st,
         COMMON.NAME %in% IUCN.end.species) %>%
  anti_join(to.rem) %>%
  group_by(ST_NM) %>% 
  arrange(desc(prop.range), .by_group=TRUE) %>%
  slice(1)
key.state.species.report = key.state.species.report %>%
  bind_rows(IUCN.add) %>% 
  group_by(ST_NM) %>% 
  arrange(desc(prop.range), .by_group=TRUE)

initial.report = key.state.species.report %>%
  select(-Current.Sort, -Long.Term.Sort, -rangemean) %>%
  left_join(range.status, by = c("COMMON.NAME" = "eBird.English.Name.2022"))

flag = 0
check1 = key.state.species.report %>%
  filter(!COMMON.NAME %in% IUCN.end.species) %>%
  group_by(COMMON.NAME) %>% 
  reframe(n = n(), p = min(prop.range)) %>% 
  filter(n > 1 & p < 0.35) %>%
  distinct(COMMON.NAME) %>% 
  pull(COMMON.NAME)
check0 = check1

## remove repeated species

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

check = key.state.species.report %>%
  group_by(ST_NM) %>% 
  reframe(n = n()) %>%
  filter(n < 4)

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
  left_join(range.status, by = c("COMMON.NAME" = "eBird.English.Name.2022"))



state.species = key.state.species.report %>%
  distinct(ST_NM,COMMON.NAME)

initial.report.range = initial.report %>%
  filter(SOIBv2.Current.Status %in% c("Insufficient Data","Trend Inconclusive"),
         SOIBv2.Long.Term.Status %in% c("Insufficient Data","Trend Inconclusive")) %>%
  anti_join(state.species) %>%
  filter(prop.range > 0.05)

key.state.species.report.temp = key.state.species.report %>%
  filter(prop.range < 0.08, 
         ST_NM %in% initial.report.range$ST_NM) %>%
  group_by(ST_NM) %>% 
  arrange(prop.range, .by_group=TRUE) %>%
  slice(1) %>%
  distinct(ST_NM, COMMON.NAME)

count.rem = key.state.species.report.temp %>%
  group_by(ST_NM) %>% 
  reframe(n = n())


initial.report.range.temp = initial.report.range %>% 
  left_join(count.rem) %>%
  filter(ST_NM %in% key.state.species.report.temp$ST_NM) %>%
  group_by(ST_NM) %>%
  slice(1:max(n)) %>%
  dplyr::select(-n)

key.state.species.report = key.state.species.report %>%
  anti_join(key.state.species.report.temp) %>%
  bind_rows(initial.report.range.temp) %>%
  group_by(ST_NM) %>% 
  arrange(desc(prop.range), .by_group=TRUE) %>% 
  ungroup()
  
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

key.state.species.report = key.state.species.report %>%
  bind_rows(initial.report.rem) %>%
  group_by(ST_NM) %>% 
  arrange(desc(prop.range), .by_group=TRUE) %>% 
  slice(1:4) %>%
  rename(eBird.English.Name.2022 = COMMON.NAME)

miss.spec = data.frame(
  ST_NM = c(
    "Manipur","Andhra Pradesh","Uttarakhand","Himachal Pradesh","Gujarat","Kerala",
    "Andaman and Nicobar Islands","Bihar","Maharashtra","West Bengal","Assam","Assam","Goa"
  ),
  eBird.English.Name.2022 = c(
    "Manipur Bush-Quail","Jerdon's Courser","Finn's Weaver","Ruddy Shelduck","Greater Flamingo",
    "Garganey","Nicobar Megapode","Northern Pintail","Indian Courser","Rufous-necked Hornbill",
    "White-winged Duck","Bengal Florican","Black-capped Kingfisher"
  )
)

add.final = main %>% 
  filter(eBird.English.Name.2022 %in% c(
    "Manipur Bush-Quail","Jerdon's Courser","Finn's Weaver","Ruddy Shelduck","Greater Flamingo",
    "Garganey","Nicobar Megapode","Northern Pintail","Indian Courser","Rufous-necked Hornbill",
    "White-winged Duck","Bengal Florican","Black-capped Kingfisher"
  )) %>%
  left_join(miss.spec) %>%
  dplyr::select(ST_NM, eBird.English.Name.2022, 
                SOIBv2.Current.Status, SOIBv2.Long.Term.Status, SOIBv2.Range.Status)

key.state.temp = key.state.species %>% 
  dplyr::select(ST_NM, COMMON.NAME, prop.range) %>%
  rename(eBird.English.Name.2022 = COMMON.NAME)
  
prop.to.add = add.final %>%
  left_join(key.state.temp) %>%
  dplyr::select(ST_NM, eBird.English.Name.2022, prop.range,
                SOIBv2.Current.Status, SOIBv2.Long.Term.Status, SOIBv2.Range.Status)

key.state.species.report = key.state.species.report %>%
  filter((ST_NM != "Manipur" | eBird.English.Name.2022 != "Yellow-breasted Bunting") &
           (ST_NM != "Andhra Pradesh" | eBird.English.Name.2022 != "Yellow-throated Bulbul") &
           (ST_NM != "Uttarakhand" | eBird.English.Name.2022 != "Rufous-vented Tit") &
           (ST_NM != "Himachal Pradesh" | eBird.English.Name.2022 != "Tibetan Blackbird") &
           (ST_NM != "Gujarat" | eBird.English.Name.2022 != "Forest Owlet") &
           (ST_NM != "Kerala" | eBird.English.Name.2022 != "Nilgiri Laughingthrush") &
           (ST_NM != "Andaman and Nicobar Islands" | eBird.English.Name.2022 != "Nicobar Bulbul") &
           (ST_NM != "Bihar" | eBird.English.Name.2022 != "Great Slaty Woodpecker") &
           (ST_NM != "Maharashtra" | eBird.English.Name.2022 != "Great Knot") &
           (ST_NM != "West Bengal" | eBird.English.Name.2022 != "Mangrove Pitta") &
           (ST_NM != "Assam" | eBird.English.Name.2022 != "Greater Adjutant") &
           (ST_NM != "Assam" | eBird.English.Name.2022 != "Finn's Weaver") &
           (ST_NM != "Goa" | eBird.English.Name.2022 != "Sanderling")) %>%
  bind_rows(prop.to.add) %>%
  group_by(ST_NM) %>% 
  arrange(desc(prop.range), .by_group=TRUE)

check3 = key.state.species0 %>%
  select(-Current.Sort, -Long.Term.Sort, -rangemean) %>%
  left_join(range.status, by = c("COMMON.NAME" = "eBird.English.Name.2022")) %>%
  group_by(ST_NM) %>% 
  arrange(desc(prop.range), .by_group = TRUE) %>%
  slice(1:4) %>% 
  ungroup() %>%
  rename(eBird.English.Name.2022 = COMMON.NAME)

check3.small = check3 %>%
  filter(ST_NM %in% c("Chhattisgarh","Puducherry"))

key.state.species.report = key.state.species.report %>%
  anti_join(check3.small) %>%
  bind_rows(check3.small) %>%
  group_by(ST_NM) %>% 
  arrange(desc(prop.range), .by_group=TRUE) %>% 
  slice(1:4)

key.state.species.report.final = key.state.species.report %>%
  left_join(india.checklist.map) %>%
  select(ST_NM, India.Checklist.Common.Name, prop.range, 
         SOIBv2.Long.Term.Status, SOIBv2.Current.Status, SOIBv2.Range.Status)


write.csv(key.state.species.report.final, "01_analyses_full/results/key_state_species_4.csv", row.names = F)









one.perc = main %>% 
  filter(!is.na(Onepercent.Estimates)) %>%
  dplyr::select(eBird.English.Name.2022, Onepercent.Estimates)

full.list.one.perc = data0 %>%
  rename(eBird.English.Name.2022 = COMMON.NAME) %>%
  filter(year > 2017) %>%
  left_join(one.perc) %>%
  filter(!is.na(Onepercent.Estimates), 
         OBSERVATION.COUNT != "X") %>%
  mutate(OBSERVATION.COUNT = as.numeric(OBSERVATION.COUNT)) %>%
  filter(OBSERVATION.COUNT >= Onepercent.Estimates, 
         eBird.English.Name.2022 %in% specs.states) %>%
  distinct(ST_NM, eBird.English.Name.2022) %>%
  arrange(ST_NM)

full.list.one.perc.prop = key.state.species0 %>%
  rename(eBird.English.Name.2022 = COMMON.NAME) %>%
  select(ST_NM, eBird.English.Name.2022, prop.range) %>%
  semi_join(full.list.one.perc) %>%
  group_by(ST_NM) %>% 
  arrange(desc(prop.range), .by_group = TRUE) %>%
  ungroup()

full.list.4 = key.state.species.report %>%
  anti_join(full.list.one.perc) %>%
  distinct(ST_NM, eBird.English.Name.2022) %>%
  arrange(ST_NM) %>%
  bind_rows(full.list.one.perc)

full.list.4.prop = key.state.species0 %>%
  rename(eBird.English.Name.2022 = COMMON.NAME) %>%
  select(ST_NM, eBird.English.Name.2022, prop.range) %>%
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


full.list.prop.rem = data0 %>%
  rename(eBird.English.Name.2022 = COMMON.NAME) %>%
  filter(year > 2017) %>%
  filter(eBird.English.Name.2022 %in% specs.states) %>%
  group_by(eBird.English.Name.2022) %>%
  mutate(tot.range = n_distinct(gridg1)) %>%
  group_by(ST_NM, eBird.English.Name.2022) %>%
  reframe(prop.range = n_distinct(gridg1) / max(tot.range)) %>%
  dplyr::select(ST_NM, eBird.English.Name.2022, prop.range) %>%
  anti_join(full.list.4) %>%
  left_join(num.spec) %>%
  mutate(n = case_when(is.na(n) ~ 0, TRUE ~ n)) %>%
  group_by(ST_NM) %>% 
  arrange(desc(prop.range), .by_group = TRUE) %>%
  slice(1:(20 - max(n))) %>%
  dplyr::select(-n) %>%
  filter(prop.range >= 0.05)

full.list.prop.comb = full.list.prop.rem %>%
  bind_rows(full.list.4.prop) %>%
  group_by(ST_NM) %>% 
  arrange(desc(prop.range), .by_group = TRUE) %>%
  ungroup()


key.state.species.full.list = full.list.prop.comb %>%
  left_join(india.checklist.map) %>%
  select(ST_NM, India.Checklist.Common.Name, prop.range)

write.csv(key.state.species.full.list, "01_analyses_full/results/key_state_species_full_list.csv", row.names = F)

delim.state.list = key.state.species.full.list %>%
  group_by(ST_NM) %>%
  summarise(India.Checklist.Common.Name = toString(India.Checklist.Common.Name)) %>%
  ungroup()

write.csv(delim.state.list, "01_analyses_full/results/key_state_species_full_list_delim.csv", row.names = F)


rm(list = ls()[!ls() %in% c("data0", "key.state.species0")])

