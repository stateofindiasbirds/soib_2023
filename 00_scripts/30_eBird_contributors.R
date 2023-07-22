require(tidyverse)
require(glue)


# thresholds for selection
hist_listsX <- 100
full_listsY <- 2000
full_gridsZ <- 50
full_gridsZ_lists <- 10


# user info (not reproducible)

load(url("https://github.com/birdcountindia/ebird-datasets/raw/main/EBD/latest_non-EBD_paths.RData"))
userspath <- glue("../ebird-datasets/{userspath}")
groupaccspath <- glue("../ebird-datasets/{groupaccspath}")

eBird_users <- read.delim(userspath, 
                          sep = "\t", header = T, quote = "", 
                          stringsAsFactors = F, na.strings = c(""," ",NA)) %>% 
  transmute(OBSERVER.ID = observer_id,
            FULL.NAME = paste(first_name, last_name, sep = " "))

# list of group accounts to be filtered
groupaccs <- read_csv(groupaccspath) %>%
  mutate(CATEGORY = case_when(GA.1 == 1 ~ "GA.1", GA.2 == 1 ~ "GA.2", TRUE ~ "NG"))
filtGA <- groupaccs %>% 
  # both categories need to be filtered because this is birder-related 
  filter(CATEGORY == "GA.1" | CATEGORY == "GA.2") %>% 
  select(OBSERVER.ID)


# data used for SoIB 2023
load("01_analyses_full/dataforanalyses.RData")

data <- data %>% 
  dplyr::select(OBSERVER.ID, group.id, year, gridg1) %>% 
  left_join(eBird_users, by = "OBSERVER.ID") %>% 
  anti_join(filtGA, by = "OBSERVER.ID") %>% 
  filter(!(FULL.NAME %in% c("Mysore Amateur Naturalists Mysore")))

data_hist <- data %>% 
  filter(year <= 2000) 


# getting full list of contributors ------------------------------------------------------

# based on number of grids with threshold lists (how widely birded is the person)
all_contr1 <- data %>% 
  group_by(OBSERVER.ID, gridg1) %>% 
  reframe(NO.LISTS = n_distinct(group.id),
          FULL.NAME = first(FULL.NAME)) %>% 
  filter(NO.LISTS >= full_gridsZ_lists) %>% 
  group_by(OBSERVER.ID) %>% 
  reframe(NO.GRIDS = n_distinct(gridg1),
          FULL.NAME = first(FULL.NAME)) %>% 
  filter(NO.GRIDS >= full_gridsZ) %>% 
  arrange(desc(NO.GRIDS))

# based on number of lists
all_contr2 <- data %>% 
  group_by(OBSERVER.ID) %>% 
  reframe(TOT.LISTS = n_distinct(group.id),
          FULL.NAME = first(FULL.NAME)) %>% 
  arrange(desc(TOT.LISTS)) %>% 
  filter(TOT.LISTS >= full_listsY)

all_contr <- full_join(all_contr1, all_contr2) %>% 
  mutate(QUALIFICATION = "Total lists and grids")

# getting list of historical contributors -------------------------------------------

hist_contr <- data_hist %>% 
  group_by(OBSERVER.ID) %>% 
  reframe(HIST.LISTS = n_distinct(group.id),
          FULL.NAME = first(FULL.NAME)) %>% 
  arrange(desc(HIST.LISTS)) %>% 
  filter(HIST.LISTS >= hist_listsX) %>% 
  mutate(QUALIFICATION = "Historical lists")


# final list ------------------------------------------------------------------------

list_contr <- full_join(all_contr, hist_contr, by = c("OBSERVER.ID", "FULL.NAME")) %>% 
  relocate(OBSERVER.ID, FULL.NAME, NO.GRIDS, HIST.LISTS, TOT.LISTS) %>% 
  arrange(desc(NO.GRIDS), QUALIFICATION.y, desc(HIST.LISTS), QUALIFICATION.x, desc(TOT.LISTS), FULL.NAME)
