library(tidyverse)

trends1 = read.csv("trends_1.csv")
trends = rbind(trends1)

trends = trends %>%
  group_by(COMMON.NAME,timegroupsf,timegroups) %>% 
  summarize(mean = mean(freq), se = sd(!is.na(freq)) + sqrt(sum(se^2))/n()) %>%
  ungroup()

trends$COMMON.NAME = factor(trends$COMMON.NAME, levels = specieslist$COMMON.NAME)
trends = trends %>%
  arrange(COMMON.NAME,timegroups)