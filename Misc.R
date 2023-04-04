clem = read.csv("SoIB_mapping_2022_temp.csv")
ind = read.csv("India-Checklist_v7_0.csv")

library(tidyverse)
map2022 = left_join(clem,ind)

write.csv(map2022,"SoIB_mapping_2022.csv",row.names=F)

