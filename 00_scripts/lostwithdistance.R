load("../ebird-datasets/EBD/ebd_IN_relNov-2022.RData")

slice_g <- data %>% 
  group_by(GROUP.ID) %>% 
  slice(1) %>% 
  ungroup()

x <- slice_g %>% 
  filter(is.na(EFFORT.DISTANCE.KM) | EFFORT.DISTANCE.KM <= 50) 

y <- slice_g %>% 
  filter(is.na(EFFORT.DISTANCE.KM) | EFFORT.DISTANCE.KM <= 10) 

z <- slice_g %>% 
  filter(is.na(EFFORT.DISTANCE.KM) | EFFORT.DISTANCE.KM <= 20) 

n_distinct(slice_g$GROUP.ID)
n_distinct(x$GROUP.ID)
n_distinct(y$GROUP.ID)
n_distinct(z$GROUP.ID)

lost <- anti_join(x, z)
  
save(lost, file = "00_data/lostwithdistance.RData")
