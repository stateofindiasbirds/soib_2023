i = 1

queen_neighbours = our_neighbours

###

require(tidyverse)
require(reshape2)
require(data.table)
require(unmarked)


species = speciesforocc$eBird.English.Name.2022[i]
status = speciesforocc$status[i]

data_filt_mig <- data %>% 
  # filter (or not) the data based on migratory status
  filt_data_for_mig(species, status)

# in each iteration (based on the species) we want a different value of medianlla
# to predict
medianlla = data_filt_mig %>%
  group_by(gridg1, group.id) %>% 
  slice(1) %>% 
  group_by(gridg1) %>%
  reframe(medianlla = median(no.sp)) %>%
  reframe(medianlla = round(mean(medianlla))) %>% 
  pull(medianlla)

# expanding data for absences also
data_exp = expandbyspecies(data_filt_mig, species) %>% 
  # converting months to seasons
  mutate(month = as.numeric(month)) %>% 
  mutate(month = case_when(month %in% c(12, 1, 2) ~ "Win",
                           month %in% c(3, 4, 5) ~ "Sum",
                           month %in% c(6, 7, 8) ~ "Mon",
                           month %in% c(9, 10, 11) ~ "Aut")) %>% 
  mutate(month = as.factor(month))

# reordering the checklists within each grid to minimise bias
data_exp = data_exp[sample(x = 1:nrow(data_exp)),] 

# number of checklists per grid cell
lists_per_grid = data_exp %>%
  group_by(gridg1) %>% 
  reframe(lpg = n())

# deciding a cutoff threshold based on 95% quantile---ignore all lists after that
listcutoff = quantile(lists_per_grid$lpg, 0.95, na.rm = TRUE)

data_exp = data_exp %>%
  arrange(gridg1) %>%
  group_by(gridg1) %>% 
  mutate(group.id = 1:n()) %>% 
  ungroup()

# presences and absences
occdata_full = data_exp %>%
  group_by(gridg1) %>% 
  # does the grid have the species?
  summarize(presence = sum(OBSERVATION.COUNT)) %>%
  mutate(presence = replace(presence, presence > 1, 1),
         # initialising this column which will later have info on 
         # proportion of neighbouring cells that have the species
         prop_nb = 0,
         gridg1 = as.character(gridg1))

occdata_cell_nb <- occdata_full %>% 
  # numeric vector of neighbours of each cell being iterated over
  mutate(nb_list = map(gridg1, ~ as.numeric(queen_neighbours[[as.numeric(.)]]))) %>% 
  # this gives fewer neighbours than above, because not all neighbours are in data
  # (and even fewer with complete lists, etc.)
  mutate(occdata_nb = map(nb_list, ~ occdata_full %>% 
                            filter(gridg1 %in% .x) %>% 
                            pull(presence))) %>% 
  # numerator: total number of neighbour cells that have the species
  # denominator should be all neighbour cells
  mutate(prop_nb = map2_dbl(occdata_nb, nb_list, ~ sum(.x)/length(.y))) %>%
  dplyr::select(-nb_list, -occdata_nb)

# absences
occdata_abs = occdata_cell_nb %>% filter(presence != 1)

occdata_cell_nb = occdata_cell_nb %>% dplyr::select(-presence)


# creating matrices for occupancy
# need data.table because very large objects
setDT(data_exp)

# matrices of detection/non-, season, and median list length (matrix needed for occ)
det = dcast(data_exp, gridg1 ~ group.id, value.var = "OBSERVATION.COUNT")
cov.month = dcast(data_exp, gridg1 ~ group.id, value.var = "month")
cov.nosp = dcast(data_exp, gridg1 ~ group.id, value.var = "no.sp")

# back to dataframe
det = setDF(det)
cov.month = setDF(cov.month)
cov.nosp = setDF(cov.nosp)

# for every grid cell, selecting only N lists; this removes outlier grid cells 
# like Bangalore
det = det[, 1:listcutoff]
cov.month = cov.month[, 1:listcutoff]
cov.nosp = cov.nosp[, 1:listcutoff]


# input to occupancy modelling
occdata_UFO = unmarkedFrameOccu(
  y = det[, -1], # response (only 1s and 0s)
  siteCovs = data.frame(prop_nb = occdata_cell_nb$prop_nb),
  obsCovs = list(cov1 = cov.nosp[, -1],
                 cov2 = cov.month[, -1])
)

# if not resident, we don't use seasonality as a covariate because data 
# already filtered to those months
if (status == "R") {
  
  occ_det = tryCatch({occu(~ log(cov1) * cov2 ~ prop_nb,
                           data = occdata_UFO,
                           starts = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
                           engine = "C")},
                     error = function(cond){"skip"})
  
  newdat1 = data.frame(cov1 = medianlla,
                       cov2 = factor(c("Sum", "Mon", "Aut", "Win")))
  newdat2 = data.frame(prop_nb = occdata_abs$prop_nb)
  
} else {
  
  occ_det = tryCatch({occu(~ log(cov1) ~ prop_nb, 
                           data = occdata_UFO, 
                           starts = c(0, 0, 0, 0),
                           engine = "C")},
                     error = function(cond){"skip"})
  
  newdat1 = data.frame(cov1 = medianlla)
  newdat2 = data.frame(prop_nb = occdata_abs$prop_nb)
  
}

# output of above will be character when errored ("skip")
if (!is.character(occ_det)) {
  
  # detection probability
  occpred_detection = unmarked::predict(occ_det, newdata = newdat1, type = "det")
  occpred_detection = mean(occpred_detection$Predicted) 
  
  # occupancy
  occpred_occupancy = unmarked::predict(occ_det, newdata = newdat2, type = "state")
  occpred_occupancy$prop_nb = newdat2$prop_nb
  occpred_occupancy$gridg1 = occdata_abs$gridg1
  
  occpred_occupancy <- occpred_occupancy %>% 
    filter(!is.na(Predicted)) %>% 
    mutate(detprob = occpred_detection,
           status = status,
           COMMON.NAME = species)
  names(occpred_occupancy)[1:2] = c("occupancy","se")
  occpred_occupancy = occpred_occupancy %>% dplyr::select(-lower, -upper)

}

# to combine
return(occpred_occupancy)
