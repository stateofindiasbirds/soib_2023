# creating new directory if it doesn't already exist
if (!dir.exists(cur_metadata$OCCU.MOD.PATHONLY)) {
  dir.create(cur_metadata$OCCU.MOD.PATHONLY, 
             recursive = T)
}

###

library(parallel)
require(foreach)
require(doParallel)

load("00_data/grids_sf_nb.RData")
our_neighbours <- g1_nb_q
rm(g1_nb_r, g2_nb_q, g2_nb_r, g3_nb_q, g3_nb_r, g4_nb_q, g4_nb_r)

data = data %>%
  mutate(OBSERVATION.COUNT = replace(OBSERVATION.COUNT, !is.na(OBSERVATION.COUNT), "1")) %>% 
  mutate(OBSERVATION.COUNT = as.numeric(OBSERVATION.COUNT))


# calculation -------------------------------------------------------------

chunks = split(vec, cut(seq_along(vec), 80, labels = FALSE))

for (k in 1:80)
{
  
  # file names for individual files
  write_path <- cur_metadata %>% 
    dplyr::summarise(OCCU.MOD.PATH = glue("{OCCU.MOD.PATHONLY}chunk_{k}.csv")) %>% 
    pull(OCCU.MOD.PATH)

  # start parallel
  n.cores = parallel::detectCores()/2 - 2
  # create the cluster
  my.cluster = parallel::makeCluster(
    n.cores, 
    type = "PSOCK"
  )
  # register it to be used by %dopar%
  doParallel::registerDoParallel(cl = my.cluster)
  
  # # check if it is registered (optional)
  # foreach::getDoParRegistered()
  # # how many workers are available? (optional)
  # foreach::getDoParWorkers()
  
  start = Sys.time()
  occu0 = foreach(i = chunks[[k]], .combine = 'rbind', .errorhandling = 'remove') %dopar%
    occupancyrun(data = data, 
                 i = i,
                 speciesforocc = speciesforocc,
                 nb8g1 = nb8g1)
  end = Sys.time()
  print(end-start)
  
  parallel::stopCluster(cl = my.cluster)
  
  print(k)
  write.csv(occu0, file = write_path, row.names = F)
  
  gc()
  
}
