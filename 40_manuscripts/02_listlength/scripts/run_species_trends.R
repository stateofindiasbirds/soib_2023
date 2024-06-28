require(tidyverse)
require(lme4)
require(VGAM)
require(parallel)
require(foreach)
require(doParallel)
require(glue)

source('00_scripts/00_functions.R')
source('40_manuscripts/02_listlength/scripts/functions.R')

load("40_manuscripts/02_listlength/KL_Atlas_data.RData")
load("40_manuscripts/02_listlength/KL_data.RData")

specieslist_data = KL_data %>%
  filter(CATEGORY %in% c("issf","species")) %>%
  distinct(COMMON.NAME) %>% pull(COMMON.NAME)

Atlas_freqs = KL_Atlas_data %>%
  mutate(lists = n_distinct(group.id)) %>%
  filter(COMMON.NAME %in% specieslist_data) %>%
  group_by(COMMON.NAME) %>% reframe(freq = n_distinct(group.id)/max(lists)) %>%
  arrange(desc(freq))

# restrict analyses to species in at least 1% of lists
specieslist = Atlas_freqs %>%
  filter(freq > 0.01) %>% pull(COMMON.NAME)


for (k in 1:1000)
{
  read_path = paste("40_manuscripts/02_listlength/dataforsim/overall/data",k,".csv",sep="")
  read_path_du = paste("40_manuscripts/02_listlength/dataforsim/duration/data",k,".csv",sep="")
  read_path_di = paste("40_manuscripts/02_listlength/dataforsim/distance/data",k,".csv",sep="")
  write_path = paste("40_manuscripts/02_listlength/results/results_",k,".csv",sep="")
  
  tictoc::tic(glue("Model runs for iteration {k}"))
  
  data = read.csv(read_path) %>% 
    mutate(across(.cols = c(gridg1, gridg2, gridg3, gridg4),
                  ~ as.factor(.))) %>% 
    mutate(gridg = gridg3)
  data_du = read.csv(read_path_du) %>% 
    mutate(across(.cols = c(gridg1, gridg2, gridg3, gridg4),
                  ~ as.factor(.))) %>% 
    mutate(gridg = gridg3)
  data_di = read.csv(read_path_di) %>% 
    mutate(across(.cols = c(gridg1, gridg2, gridg3, gridg4),
                  ~ as.factor(.))) %>% 
    mutate(gridg = gridg3)
  
  # start parallel
  n.cores = parallel::detectCores()/2
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
  
  results0 = foreach(i = specieslist, .combine = 'cbind', .errorhandling = 'remove') %dopar%
    singlespeciesrun_KL(data = data, data_du = data_du, data_di = data_di, 
                        species = i)
  
  parallel::stopCluster(cl = my.cluster)
  
  results = data.frame(results0) %>% 
    # converting first row of species names (always true) to column names
    magrittr::set_colnames(.[1,]) %>% 
    slice(-1) %>% 
    # will always have 28 rows
    mutate(sl = k,
           model = rep(c("m1","m2","m3","m4","m5","m6","m7","m8"),8),
           type = rep(c("freq","se","ll","ll.se","du","du.se","di","di.se"), 
                      each = 8)) %>%  # sim number
    # pivoting species names longer
    pivot_longer(-c(model, sl, type), 
                 values_to = "value", names_to = "COMMON.NAME") %>% 
    pivot_wider(names_from = type, values_from = value)
  
  write.csv(results, file = write_path, row.names = F)
  
  tictoc::toc() 
  
  gc()
}
