# preparing data for specific mask (this is the only part that changes, but automatically)
cur_metadata <- analyses_metadata %>% filter(MASK == cur_mask)
speclist_path <- cur_metadata$SPECLISTDATA.PATH

# for the full country analysis, runs are split among multiple systems, and use
# separate subsampled datasets. We need to ensure this information exists.
if (!exists("my_assignment")) {
  
  return("'my_assignment' is empty! Please specify IDs of data files assigned to you.")
  
} else {
  
  cur_assignment <- my_assignment
  
}

###

require(tidyverse)
require(lme4)
require(VGAM)
require(parallel)
require(foreach)
require(doParallel)

source('00_scripts/00_functions.R')


load(speclist_path)

lsa = specieslist %>% filter(!is.na(ht) | !is.na(rt))
listofspecies = c(lsa$COMMON.NAME, restrictedspecieslist$COMMON.NAME)
speclen = length(listofspecies)

# creating new directory if it doesn't already exist
if (!dir.exists(cur_metadata$TRENDS.PATHONLY)) {
  dir.create(cur_metadata$TRENDS.PATHONLY, 
             recursive = T)
}

for (k in cur_assignment)
{
  
  # file names for individual files
  write_path <- cur_metadata %>% 
    dplyr::summarise(TRENDS.PATH = glue("{TRENDS.PATHONLY}trends_{k}.csv")) %>% 
    as.character()
  
  data_path = cur_metadata %>% 
    dplyr::summarise(SIMDATA.PATH = glue("{SIMDATA.PATHONLY}data{k}.csv")) %>% 
    as.character()

  
  tictoc::tic(glue("Species trends for full country: {k}/{max(cur_assignment)}"))
  
  # read data files
  data = read.csv(data_path) %>% 
    mutate(across(.cols = c(gridg1, gridg2, gridg3, gridg4, month, timegroups),
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

  trends0 = foreach(i = listofspecies, .combine = 'cbind', .errorhandling = 'remove') %dopar%
    singlespeciesrun(data = data, 
                     species = i, 
                     specieslist = specieslist, 
                     restrictedspecieslist = restrictedspecieslist)
  
  trends = data.frame(trends0)
  
  # <annotation_pending_AV>
  # also maybe rename objects to be more informative?
  len = length(as.vector(trends[,1]))
  n = len/29
  spnames = as.vector(trends[1,])
  sq = -seq(1, len, 29)
  trends = trends[sq,]
  names(trends) = spnames
  a = rep(c("freq", "se"), each = 14)
  a1 = rep(a, n)
  tg = c("before 2000","2000-2006","2007-2010",
         "2011-2012","2013","2014","2015","2016",
         "2017","2018","2019","2020","2021","2022")
  #b1 = rep(1:n,each=28)
  databins1 = rep(databins$year, n * 2)
  tg1 = rep(tg, n * 2)
  
  trends$timegroups = databins1
  trends$timegroupsf = tg1
  trends$type = a1
  trends$sl = k
  
  trends = trends %>% 
    pivot_longer(-c(timegroups, timegroupsf, sl, type), 
                        values_to = "value", names_to = "COMMON.NAME") %>% 
    pivot_wider(names_from = type, values_from = value)
  
  speclen = length(unique(trends$COMMON.NAME))
  trends$sp = rep(1:speclen, 14 * n)
  
  trends = trends %>%
    arrange(sl, sp) %>%
    select(sl, COMMON.NAME, timegroupsf, timegroups, freq, se, 
           -sp)
  
  write.csv(trends, file = write_path, row.names = F)

  tictoc::toc() 

  gc()
  
}






 



