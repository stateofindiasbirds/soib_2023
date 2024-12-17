cur_mask <- "none"

my_assignment <- 1


cur_metadata <- get_metadata(cur_mask)
speclist_path <- cur_metadata$SPECLISTDATA.PATH
databins_path <- cur_metadata$DATA.PATH # for databins

# don't run if no species selected
load(speclist_path)
to_run <- (1 %in% specieslist$ht) | (1 %in% specieslist$rt) |
  (1 %in% restrictedspecieslist$ht) | (1 %in% restrictedspecieslist$rt)


# singleyear = interannualupdate
singleyear = FALSE
# not using single year modelling approach, since test runs showed that
# single year models produce notably higher estimates than full-year models



require(tidyverse)
require(lme4)
require(VGAM)
require(foreach)


source('00_scripts/00_functions.R')


load(speclist_path)
load(databins_path)
rm(data)

lsa = specieslist %>% filter(!is.na(ht) | !is.na(rt))
listofspecies = c(lsa$COMMON.NAME, restrictedspecieslist$COMMON.NAME)
speclen = length(listofspecies)


k <- my_assignment
  
  # file names for individual files
  write_path <- cur_metadata %>% 
    dplyr::summarise(TRENDS.PATH = glue("{TRENDS.PATHONLY}trends_{k}.csv")) %>% 
    as.character()
  
  data_path = cur_metadata %>% 
    dplyr::summarise(SIMDATA.PATH = glue("{SIMDATA.PATHONLY}data{k}.RData")) %>% 
    as.character()
  
  

  # read data files
  load(data_path)
  
  cols_temp <- if (singleyear == FALSE) {
    c("gridg1", "gridg2", "gridg3", "gridg4", "month", "timegroups")
  } else if (singleyear == TRUE) {
    c("gridg1", "gridg2", "gridg3", "gridg4", "month")
  }
  
  data <- data_filt %>% 
    mutate(across(.cols = all_of(cols_temp), ~ as.factor(.))) %>% 
    mutate(gridg = gridg3)
  
  rm(cols_temp)
  
  
  i <- listofspecies[2]
  species = i

  
  require(tidyverse)
  require(lme4)
  require(merTools)
  require(glue)
  
  data1 = data
  
  # get information for the species of interest 
  specieslist2 = specieslist %>% filter(COMMON.NAME == species)
  
  # three different flags for three different model types that will be run.
  # 0 is normal model, with full random effects. depending on restricted species,
  # model changes slightly.
  flag = 0
  if (species %in% restrictedspecieslist$COMMON.NAME)
  {
    flag = 1
    restrictedlist1 = restrictedspecieslist %>% filter(COMMON.NAME == species)
    specieslist2$ht = restrictedlist1$ht
    specieslist2$rt = restrictedlist1$rt
    
    if (restrictedlist1$mixed == 0) {
      flag = 2
    }
  }
  
  # filters data based on whether the species has been selected for long-term trends (ht) 
  # or short-term trends (rt) 
  # (if only recent, then need to filter for recent years. else, use all years so no filter.)
  
  if (singleyear == FALSE) {
    
    if (is.na(specieslist2$ht) & !is.na(specieslist2$rt)) {
      data1 = data1 %>% filter(year >= soib_year_info("cat_start"))
    }
    
  } else if (singleyear == TRUE) {
    
    data1 = data1 %>% filter(year == soib_year_info("latest_year"))
  }
  
  
  data1 = data1 %>%
    filter(COMMON.NAME == species) %>%
    distinct(gridg3, month) %>% 
    left_join(data1)
  
  tm = data1 %>% distinct(timegroups)
  #rm(data, pos = ".GlobalEnv")
  
  datay = data1 %>%
    distinct(gridg3, gridg1, group.id, .keep_all = TRUE) %>% 
    group_by(gridg3, gridg1) %>% 
    reframe(medianlla = median(no.sp)) %>%
    group_by(gridg3) %>% 
    reframe(medianlla = mean(medianlla)) %>%
    reframe(medianlla = round(mean(medianlla)))
  
  medianlla = datay$medianlla
  
  
  # expand dataframe to include absences as well
  ed = expand_dt(data1, species) %>% 
    # converting months to seasons
    mutate(month = as.numeric(month)) %>% 
    mutate(month = case_when(month %in% c(12,1,2) ~ "Win",
                             month %in% c(3,4,5) ~ "Sum",
                             month %in% c(6,7,8) ~ "Mon",
                             month %in% c(9,10,11) ~ "Aut")) %>% 
    mutate(month = as.factor(month))
  
  
  # the model ---------------------------------------------------------------
  
  fixed_effects <- "OBSERVATION.COUNT ~ month + month:log(no.sp)"
  include_timegroups <- if (singleyear == FALSE) "+ timegroups" else 
    if (singleyear == TRUE) ""
  random_effects <- if (flag == 0) "+ (1|gridg3/gridg1)" else 
    if (flag == 1) "+ (1|gridg1)" else 
      if (flag == 2) ""
  
  model_formula <- as.formula(glue("{fixed_effects} {include_timegroups} {random_effects}"))
  
  m1 <- if (flag != 2) {
    glmer(model_formula, 
          data = ed, family = binomial(link = 'cloglog'), 
          nAGQ = 0, control = glmerControl(optimizer = "bobyqa"))
  } else {
    glm(model_formula, 
        data = ed, family = binomial(link = 'cloglog'))
  }
  
  
  # predicting from model ---------------------------------------------------
  
  # prepare a new data file to predict
  
  ltemp <- ed %>% 
    {if (singleyear == FALSE) {
      group_by(., month) %>% 
        reframe(., timegroups = unique(tm$timegroups))
    } else if (singleyear == TRUE) {
      distinct(., month)
    }} %>% 
    mutate(no.sp = medianlla,
           # taking the first value but any random value will do because we do not
           # intend to predict random variation across grids
           gridg1 = data1$gridg1[1], 
           gridg3 = data1$gridg3[1])
  
  f2 <- ltemp %>% 
    {if (singleyear == FALSE) {
      dplyr::select(., timegroups)
    } else if (singleyear == TRUE) {
      .
    }} %>% 
    mutate(freq = 0, se = 0) %>%  # this is not actually needed
    {if (singleyear == FALSE) {
      .
    } else if (singleyear == TRUE) {
      dplyr::select(., freq, se)
    }}
  
  
  tictoc::tic(
    "predictInterval()"
  )
  if (flag != 2)
  {
    #pred = predict(m1, newdata = ltemp, type = "response", re.form = NA, allow.new.levels=TRUE)
    pred = predictInterval(m1, newdata = ltemp, which = "fixed", stat = "mean",
                           level = 0.48, type = "linear.prediction")
    f2$freqt = pred$fit
    f2$set = pred$fit-pred$lwr
  }
  tictoc::toc()
  

  ### bootstrapping confidence from GLMMs (bootMer) 

  pred_fun <- function(model) {
    predict(model, newdata = ltemp, re.form = NA, allow.new.levels = TRUE)
    # not specifying type = "response" because will later transform prediction along with SE
  }
  
  tictoc::tic("bootMer 10 sims")
  par_cores <- max(1, floor(detectCores()/2))
  par_cluster <- makeCluster(rep("localhost", par_cores))
  clusterEvalQ(par_cluster, library(lme4))
  clusterExport(par_cluster, varlist = "ltemp")
  
  pred_bootMer <- bootMer(m1, nsim = 10, FUN = pred_fun, 
                          seed = 1000, use.u = FALSE, type = "parametric", 
                          parallel = "snow", ncpus = par_cores, cl = par_cluster)
  
  
  stopCluster(par_cluster)
  
  tictoc::toc()
  
  
  tictoc::tic("bootMer 10 sims no par")
  pred_bootMer <- bootMer(m1, nsim = 10, FUN = pred_fun, 
                          seed = 1000, use.u = FALSE, type = "parametric", 
                          parallel = "no", ncpus = par_cores, cl = par_cluster)
  
  tictoc::toc()
  

`  median(na.omit(pred_bootMer$t[,1]))
  sd(na.omit(pred_bootMer$t[,1]))
````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````  
  
  
  test_arm = sim(m1, 10)
  
  
  
  f1 = f2 %>%
    filter(!is.na(freqt) & !is.na(set)) %>%
    # average across month
    {if (singleyear == FALSE) {
      group_by(., timegroups) %>% 
        reframe(freq = mean(freqt), se = mean(set)) %>% 
        right_join(tm) %>% 
        left_join(databins %>% distinct(timegroups, year)) %>% 
        rename(timegroupsf = timegroups,
               timegroups = year) %>% 
        mutate(timegroupsf = factor(timegroupsf, 
                                    levels = soib_year_info("timegroup_lab"))) %>% 
        complete(timegroupsf) %>% 
        arrange(timegroupsf)
    } else if (singleyear == TRUE) {
      reframe(., freq = mean(freqt), se = mean(set))
    }}
  
  
  
  tocomb = c(species, f1$freq, f1$se)
  return(tocomb)
  # each species's tocomb becomes one column in final trends0 output object
  
  
  
  
  
  
  
  
  
  
  
  trends = data.frame(trends0) %>% 
    # converting first row of species names (always true) to column names
    magrittr::set_colnames(.[1,]) %>% 
    slice(-1) %>% 
    {if (singleyear == FALSE) {
      
      mutate(.,
             timegroupsf = rep(databins$timegroups, 2),
             timegroups = rep(databins$year, 2),
             type = rep(c("freq", "se"), 
                        # will always have 2*N.YEAR rows (freq, se)
                        each = length(soib_year_info("timegroup_lab"))),
             sl = k) %>%  # sim number
        # pivoting species names longer
        pivot_longer(-c(timegroups, timegroupsf, sl, type), 
                     values_to = "value", names_to = "COMMON.NAME") %>% 
        pivot_wider(names_from = type, values_from = value) %>% 
        # numerical ID for species names, for arranging
        mutate(sp = row_number(), .by = timegroupsf) %>%
        arrange(sl, sp) %>%
        dplyr::select(-sp) %>% 
        # reordering
        relocate(sl, COMMON.NAME, timegroupsf, timegroups, freq, se)
      
    } else if (singleyear == TRUE) {
      
      mutate(.,
             type = rep(c("freq", "se"), 
                        each = 1),
             sl = k) %>%  # sim number
        # pivoting species names longer
        pivot_longer(-c(sl, type), 
                     values_to = "value", names_to = "COMMON.NAME") %>% 
        pivot_wider(names_from = type, values_from = value) %>% 
        # numerical ID for species names, for arranging
        mutate(sp = row_number()) %>%
        arrange(sl, sp) %>%
        dplyr::select(-sp) %>% 
        # reordering
        relocate(sl, COMMON.NAME, freq, se) |> 
        # bringing back timegroups columns
        mutate(timegroups = soib_year_info("latest_year"),
               timegroupsf = as.character(soib_year_info("latest_year")))
      
    }} %>% 
    # make sure freq and se are numerical
    mutate(across(c("freq", "se"), ~ as.numeric(.)))
  
  
  # if full run, overwrite the CSV
  # else append single year results to all previous year results
  if (singleyear == FALSE) {
    
    write.csv(trends, file = write_path, row.names = FALSE)
    
  } else if (singleyear == TRUE) {
    
    trends_old <- read.csv(write_path, header = TRUE) |> 
      filter(timegroups != soib_year_info("latest_year"))
    
    trends_new <- trends_old |> 
      bind_rows(trends) |> 
      left_join(specieslist |> 
                  mutate(order = row_number(),
                         ht = NULL,
                         rt = NULL), 
                by = "COMMON.NAME") |> 
      arrange(sl, order, timegroups) |> 
      dplyr::select(-order)
    
    write.csv(trends_new, file = write_path, row.names = FALSE)
    
  }
  
  
