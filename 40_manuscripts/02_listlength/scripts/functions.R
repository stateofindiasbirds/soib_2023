expandbyspecies_KL = function(data, species)
{
  require(tidyverse)
  
  data <- data %>% 
    mutate(across(contains("gridg"), ~ as.factor(.)))
  
  # considers only complete lists
  
  checklistinfo = data %>%
    distinct(gridg1, gridg2, gridg3, gridg4, 
             ALL.SPECIES.REPORTED,
             #city,
             DURATION.MINUTES,EFFORT.DISTANCE.KM,
             group.id, no.sp) %>%
    filter(ALL.SPECIES.REPORTED == 1) %>%
    group_by(group.id) %>% 
    slice(1) %>% 
    ungroup()
  
  # expand data frame to include the bird species in every list
  expanded = checklistinfo %>% 
    mutate(COMMON.NAME = species) %>% 
    left_join(data) %>%
    dplyr::select(-c("COMMON.NAME","gridg2","gridg4",
                     "ALL.SPECIES.REPORTED","group.id",
                     "LOCALITY.ID","ST_NM","DISTRICT","PROTOCOL.TYPE",
                     "year","month")) %>% 
    # deal with NAs (column is character)
    mutate(OBSERVATION.COUNT = case_when(is.na(OBSERVATION.COUNT) ~ 0,
                                         OBSERVATION.COUNT != "0" ~ 1, 
                                         TRUE ~ as.numeric(OBSERVATION.COUNT)))
  
  return(expanded)
}

singlespeciesrun_KL = function(data, data_du, data_di, species)
{
  require(tidyverse)
  require(merTools)
  
  data1 = data
  data_du1 = data_du
  data_di1 = data_di
  
  data1 = data1 %>%
    filter(COMMON.NAME == species) %>%
    distinct(gridg3, month) %>% 
    left_join(data1)
  
  data_du1 = data_du1 %>%
    filter(COMMON.NAME == species) %>%
    distinct(gridg3, month) %>% 
    left_join(data_du1)
  
  data_di1 = data_di1 %>%
    filter(COMMON.NAME == species) %>%
    distinct(gridg3, month) %>% 
    left_join(data_di1) %>%
    mutate(EFFORT.DISTANCE.KM = 1000*EFFORT.DISTANCE.KM) %>%
    filter(EFFORT.DISTANCE.KM != 0)
  
  data.ll = data1 %>%
    group_by(gridg3, gridg1, group.id) %>% 
    slice(1) %>% 
    group_by(gridg3, gridg1) %>% 
    reframe(effort = median(no.sp)) %>%
    group_by(gridg3) %>% 
    reframe(effort = mean(effort)) %>%
    reframe(effort = round(mean(effort)))
  
  median.ll = data.ll$effort
  
  data.du = data_du1 %>%
    filter(!is.na(DURATION.MINUTES)) %>%
    group_by(gridg3, gridg1, group.id) %>% 
    slice(1) %>% 
    group_by(gridg3, gridg1) %>% 
    reframe(effort = median(DURATION.MINUTES)) %>%
    group_by(gridg3) %>% 
    reframe(effort = mean(effort)) %>%
    reframe(effort = round(mean(effort)))
  
  median.du = data.du$effort
  
  data.di = data_di1 %>%
    filter(!is.na(EFFORT.DISTANCE.KM)) %>%
    group_by(gridg3, gridg1, group.id) %>% 
    slice(1) %>% 
    group_by(gridg3, gridg1) %>% 
    reframe(effort = median(EFFORT.DISTANCE.KM)) %>%
    group_by(gridg3) %>% 
    reframe(effort = mean(effort)) %>%
    reframe(effort = round(mean(effort)))
  
  median.di = data.di$effort
  
  
  # expand dataframe to include absences as well
  ed = expandbyspecies_KL(data1, species)
  ed_du = expandbyspecies_KL(data_du1, species)
  ed_di = expandbyspecies_KL(data_di1, species)
  
  

  m1 = glm(OBSERVATION.COUNT ~ log(no.sp), 
           data = ed, family = binomial(link = 'cloglog'))
  
  m2 = glmer(OBSERVATION.COUNT ~ (1|gridg3/gridg1), 
             data = ed, family = binomial(link = 'cloglog'), 
             nAGQ = 0, control = glmerControl(optimizer = "bobyqa"))
  
  m3 = glmer(OBSERVATION.COUNT ~ log(no.sp) + (1|gridg3/gridg1), 
             data = ed, family = binomial(link = 'logit'), 
             nAGQ = 0, control = glmerControl(optimizer = "bobyqa"))
  
  m4 = glmer(OBSERVATION.COUNT ~ log(no.sp) + (1|gridg3/gridg1), 
             data = ed, family = binomial(link = 'cloglog'), 
             nAGQ = 0, control = glmerControl(optimizer = "bobyqa"))
  
  m5 = glmer(OBSERVATION.COUNT ~ log(DURATION.MINUTES) + (1|gridg3/gridg1), 
             data = ed_du, family = binomial(link = 'cloglog'), 
             nAGQ = 0, control = glmerControl(optimizer = "bobyqa"))
  
  m6 = glmer(OBSERVATION.COUNT ~ log(EFFORT.DISTANCE.KM) + (1|gridg3/gridg1), 
             data = ed_di, family = binomial(link = 'cloglog'), 
             nAGQ = 0, control = glmerControl(optimizer = "bobyqa"))
  
  m7 = glmer(OBSERVATION.COUNT ~ log(no.sp) + log(DURATION.MINUTES) + 
               (1|gridg3/gridg1), 
             data = ed_du, family = binomial(link = 'cloglog'), 
             nAGQ = 0, control = glmerControl(optimizer = "bobyqa"))
  
  m8 = glmer(OBSERVATION.COUNT ~ log(no.sp) + log(DURATION.MINUTES) + 
               log(EFFORT.DISTANCE.KM) + (1|gridg3/gridg1), 
             data = ed_di, family = binomial(link = 'cloglog'), 
             nAGQ = 0, control = glmerControl(optimizer = "bobyqa"))
  


  
  
  # predicting from model ---------------------------------------------------
  
  ltemp <- data.frame(no.sp = median.ll,
                        DURATION.MINUTES = median.du,
                        EFFORT.DISTANCE.KM = median.di,
                        gridg1 = data1$gridg1[1], 
                        gridg3 = data1$gridg3[1],
                        ll = NA, ll.se = NA,
                        du = NA, du.se = NA,
                        di = NA, di.se = NA)
    

  
  f2.m1 = ltemp
  pred = predict(m1, newdata = ltemp, type = "link", se.fit = T)
  f2.m1$freq = pred$fit
  f2.m1$se = pred$se.fit
  f2.m1$model = "m1"
  f2.m1$ll = summary(m1)$coefficients[2]
  f2.m1$ll.se = summary(m1)$coefficients[4]
  
  f2.m2 = ltemp
  pred = predictInterval(m2, newdata = ltemp, which = "fixed",
                         level = 0.48, type = "linear.prediction")
  f2.m2$freq = pred$fit
  f2.m2$se = pred$fit-pred$lwr
  f2.m2$model = "m2"
  
  f2.m3 = ltemp
  pred = predictInterval(m3, newdata = ltemp, which = "fixed",
                         level = 0.48, type = "linear.prediction")
  f2.m3$freq = pred$fit
  f2.m3$se = pred$fit-pred$lwr
  f2.m3$model = "m3"
  f2.m3$ll = summary(m3)$coefficients[2]
  f2.m3$ll.se = summary(m3)$coefficients[4]
  
  f2.m4 = ltemp
  pred = predictInterval(m4, newdata = ltemp, which = "fixed",
                         level = 0.48, type = "linear.prediction")
  f2.m4$freq = pred$fit
  f2.m4$se = pred$fit-pred$lwr
  f2.m4$model = "m4"
  f2.m4$ll = summary(m4)$coefficients[2]
  f2.m4$ll.se = summary(m4)$coefficients[4]
  
  f2.m5 = ltemp
  pred = predictInterval(m5, newdata = ltemp, which = "fixed",
                         level = 0.48, type = "linear.prediction")
  f2.m5$freq = pred$fit
  f2.m5$se = pred$fit-pred$lwr
  f2.m5$model = "m5"
  f2.m5$du = summary(m5)$coefficients[2]
  f2.m5$du.se = summary(m5)$coefficients[4]
  
  f2.m6 = ltemp
  pred = predictInterval(m6, newdata = ltemp, which = "fixed",
                         level = 0.48, type = "linear.prediction")
  f2.m6$freq = pred$fit
  f2.m6$se = pred$fit-pred$lwr
  f2.m6$model = "m6"
  f2.m6$di = summary(m6)$coefficients[2]
  f2.m6$di.se = summary(m6)$coefficients[4]
  
  f2.m7 = ltemp
  pred = predictInterval(m7, newdata = ltemp, which = "fixed",
                         level = 0.48, type = "linear.prediction")
  f2.m7$freq = pred$fit
  f2.m7$se = pred$fit-pred$lwr
  f2.m7$model = "m7"
  f2.m7$ll = summary(m7)$coefficients[2]
  f2.m7$ll.se = summary(m7)$coefficients[5]
  f2.m7$du = summary(m7)$coefficients[3]
  f2.m7$du.se = summary(m7)$coefficients[6]
  
  f2.m8 = ltemp
  pred = predictInterval(m8, newdata = ltemp, which = "fixed",
                         level = 0.48, type = "linear.prediction")
  f2.m8$freq = pred$fit
  f2.m8$se = pred$fit-pred$lwr
  f2.m8$model = "m8"
  f2.m8$ll = summary(m8)$coefficients[2]
  f2.m8$ll.se = summary(m8)$coefficients[6]
  f2.m8$du = summary(m8)$coefficients[3]
  f2.m8$du.se = summary(m8)$coefficients[7]
  f2.m8$di = summary(m8)$coefficients[4]
  f2.m8$di.se = summary(m8)$coefficients[8]
  
  f1 = f2.m1 %>% bind_rows(f2.m2) %>% bind_rows(f2.m3) %>% bind_rows(f2.m4) %>%
    bind_rows(f2.m5) %>% bind_rows(f2.m6) %>% bind_rows(f2.m7) %>% bind_rows(f2.m8)
  
  
  tocomb = c(species, f1$freq, f1$se, f1$ll, f1$ll.se, f1$du, f1$du.se,
             f1$di, f1$di.se)
  return(tocomb)
  # each species's tocomb becomes one column in final trends0 output object
  
}