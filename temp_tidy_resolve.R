

modtrends_recent = modtrends_recent %>% 
  # calculating CIs
  group_by(COMMON.NAME, timegroups) %>% 
  # 1000 simulations of transformed ratio of present:original values
  # quantiles*100 from these gives us our CI limits for mean_std
  reframe(simerrordiv(mean_trans, m1, se_trans, s1)) %>%  # gives rat and val columns
  group_by(COMMON.NAME, timegroups) %>% 
  reframe(lci_std_recent = 100*as.numeric(quantile(rat, 0.025)),
          rci_std_recent = 100*as.numeric(quantile(rat, 0.975)),
          rat = rat,
          val = val) %>% 
  right_join(modtrends_recent, by = c("COMMON.NAME", "timegroups"))


set.seed(1)

temp_sims <- modtrends_recent %>%
  group_by(COMMON.NAME) %>%
  dplyr::select(timegroups, rat, val) %>%
  # filter(COMMON.NAME %in% c("Alexandrine Parakeet", "Zitting Cisticola")) %>% ###
  filter(COMMON.NAME %in% c("Alexandrine Parakeet", "Alpine Swift", "Ashy Bulbul",
                            "Ashy Drongo","Ashy Prinia")) %>% ###
  group_by(COMMON.NAME, timegroups) %>%
  mutate(sim = 1:n(),
         # for each sim, sampling from 1000 val values within species-year group
         val_sample = map_dbl(sim, ~ sample(val, 1)))



# Take simulated values of reporting frequency for each year,
# fit lm through them, then predict new values based on that lm.

# Numerator and denominator for slope calculation, along with error propagation
# Uncertainty between 2015 and 2016 is highest, hence indexing them, i.e.,
# using that uncertainty for entire slope (being cautious).

# errordiv calculates and returns slope and SE from predicted values.


# main recent trend ###

sl_data_main <- temp_sims %>% 
  group_by(COMMON.NAME, sim) %>% 
  # group_modify() performs function per grouping, so sl and slse calculated for all sim
  group_modify(~ {
    
    datatopred <- .x %>% dplyr::select(timegroups)
    
    modelfit <- lm(val_sample ~ timegroups, data = .x)
    pred <- predict(modelfit, newdata = datatopred, se = TRUE)
    
    num <- pred$fit[2] - pred$fit[1]
    den <- abs(pred$fit[1])
    numse <- sqrt(pred$se.fit[1]^2 + pred$se.fit[2]^2)
    dense <- pred$se.fit[1]
    
    # indexing for mean and se
    sl <- 100 * errordiv(num, den, numse, dense)[1] %>% as.numeric()
    slse <- errordiv(num, den, numse, dense)[2] %>% as.numeric()
    
    .x %>%
      reframe(sl = sl, 
              slse = slse)
    
  }) %>%
  group_by(COMMON.NAME) %>%
  reframe(mean.slope = mean(sl),
          se.slope = sd(sl) + sqrt(sum(slse^2)/length(slse))) %>%
  group_by(COMMON.NAME) %>%
  reframe(currentslopelci = mean.slope - 1.96*se.slope,
          currentslopemean = mean.slope,
          currentsloperci = mean.slope + 1.96*se.slope)

# joining to main data
main <- main %>%
  left_join(sl_data_main, by = c("eBird.English.Name.2022" = "COMMON.NAME"))





# sensitivity

# here, fitting the same linear model as for main trend, but for data in which
# one year is dropped each time.
# this is to see how much each year affects the estimate.
tic()
sl_data_sens <- temp_sims %>%
  group_by(COMMON.NAME, sim) %>%
  group_modify(~ {

    modelfit <- lm(val_sample ~ timegroups, 
                   # one year dropped
                   data = .x[.x$timegroups != 2015,])
    
    pred <- predict(modelfit, se = TRUE,
                    # one year dropped
                    newdata = data.frame(timegroups = .x$timegroups[.x$timegroups != 2015]))
    
    num <- pred$fit[2] - pred$fit[1]
    den <- abs(pred$fit[1])
    numse <- sqrt(pred$se.fit[1]^2 + pred$se.fit[2]^2)
    dense <- pred$se.fit[1]
    
    sl1 = 100 * errordiv(num, den, numse, dense)[1] %>% as.numeric()
    slse1 = errordiv(num, den, numse, dense)[2] %>% as.numeric()

    
    modelfit <- lm(val_sample ~ timegroups, 
                   # one year dropped
                   data = .x[.x$timegroups != 2016,])
    
    pred <- predict(modelfit, se = TRUE,
                    # one year dropped
                    newdata = data.frame(timegroups = .x$timegroups[.x$timegroups != 2016]))
    
    num <- pred$fit[2] - pred$fit[1]
    den <- abs(pred$fit[1])
    numse <- sqrt(pred$se.fit[1]^2 + pred$se.fit[2]^2)
    dense <- pred$se.fit[1]
    
    sl2 = 100 * errordiv(num, den, numse, dense)[1] %>% as.numeric()
    slse2 = errordiv(num, den, numse, dense)[2] %>% as.numeric()
    
    
    modelfit <- lm(val_sample ~ timegroups, 
                   # one year dropped
                   data = .x[.x$timegroups != 2017,])
    
    pred <- predict(modelfit, se = TRUE,
                    # one year dropped
                    newdata = data.frame(timegroups = .x$timegroups[.x$timegroups != 2017]))
    
    num <- pred$fit[2] - pred$fit[1]
    den <- abs(pred$fit[1])
    numse <- sqrt(pred$se.fit[1]^2 + pred$se.fit[2]^2)
    dense <- pred$se.fit[1]
    
    sl3 = 100 * errordiv(num, den, numse, dense)[1] %>% as.numeric()
    slse3 = errordiv(num, den, numse, dense)[2] %>% as.numeric()
    
 
    modelfit <- lm(val_sample ~ timegroups, 
                   # one year dropped
                   data = .x[.x$timegroups != 2018,])
    
    pred <- predict(modelfit, se = TRUE,
                    # one year dropped
                    newdata = data.frame(timegroups = .x$timegroups[.x$timegroups != 2018]))
    
    num <- pred$fit[2] - pred$fit[1]
    den <- abs(pred$fit[1])
    numse <- sqrt(pred$se.fit[1]^2 + pred$se.fit[2]^2)
    dense <- pred$se.fit[1]
    
    sl4 = 100 * errordiv(num, den, numse, dense)[1] %>% as.numeric()
    slse4 = errordiv(num, den, numse, dense)[2] %>% as.numeric()
    
    
    modelfit <- lm(val_sample ~ timegroups, 
                   # one year dropped
                   data = .x[.x$timegroups != 2019,])
    
    pred <- predict(modelfit, se = TRUE,
                    # one year dropped
                    newdata = data.frame(timegroups = .x$timegroups[.x$timegroups != 2019]))
    
    num <- pred$fit[2] - pred$fit[1]
    den <- abs(pred$fit[1])
    numse <- sqrt(pred$se.fit[1]^2 + pred$se.fit[2]^2)
    dense <- pred$se.fit[1]
    
    sl5 = 100 * errordiv(num, den, numse, dense)[1] %>% as.numeric()
    slse5 = errordiv(num, den, numse, dense)[2] %>% as.numeric()
    
    
    modelfit <- lm(val_sample ~ timegroups, 
                   # one year dropped
                   data = .x[.x$timegroups != 2020,])
    
    pred <- predict(modelfit, se = TRUE,
                    # one year dropped
                    newdata = data.frame(timegroups = .x$timegroups[.x$timegroups != 2020]))
    
    num <- pred$fit[2] - pred$fit[1]
    den <- abs(pred$fit[1])
    numse <- sqrt(pred$se.fit[1]^2 + pred$se.fit[2]^2)
    dense <- pred$se.fit[1]
    
    sl6 = 100 * errordiv(num, den, numse, dense)[1] %>% as.numeric()
    slse6 = errordiv(num, den, numse, dense)[2] %>% as.numeric()
    
    
    modelfit <- lm(val_sample ~ timegroups, 
                   # one year dropped
                   data = .x[.x$timegroups != 2021,])
    
    pred <- predict(modelfit, se = TRUE,
                    # one year dropped
                    newdata = data.frame(timegroups = .x$timegroups[.x$timegroups != 2021]))
    
    num <- pred$fit[2] - pred$fit[1]
    den <- abs(pred$fit[1])
    numse <- sqrt(pred$se.fit[1]^2 + pred$se.fit[2]^2)
    dense <- pred$se.fit[1]
    
    sl7 = 100 * errordiv(num, den, numse, dense)[1] %>% as.numeric()
    slse7 = errordiv(num, den, numse, dense)[2] %>% as.numeric()
    
    
    modelfit <- lm(val_sample ~ timegroups, 
                   # one year dropped
                   data = .x[.x$timegroups != 2022,])
    
    pred <- predict(modelfit, se = TRUE,
                    # one year dropped
                    newdata = data.frame(timegroups = .x$timegroups[.x$timegroups != 2022]))
    
    num <- pred$fit[2] - pred$fit[1]
    den <- abs(pred$fit[1])
    numse <- sqrt(pred$se.fit[1]^2 + pred$se.fit[2]^2)
    dense <- pred$se.fit[1]
    
    sl8 = 100 * errordiv(num, den, numse, dense)[1] %>% as.numeric()
    slse8 = errordiv(num, den, numse, dense)[2] %>% as.numeric()
    
    
    .x %>%
      reframe(sl1 = sl1, slse1 = slse1,
              sl2 = sl2, slse2 = slse2,
              sl3 = sl3, slse3 = slse3,
              sl4 = sl4, slse4 = slse4,
              sl5 = sl5, slse5 = slse5,
              sl6 = sl6, slse6 = slse6,
              sl7 = sl7, slse7 = slse7,
              sl8 = sl8, slse8 = slse8)

    
  }) %>%
  group_by(COMMON.NAME) %>%
  reframe(mean.slope1 = mean(sl1),
          se.slope1 = sd(sl1) + sqrt(sum(slse1^2)/length(slse1)),
          mean.slope2 = mean(sl2),
          se.slope2 = sd(sl2) + sqrt(sum(slse2^2)/length(slse2)),
          mean.slope3 = mean(sl3),
          se.slope3 = sd(sl3) + sqrt(sum(slse3^2)/length(slse1)),
          mean.slope4 = mean(sl4),
          se.slope4 = sd(sl4) + sqrt(sum(slse4^2)/length(slse1)),
          mean.slope5 = mean(sl5),
          se.slope5 = sd(sl5) + sqrt(sum(slse5^2)/length(slse1)),
          mean.slope6 = mean(sl6),
          se.slope6 = sd(sl6) + sqrt(sum(slse6^2)/length(slse1)),
          mean.slope7 = mean(sl7),
          se.slope7 = sd(sl7) + sqrt(sum(slse7^2)/length(slse1)),
          mean.slope8 = mean(sl8),
          se.slope8 = sd(sl8) + sqrt(sum(slse8^2)/length(slse1))) %>%
  group_by(COMMON.NAME) %>%
  reframe(currentslopelci1 = mean.slope1 - 1.96*se.slope1,
          currentslopemean1 = mean.slope1,
          currentsloperci1 = mean.slope1 + 1.96*se.slope1,
          currentslopelci2 = mean.slope2 - 1.96*se.slope2,
          currentslopemean2 = mean.slope2,
          currentsloperci2 = mean.slope2 + 1.96*se.slope2,
          currentslopelci3 = mean.slope3 - 1.96*se.slope3,
          currentslopemean3 = mean.slope3,
          currentsloperci3 = mean.slope3 + 1.96*se.slope3,
          currentslopelci4 = mean.slope4 - 1.96*se.slope4,
          currentslopemean4 = mean.slope4,
          currentsloperci4 = mean.slope4 + 1.96*se.slope4,
          currentslopelci5 = mean.slope5 - 1.96*se.slope5,
          currentslopemean5 = mean.slope5,
          currentsloperci5 = mean.slope5 + 1.96*se.slope5,
          currentslopelci6 = mean.slope6 - 1.96*se.slope6,
          currentslopemean6 = mean.slope6,
          currentsloperci6 = mean.slope6 + 1.96*se.slope6,
          currentslopelci7 = mean.slope7 - 1.96*se.slope7,
          currentslopemean7 = mean.slope7,
          currentsloperci7 = mean.slope7 + 1.96*se.slope7,
          currentslopelci8 = mean.slope8 - 1.96*se.slope8,
          currentslopemean8 = mean.slope8,
          currentsloperci8 = mean.slope8 + 1.96*se.slope8)

# joining to main data
sens <- sens %>%
  left_join(sl_data_sens, by = c("eBird.English.Name.2022" = "COMMON.NAME"))

toc()




# projected trends
tic()
ext_trends <- temp_sims %>%
  group_by(COMMON.NAME, sim) %>%
  group_modify(~ {
    
    datatopred <- data.frame(timegroups = extra.years)
    
    modelfit <- lm(log(val_sample) ~ timegroups, data = .x)
    pred <- predict(modelfit, newdata = datatopred, se = TRUE)
    
    # No need to calculate slope here. 
    
    first_year <- .x %>% filter(timegroups == recentcutoff)
    
    .x %>%
      reframe(timegroups = datatopred,
              mean = pred$fit,
              se = pred$se.fit,
              # value in first year
              val = first_year$val)
    
  }) %>%
  # we want mean and SE of change in repfreq between years, so divide by first year value
  mutate(lci_bt = 100*exp(mean - 1.96 * se)/val, 
         mean_bt = 100*exp(mean)/val,
         rci_bt = 100*exp(mean + 1.96 * se)/val) %>%
  group_by(COMMON.NAME, timegroups) %>%
  reframe(lci_ext_std = mean(lci_bt),
          mean_ext_std = mean(mean_bt),
          rci_ext_std = mean(rci_bt))
toc()


