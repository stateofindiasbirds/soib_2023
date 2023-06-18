
# sensitivity analysis of recent trends
tic("current trends: tidy (full but 5spp.) REVISED")

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

# main recent trends
sl_data_main <- temp_sims %>% 
  group_by(COMMON.NAME, sim) %>% 
  # arrange(COMMON.NAME, sim) %>%
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

toc(quiet = TRUE, log = TRUE)
