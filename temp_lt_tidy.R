tic("tidy lt trends (full)")

set.seed(10)

modtrends = modtrends %>% 
  # calculating CIs
  group_by(COMMON.NAME, timegroups) %>% 
  # 1000 simulations of transformed ratio of present:original values
  # quantiles*100 from these gives us our CI limits for mean_std
  reframe(tp0 = simerrordiv(mean_trans, m1, se_trans, s1)$rat) %>% 
  group_by(COMMON.NAME, timegroups) %>% 
  reframe(lci_std = 100*as.numeric(quantile(tp0, 0.025)),
          rci_std = 100*as.numeric(quantile(tp0, 0.975))) %>% 
  right_join(modtrends, by = c("COMMON.NAME", "timegroups"))


# saving the values for 2022 in "main" as well:
# temp object then left_join instead of right_join because species order in main
# needs to be preserved
temp <- modtrends %>%
  filter(timegroups == 2022) %>%
  dplyr::select(COMMON.NAME, lci_std, mean_std, rci_std) %>%
  rename(longtermlci = lci_std,
         longtermmean = mean_std,
         longtermrci = rci_std)

main <- main %>%
  left_join(temp, by = c("eBird.English.Name.2022" = "COMMON.NAME"))

modtrends = modtrends %>%
  group_by(COMMON.NAME) %>%
  # making CI band zero for first year
  mutate(lci_std = case_when(timegroups == first(timegroups) ~ mean_std,
                             TRUE ~ lci_std),
         rci_std = case_when(timegroups == first(timegroups) ~ mean_std,
                             TRUE ~ rci_std)) %>%
  ungroup() %>%
  dplyr::select(timegroupsf, timegroups, COMMON.NAME, lci_std, mean_std, rci_std)


toc(quiet = TRUE, log = TRUE)
