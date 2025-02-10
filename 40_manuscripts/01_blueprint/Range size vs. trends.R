## Comparison table of range size vs. trends

main = read.csv("01_analyses_full/results/SoIB_main.csv")

range_ltt <- main %>%
  filter(Selected.SOIB == "X") %>%
  mutate(SOIBv2.Range.Status = factor(SOIBv2.Range.Status, levels = c(
    "Historical", "Very Restricted", 
    "Restricted", "Moderate", "Large", "Very Large")),
    SOIBv2.Long.Term.Status = factor(SOIBv2.Long.Term.Status, levels = c(
      "Insufficient Data", "Trend Inconclusive", "Rapid Decline", "Decline",
      "Stable", "Increase", "Rapid Increase"
    ))) %>%
  group_by(SOIBv2.Range.Status, SOIBv2.Long.Term.Status) %>%
  tally() %>%
  pivot_wider(names_from = SOIBv2.Long.Term.Status, values_from = n) %>%
  replace(is.na(.), 0) %>%
  magrittr::set_colnames(c(" ", "Insufficient Data", "Trend Inconclusive", "Rapid Decline", "Decline",
                           "Stable", "Increase", "Rapid Increase"))

range_ltt_perc = range_ltt %>%
  mutate(Sum = `Insufficient Data` + `Trend Inconclusive` + `Rapid Decline` + `Decline` +
           Stable + Increase + `Rapid Increase`) %>%
  mutate(across(c("Insufficient Data", "Trend Inconclusive", "Rapid Decline", "Decline",
                  "Stable", "Increase", "Rapid Increase"),
                ~ round(100 * (. / Sum), 0))) %>%
  mutate(Sum = NULL)


range_cat <- main %>%
  filter(Selected.SOIB == "X") %>%
  mutate(SOIBv2.Range.Status = factor(SOIBv2.Range.Status, levels = c(
    "Historical", "Very Restricted", 
    "Restricted", "Moderate", "Large", "Very Large")),
    SOIBv2.Current.Status = factor(SOIBv2.Current.Status, levels = c(
      "Insufficient Data", "Trend Inconclusive", "Rapid Decline", "Decline",
      "Stable", "Increase", "Rapid Increase"
    ))) %>%
  group_by(SOIBv2.Range.Status, SOIBv2.Current.Status) %>%
  tally() %>%
  pivot_wider(names_from = SOIBv2.Current.Status, values_from = n) %>%
  replace(is.na(.), 0) %>%
  magrittr::set_colnames(c(" ", "Insufficient Data", "Trend Inconclusive", "Rapid Decline", "Decline",
                           "Stable", "Increase", "Rapid Increase"))

range_cat_perc = range_cat %>%
  mutate(Sum = `Insufficient Data` + `Trend Inconclusive` + `Rapid Decline` + `Decline` +
           Stable + Increase + `Rapid Increase`) %>%
  mutate(across(c("Insufficient Data", "Trend Inconclusive", "Rapid Decline", "Decline",
                  "Stable", "Increase", "Rapid Increase"),
                ~ round(100 * (. / Sum), 0))) %>%
  mutate(Sum = NULL)

range_trends = range_ltt_perc %>%
  bind_rows(range_cat_perc)

write.csv(range_trends,"40_manuscripts/01_blueprint/range_trends_crosstab.csv",row.names = F)
