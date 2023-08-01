plot_soib_trends("single", "LTT", "Alpine Swift")
plot_soib_trends("single_mask", "CAT", "Ashy Drongo")
plot_soib_trends("single_mask", "LTT", "Ashy Drongo")

# plot_type = "single"
cur_mask = "PA"

# cur_spec <- "all"
fn_plot_type = "composite"
fn_cur_trend = "LTT"
fn_cur_mask = "cropland"

plot_load_filter_data(fn_plot_type = plot_type, 
                      fn_cur_trend = cur_trend, 
                      fn_cur_mask = "cropland")

plot_type <- "composite"
cur_trend = "CAT"
cur_spec = "Ashy Drongo"

cur_plot_metadata <- plot_metadata %>%
  filter(PLOT.NO == "02") %>% 
  dplyr::select(where(~ !any(is.na(.))))

# %>% 
#   mutate(PLOT.SPEC = str_split(PLOT.SPEC, ", ")) %>%
#   unnest(PLOT.SPEC)


###########


main = read.csv("01_analyses_full/results/SoIB_main.csv")
trends = read.csv("01_analyses_full/results/trends.csv")

qualifying.species = main$eBird.English.Name.2022[!main$SOIBv2.Long.Term.Status %in% 
                                                    c("eBird Data Inconclusive","eBird Data Deficient") & 
                                                    main$Long.Term.Analysis == "X"]

trends = trends %>% filter(COMMON.NAME %in% qualifying.species) %>%
  filter(timegroups <= 2022)
main = main %>% filter(eBird.English.Name.2022 %in% qualifying.species)






## diet composite

sps = "Composite Diet Guilds"

temp = main
temp$Diet.Guild[temp$Diet.Guild == ""] = NA

groups = unique(temp$Diet.Guild)
ct = 0
for (i in groups)
{
  ct = ct+1
  specs = temp$eBird.English.Name.2022[temp$Diet.Guild == i]
  temp_trends = trends %>% filter(COMMON.NAME %in% specs)
  temp_trends = temp_trends %>%
    group_by(timegroups,timegroupsf) %>% 
    reframe(mean_std = mean(mean_std),
            lci_std = mean(lci_std),
            rci_std = mean(rci_std))
  temp_trends$COMMON.NAME = i
  if (ct == 1)
    trend_group = temp_trends
  if (ct > 1)
    trend_group = rbind(trend_group,temp_trends)
}

temp = trend_group

t1 = temp[temp$timegroups == 2022,]
t1 = t1 %>% arrange(desc(mean_std))
order = t1$COMMON.NAME

