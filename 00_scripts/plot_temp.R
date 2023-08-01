plot_soib_trends("single", "LTT", "Alpine Swift")
plot_soib_trends("single", "LTT", "Lesser Sand-Plover") # in eBird name
plot_soib_trends("single_mask", "LTT", "Ashy Drongo")
plot_soib_trends("single_mask", "CAT", "Ashy Drongo")
plot_soib_trends("multi")
plot_soib_trends("composite")


plot_type <- "single_mask"
cur_trend <- "CAT"
cur_spec <- "Ashy Drongo"
plot_load_filter_data(plot_type, cur_trend, "woodland")

plot_type <- "multi"
cur_trend <- NULL
cur_spec <- NULL
plot_load_filter_data(plot_type, cur_trend, "woodland")
cur_plot_metadata <- plot_metadata %>%
  filter(PLOT.NO == "05") %>% 
  mutate(PLOT.SPEC = str_split(PLOT.SPEC, ", ")) %>%
  unnest(PLOT.SPEC)


our_names <- c("Great Gray Shrike", "Jungle Bush-Quail", "Common Hawk-Cuckoo", 
               "Lesser Sand-Plover", "Eastern Marsh-Harrier")
specname_to_india_checklist(our_names)
