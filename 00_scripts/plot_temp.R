plot_soib_trends("single", "LTT", "Alpine Swift")
plot_soib_trends("single_mask", "CAT", "Ashy Drongo")
plot_soib_trends("single_mask", "LTT", "Ashy Drongo")

# plot_type = "single"
cur_mask = "PA"

# cur_spec <- "all"
fn_plot_type = "multi"
fn_cur_trend = "LTT"
fn_cur_mask = "cropland"

plot_load_filter_data(fn_plot_type = plot_type, 
                      fn_cur_trend = cur_trend, 
                      fn_cur_mask = "cropland")

plot_type <- "multi"
cur_trend = "CAT"
cur_spec = "Ashy Drongo"

cur_plot_metadata <- plot_metadata %>%
  filter(PLOT.NO == "09") %>% 
  mutate(PLOT.SPEC = str_split(PLOT.SPEC, ", ")) %>%
  unnest(PLOT.SPEC)


###########

