plot_soib_trends("single", "LTT", "Alpine Swift")
plot_soib_trends("single_mask", "LTT", "Ashy Drongo")

# plot_type = "single"
cur_mask = "PA"

# cur_spec <- "all"
fn_plot_type = "single_mask"
fn_cur_trend = "LTT"
fn_cur_mask = "cropland"

plot_load_filter_data(fn_plot_type = plot_type, 
                      fn_cur_trend = cur_trend, 
                      fn_cur_mask = "cropland")

plot_type <- "single_mask"
cur_trend = "LTT"
cur_spec = "Ashy Drongo"

ref_line <- c(75, 75, 50)
# plot_ybreaks <- c(0, 20, 50, 80, 100)
plot_ybreaks <- c(0, 20, 40, 80, 100)

###########

