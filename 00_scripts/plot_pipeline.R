source("00_scripts/02_generate_plots")

gen_trend_plots("single", "LTT", "Alpine Swift")
gen_trend_plots("single", "LTT", "Lesser Sand-Plover") # in eBird name
gen_trend_plots("single_mask", "LTT", "Ashy Drongo")
gen_trend_plots("single_mask", "CAT", "Ashy Drongo")
gen_trend_plots("multi")
gen_trend_plots("composite")

