# Same sequence of calls as 02_plot_pipeline.R -- no changes needed here,
# since the actual bug lived in gen_trend_plots()/gen_trend_plots_sysmon()/
# gen_range_maps() themselves (00_scripts/02_generate_plots.R), not in how
# this script calls them. Those three functions used to wipe every global
# object except themselves at the end of each call (including
# interannual_update, and -- for gen_range_maps() -- functions like
# soib_trend_plot() that a later bare call in this script depends on), so
# every call here after the first one would fail. That cleanup has been
# removed, so this script can now run start to finish in one go.

require(tidyverse)

interannual_update = TRUE

source("00_scripts/02_generate_plots.R")
source("00_scripts/00_plot_functions.R")

# single-species for full country ---------------------------------------------------

# gen_trend_plots("single", "LTT", "Ashy Prinia")
gen_trend_plots("single", "LTT") # 25 mins (<10 min with advanced Kenbunshoku Haki; 85 sec on server)
gen_trend_plots("single", "CAT") # 20.5 mins (11.3 min with advanced Kenbunshoku Haki; 70 sec on server)

# single-species masks vs country ---------------------------------------------------

# 2.8 sec per species * no.sp * (no. masks or states)
gen_trend_plots("single_mask", "LTT")
# 1.8 sec per species * no.sp * (no. masks or states)
gen_trend_plots("single_mask", "CAT")

# multiple species plots ------------------------------------------------------------

gen_trend_plots("multi") # 30 sec

# composite plots -------------------------------------------------------------------

gen_trend_plots("composite") # 15 sec

# range maps -------------------------------------------------------------------------

# only for full country and states

gen_range_maps("country") # 38 min
gen_range_maps("state") # 2.5 h

# insufficient data plot ------------------------------------------------------------

soib_trend_plot("stamp")
