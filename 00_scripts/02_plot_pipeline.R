require(tidyverse)

source("00_scripts/02_generate_plots.R")
source("00_scripts/00_plot_functions.R")

# single-species for full country ---------------------------------------------------

gen_trend_plots("single", "LTT") # 20 mins
gen_trend_plots("single", "CAT") # 19.5 mins

# single-species masks vs country ---------------------------------------------------

gen_trend_plots("single_mask", "LTT") # 2.8 sec per species * no.sp * (no. masks or states)
gen_trend_plots("single_mask", "CAT") # 1.8 sec per species * no.sp * (no. masks or states)

# multiple species plots ------------------------------------------------------------

gen_trend_plots("multi") # 30 sec

# composite plots -------------------------------------------------------------------

gen_trend_plots("composite") # 15 sec

# systematic monitoring plots -------------------------------------------------------

fetch_sysmon_metadata("full")

# 2 mins
sysmon_metadata %>% 
  filter(CASE != "eaglenest") %>% 
  pull(CASE) %>% 
  walk(., ~ gen_trend_plots_sysmon(.x))
