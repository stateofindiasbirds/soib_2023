require(tidyverse)

source("00_scripts/02_generate_plots.R")
source("00_scripts/00_plot_functions.R")

# single-species for full country ---------------------------------------------------

gen_trend_plots("single", "LTT") # 25 mins
gen_trend_plots("single", "CAT") # 20.5 mins

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
  # filter(CASE == "nannaj") %>% 
  pull(CASE) %>% 
  walk(., ~ gen_trend_plots_sysmon(.x))


# range maps -------------------------------------------------------------------------

# only for full country and states

soib_rangemap("Black-backed Dwarf-Kingfisher")
soib_rangemap(c("Black-backed Dwarf-Kingfisher", "Black Eagle"))
