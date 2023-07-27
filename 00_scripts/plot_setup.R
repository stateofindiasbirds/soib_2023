# packages -------------------------------------------------------

library(tidyverse)
library(ggdist)
library(ggridges)
library(ggpubr)
library(ggrepel)
library(cowplot)
library(extrafont)
library(glue)

# current metadata --------------------------------------------------

load("00_data/analyses_metadata.RData")
cur_metadata <- analyses_metadata %>% filter(MASK == cur_mask)

path_data_main <- cur_metadata$SOIBMAIN.PATH
path_data_trends <- cur_metadata$TRENDS.OUTPATH


# path (folder) to write to
# create path if doesn't exist
if (cur_trend == "LTT") {
  path_write <- cur_metadata %>% 
    mutate(PLOT.OUTPATH = glue("{PLOT.SINGLE.FOLDER}long-term trends/")) %>% 
    pull(PLOT.OUTPATH)
} else if (cur_trend == "CAT") {
  path_write <- cur_metadata %>% 
    mutate(PLOT.OUTPATH = glue("{PLOT.SINGLE.FOLDER}current trends/")) %>% 
    pull(PLOT.OUTPATH)
}

if (!dir.exists(path_write)) {dir.create(path_write, recursive = TRUE)}


# plot theme settings -----------------------------------------------------

palette_trend_groups <- c("#869B27", "#E49B36", "#436b74", "#CC6666", 
                          "#B69AC9", "#78CAE0","#31954E","#493F3D",
                          "#EA5599", "#9999CC", "#A13E2B", "#66CC99")

palette_plot_elem <- "#56697B"

palette_plot_title <- "#A13E2B"


# other plot settings

if (cur_trend == "LTT") {
  timegroups_lab <- c("before 2000", "2000-2006", "2007-2010", "2011-2012", 
                      "2013", "2014", "2015", "2016", "2017", "2018", "2019", 
                      "2020", "2021", "2022")
} else if (cur_trend == "CAT") {
  timegroups_lab <- c("2015", "2016", "2017", "2018", "2019", "2020", "2021", "2022")
}


# load data ---------------------------------------------------------------

source('00_scripts/00_functions.R')

data_main = read.csv(path_data_main)
data_trends = read.csv(path_data_trends)


# filtering for qualified species only
# - not plotting inconclusive or data deficient; 
# - only species sel. for that trend; 
# - only till MY 2022

if (cur_trend == "LTT") {
  
  spec_qual <- data_main %>% 
    filter(!(SOIBv2.Long.Term.Status %in% c("eBird Data Inconclusive",
                                            "eBird Data Deficient")),
           Long.Term.Analysis == "X") %>%  #### ### ### ###
    pull(eBird.English.Name.2022)
  
  data_trends <- data_trends %>% 
    filter(COMMON.NAME %in% spec_qual,
           timegroups <= 2022)
  
} else if (cur_trend == "CAT") {
  
  spec_qual <- data_main %>% 
    filter(!(SOIBv2.Current.Status %in% c("eBird Data Indecisive",
                                          "eBird Data Deficient")),
           Current.Analysis == "X") %>%  #### ### ### ###
    pull(eBird.English.Name.2022)
  
  data_trends <- data_trends %>% 
    filter(COMMON.NAME %in% spec_qual,
           timegroups >= 2015 & timegroups <= 2022)
  
}



#### loop over all species in spec_qual

path_write_file <- glue("{path_write}{cur_spec}.png")

