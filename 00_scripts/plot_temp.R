plot_soib_trends("single", "none", "LTT", "Alpine Swift")

# plot_type = "single"
cur_mask = "none"
cur_trend = "LTT"
cur_spec = "Alpine Swift"
# cur_spec <- "all"
#

plot_type <- "single_mask"

ref_line <- c(75, 75, 50)
# plot_ybreaks <- c(0, 20, 50, 80, 100)
plot_ybreaks <- c(0, 20, 40, 80, 100)

###########


for (z in qualifying.species)
{

  t1 = temp[temp$timegroups == 2022,]
  t1 = t1 %>% arrange(desc(mean_std))
  order = t1$COMMON.NAME


  cols = cols.masks$cols
  ns = length(unique(temp$COMMON.NAME))
  cols1 = cols[c(1:ns)]
  
  bks1 = sort(unique(temp$COMMON.NAME))
  lbs1 = sort(unique(temp$COMMON.NAME))
  
  
  

  
  ggp = ggplot(temp, aes(x = timegroups, y = mean_std, col = COMMON.NAME, label = COMMON.NAMEx)) +
    geom_line(linewidth = 2) +
    geom_text_repel(nudge_x = -2, direction = "y", hjust = "center", size = 4, 
                    min.segment.length = Inf, fontface = "bold") +
    #geom_point(size = 3) +
    scale_colour_manual(breaks = bks1, 
                        labels = lbs1,
                        values = cols1) +
    scale_fill_manual(breaks = bks1, 
                      labels = lbs1,
                      values = cols1) +

}
