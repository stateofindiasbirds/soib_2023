library(tidyverse)

load("40_manuscripts/01_blueprint/dataforanalyses_extra_2023.RData")
data0 = data0 %>%
  select(COMMON.NAME,ST_NM,DISTRICT,ALL.SPECIES.REPORTED,group.id,month,year,no.sp,
         timegroups,DURATION.MINUTES,EFFORT.DISTANCE.KM)

regions = read.csv("00_data/eBird_district_region_mapping.csv") %>% 
  select(ST_NM,DISTRICT,region)

data0 = data0 %>% left_join(regions)

lossbygroup1 = data0 %>% 
  filter(ALL.SPECIES.REPORTED == 1) %>%
  group_by(timegroups) %>% mutate(lists = n_distinct(group.id)) %>%
  filter(!is.na(no.sp)) %>%
  mutate(type = "list length") %>%
  group_by(timegroups,type) %>% reframe(rat = 100*n_distinct(group.id)/max(lists))
lossbygroup2 = data0 %>% 
  filter(ALL.SPECIES.REPORTED == 1) %>%
  group_by(timegroups) %>% mutate(lists = n_distinct(group.id)) %>%
  filter(!is.na(DURATION.MINUTES)) %>%
  mutate(type = "duration") %>%
  group_by(timegroups,type) %>% reframe(rat = 100*n_distinct(group.id)/max(lists))
lossbygroup3 = data0 %>% 
  filter(ALL.SPECIES.REPORTED == 1) %>%
  group_by(timegroups) %>% mutate(lists = n_distinct(group.id)) %>%
  filter(!is.na(EFFORT.DISTANCE.KM)) %>%
  mutate(type = "distance") %>%
  group_by(timegroups,type) %>% reframe(rat = 100*n_distinct(group.id)/max(lists))

lossbygroup = lossbygroup1 %>% rbind(lossbygroup2) %>% rbind(lossbygroup3)


before20001 = data0 %>% 
  filter(timegroups == "before 2000") %>%
  filter(ALL.SPECIES.REPORTED == 1) %>%
  group_by(region) %>% mutate(lists = n_distinct(group.id)) %>%
  filter(!is.na(no.sp)) %>%
  mutate(type = "list length") %>%
  group_by(region,type) %>% reframe(rat = 100*n_distinct(group.id)/max(lists))
before20002 = data0 %>% 
  filter(timegroups == "before 2000") %>%
  filter(ALL.SPECIES.REPORTED == 1) %>%
  group_by(region) %>% mutate(lists = n_distinct(group.id)) %>%
  filter(!is.na(DURATION.MINUTES)) %>%
  mutate(type = "duration") %>%
  group_by(region,type) %>% reframe(rat = 100*n_distinct(group.id)/max(lists))
before20003 = data0 %>% 
  filter(timegroups == "before 2000") %>%
  filter(ALL.SPECIES.REPORTED == 1) %>%
  group_by(region) %>% mutate(lists = n_distinct(group.id)) %>%
  filter(!is.na(EFFORT.DISTANCE.KM)) %>%
  mutate(type = "distance") %>%
  group_by(region,type) %>% reframe(rat = 100*n_distinct(group.id)/max(lists))

before2000 = before20001 %>% rbind(before20002) %>% rbind(before20003) %>%
  filter(!is.na(region))

require(ggthemes)
theme_set(theme_tufte())
require(extrafont)

lossbygroup$timegroups = factor(lossbygroup$timegroups, 
                                levels = c("before 2000","2000-2006","2007-2010",
                                           "2011-2012","2013","2014","2015","2016",
                                           "2017","2018","2019","2020","2021","2022"))
lossbygroup$type = factor(lossbygroup$type,
                          levels = c("list length","duration","distance"))
before2000$type = factor(before2000$type,
                         levels = c("list length","duration","distance"))

ggp = ggplot(lossbygroup, aes(timegroups,rat,col = type, fill = type))+
  geom_bar(stat="identity",position="dodge", width = 0.7, alpha = 0.6)+
  geom_hline(yintercept = 50, linetype = "dotted", linewidth = 1) +
  geom_hline(yintercept = 75, linetype = "dotted", linewidth = 1) +
  xlab("time interval")+
  ylab("% available complete checklists\nby time period")+
  theme_bw()

ggp1 = ggp+
  theme(axis.title.x = element_blank(), axis.text.x = element_text(size = 12),
        axis.title.y = element_text(angle = 90, size = 14), axis.text.y = element_text(size = 14)) +
  theme(legend.title = element_blank(), legend.text = element_text(size = 12)) +
  scale_fill_manual(values = c("#E69F00", "#56B4E9", "#CC6666"),
                    breaks = c("list length","duration","distance"),
                    labels = c("List Length\n(no. of species in a checklist)",
                               "Duration\n(min)","Distance\n(km)")) +
  scale_colour_manual(values = c("#E69F00", "#56B4E9", "#CC6666"),
                      breaks = c("list length","duration","distance"),
                      labels = c("List Length\n(no. of species in a checklist)",
                                 "Duration\n(min)","Distance\n(km)")) +
  scale_x_discrete(breaks = c("before 2000","2000-2006","2007-2010",
                              "2011-2012","2013","2014","2015","2016",
                              "2017","2018","2019","2020","2021","2022"),
                   labels = c("before\n2000","2000\n2006","2007\n2010",
                              "2011\n2012","2013","2014","2015","2016",
                              "2017","2018","2019","2020","2021","2022")) +
  theme(text=element_text(family="Gandhi Sans")) +
  #theme(legend.position = "none")+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()
        ##panel.border = theme_blank(),
        ##panel.background = theme_blank()
  )

# before 2000 graphs

ggp = ggplot(before2000, aes(region,rat,col = type, fill = type))+
  geom_bar(stat="identity",position="dodge", width = 0.7, alpha = 0.6)+
  geom_hline(yintercept = 50, linetype = "dotted", linewidth = 1) +
  geom_hline(yintercept = 25, linetype = "dotted", linewidth = 1) +
  xlab("ecological region")+
  ylab("% available complete checklists\nbefore 2000")+
  theme_bw()

ggp2 = ggp+
  theme(axis.title.x = element_blank(), axis.text.x = element_text(size = 12),
        axis.title.y = element_text(angle = 90, size = 14), 
        axis.text.y = element_text(size = 14)) +
  theme(legend.title = element_blank(), legend.text = element_text(size = 12)) +
  scale_fill_manual(values = c("#E69F00", "#56B4E9", "#CC6666"),
                    breaks = c("list length","duration","distance"),
                    labels = c("List Length\n(no. of species in a checklist)",
                               "Duration\n(min)","Distance\n(km)")) +
  scale_colour_manual(values = c("#E69F00", "#56B4E9", "#CC6666"),
                      breaks = c("list length","duration","distance"),
                      labels = c("List Length\n(no. of species in a checklist)",
                                 "Duration\n(min)","Distance\n(km)")) +
  theme(text=element_text(family="Gandhi Sans")) +
  #theme(legend.position = "none")+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()
        ##panel.border = theme_blank(),
        ##panel.background = theme_blank()
  )

require(gridExtra)
require(grid)

grid_arrange_shared_legend <- function(...) {
  plots <- list(...)
  g <- ggplotGrob(plots[[1]] + theme(legend.position="bottom"))$grobs
  legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
  lheight <- sum(legend$height)
  grid.arrange(
    do.call(arrangeGrob, c(lapply(plots, function(x)
      x + theme(legend.position="none")), list(nrow = 2))),
    legend,
    ncol = 1,
    heights = unit.c(unit(1, "npc") - lheight, lheight))
}

jpeg('40_manuscripts/01_blueprint/figs/Fig. S8.jpg', units="in", width=10, height=7, res=600)
grid_arrange_shared_legend(ggp1,ggp2)
dev.off()




# S9


trends = read.csv("01_analyses_full/results/trends.csv")
trends1 = trends %>% filter(timegroupsf == "before 2000") %>%
  dplyr::select(mean) %>% mutate(type = "long-term")
trends2 = trends %>% filter(timegroupsf == "2015") %>%
  dplyr::select(mean) %>% mutate(type = "current")

trendscomb = trends1 %>% rbind(trends2) %>%
  rename(freq = mean)

require(ggthemes)
theme_set(theme_tufte())
require(extrafont)

ggp = ggplot(trendscomb, aes(freq, fill = type))+
  geom_histogram(binwidth = c(0.01), col = "black")+
  facet_grid(type ~ ., scale="free_y")+
  geom_vline(xintercept = 0.5, linetype = "dotted", linewidth = 1) +
  xlab("Frequency of reporting in the first time-interval")+
  ylab("Number of species")+
  theme_bw()

ggp1 = ggp +
  theme(axis.title.x = element_text(vjust = 0.3, size = 20), axis.text.x = element_text(size = 14), 
        axis.title.y = element_text(vjust = 0.3, angle = 90, size = 20), axis.text.y = element_text(size = 14)) +
  scale_x_continuous(limits = c(0,1))+
  scale_fill_manual(values=c("#CCCCCC","#FFFFFF")) +
  theme(text=element_text(family="Gandhi Sans")) +
  theme(legend.position = "none")+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()
        ##panel.border = theme_blank(),
        ##panel.background = theme_blank()
  )+
  theme(strip.text.y = element_text(size = 20, vjust = 1.7, angle = -90))

jpeg('40_manuscripts/01_blueprint/figs/Fig. S9.jpg', units="in", width=10, height=7, res=600)
grid::grid.draw(ggp1)
dev.off()