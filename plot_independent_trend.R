source('SoIB_v2 functions.R')
library(plyr)
library(tidyverse)
library(grid)
library(cowplot)
library(extrafont)


indtrends = read.csv("blackredstart.csv")
species = "Black Redstart"

temp = indtrends

scol = "#869B27"
#loadfonts(device = "win")

range = max(temp$cir)-min(temp$cil)

liml = min(temp$cil)-0.1*range
limu = max(temp$cir)+0.1*range

ybreaks = seq(liml,limu,length.out=5)
ybreaksl = round(ybreaks,2)

ggp = ggplot(temp, aes(x=timegroups, y=count)) + 
  geom_point(size = 3, colour = scol) +
  geom_line(size = 1.5, colour = scol) +
  geom_hline(yintercept = ybreaks[1], linetype = "dotted", size = 0.7) +
  geom_hline(yintercept = ybreaks[2], linetype = "dotted", size = 0.7) +
  geom_hline(yintercept = ybreaks[3], linetype = "dotted", size = 0.7) +
  geom_hline(yintercept = ybreaks[4], linetype = "dotted", size = 0.7) +
  geom_hline(yintercept = ybreaks[5], linetype = "dotted", size = 0.7) +
  geom_ribbon(aes(x = timegroups, ymin = cil,
                  ymax = cir), fill = scol, colour = NA, alpha = 0.3) +
  xlab("years") +
  ylab("density in Spiti (per ha)")

xbreaks1 = temp$timegroups
lbreaks1 = temp$timegroups

lbreaks1[c(3,6,8,10)] = ""

ggpx = ggp +
  theme(axis.title.x = element_blank(), 
        axis.text.x = element_text(size = 19, colour = "#56697B", vjust = -3, 
                                   margin = margin(0, 0, 0.8, 0, 'cm')),
        axis.title.y = element_text(size = 22, colour = "#56697B",vjust = -1,
                                    margin = margin(0, 0.8, 0, 0, 'cm')), 
        axis.ticks.x = element_line(size = 0.7, colour = "#56697B"), 
        axis.ticks.length=unit(.4, "cm"),
        axis.text.y = element_text(size = 24, colour = "#56697B", vjust = -0.4, hjust = 1, 
                                   margin = margin(0, -0.8, 0, 0, 'cm')),
        axis.ticks.y = element_blank(), 
        axis.line.x = element_line(size = 0.7, colour = "#56697B")) +
  theme(legend.title = element_blank(), legend.text = element_text(size = 12)) +
  theme(text=element_text(family="Gill Sans MT")) +
  scale_x_continuous(breaks = xbreaks1, labels = lbreaks1, limits = c(1992,2021)) +
  scale_y_continuous(expand=c(0,0),
                     breaks = c(ybreaks[1],ybreaks[2],ybreaks[3],ybreaks[4],ybreaks[5]),
                     labels = c(ybreaksl[1],ybreaksl[2],ybreaksl[3],
                                ybreaksl[4],ybreaksl[5]))+
  coord_cartesian(ylim = c(liml-0.1*range,limu+0.2*range), clip="off")+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        plot.margin=unit(c(0,0,0,0), "cm"),
        plot.title = element_text(hjust = 0.5),
        plot.background = element_rect(fill = "transparent",colour = NA),
        panel.background = element_rect(fill = "transparent",colour = NA))+
  theme(legend.position = "none")

rect1 = rectGrob(
  x = unit(5.46, "in"),
  y = unit(0.64, "in"),
  width = unit(0.15, "in"),
  height = unit(0.15, "in"),
  hjust = 0, vjust = 1,
  gp = gpar(fill = "white", col = "white", alpha = 1)
)
rect2 = rectGrob(
  x = unit(6.72, "in"),
  y = unit(0.64, "in"),
  width = unit(0.15, "in"),
  height = unit(0.15, "in"),
  hjust = 0, vjust = 1,
  gp = gpar(fill = "white", col = "white", alpha = 1)
)
rect3 = rectGrob(
  x = unit(8.89, "in"),
  y = unit(0.64, "in"),
  width = unit(0.15, "in"),
  height = unit(0.15, "in"),
  hjust = 0, vjust = 1,
  gp = gpar(fill = "white", col = "white", alpha = 1)
)
rect4 = rectGrob(
  x = unit(9.85, "in"),
  y = unit(0.64, "in"),
  width = unit(0.15, "in"),
  height = unit(0.15, "in"),
  hjust = 0, vjust = 1,
  gp = gpar(fill = "white", col = "white", alpha = 1)
)

ggpx2 = ggdraw(ggpx) +
  draw_grob(rect1) + draw_grob(rect2) + draw_grob(rect3) + draw_grob(rect4)

sps = species
name = paste(sps,"_","independent_trend_graph.jpg",sep="")

jpeg(name, units="in", width=11, height=7, res=1000, bg="transparent")
grid::grid.draw(ggpx2)
dev.off()