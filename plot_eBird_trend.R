source('SoIB_v2 functions.R')
library(plyr)
library(tidyverse)
library(grid)
library(cowplot)
library(extrafont)


trends = read.csv("assorted_trends_2.csv")
trends = trends %>% filter(species %in% c("Small Minivet"))

scol = "#869B27"
temp = stdtrends(trends)
#loadfonts(device = "win")

maxci = temp$nmfreqbyspec + temp$nmsebyspec*1.96
minci = temp$nmfreqbyspec - temp$nmsebyspec*1.96

liml = min(minci)
liml = round_any(liml,50,floor)

limu = max(maxci)
limu = round_any(limu,50,ceiling)

if ((limu-liml) < 100 & liml < 0)
  liml = liml - 50
if ((limu-liml) < 100 & limu > 0)
  limu = limu + 50

range = limu-liml

ybreaks = seq(liml,limu,length.out=5)

if (any(ybreaks != 100))
{
  tmpx = sort((abs(ybreaks-100)))
  tmp = tmpx[1]
  tmp1 = ybreaks - tmp
  tmp2 = ybreaks + tmp
  if (any(tmp1 == 100) & min(tmp1) >= 0)
  {
    ybreaks = tmp1
    liml = round_any(ybreaks[1],50,floor)
  }
  if (min(tmp1) < 0 & any(tmp1 == 100))
  {
    ybreaks = ybreaks + tmpx[2]
    limu = round_any(ybreaks[5],50,ceiling)
    limu = limu + round(0.01*(limu-liml))
  }
  if (any(tmp2 == 100))
  {
    ybreaks = tmp2
    limu = round_any(ybreaks[5],50,ceiling)
    limu = limu + round(0.01*(limu-liml))
  }
  
  ybreaks = round_any(ybreaks,10,round)
}

ybreaksl = rep("",5)

for (j in 1:5)
{
  ybreaksl[j] = paste("+",(ybreaks[j]-100),"%",sep="")
  if (ybreaks[j] <= 100)
    ybreaksl[j] = paste((ybreaks[j]-100),"%",sep="")
}


ggp = ggplot(temp, aes(x=timegroups, y=nmfreqbyspec)) + 
  geom_point(size = 3, colour = scol) +
  geom_line(size = 1.5, colour = scol) +
  geom_hline(yintercept = ybreaks[1], linetype = "dotted", size = 0.7) +
  geom_hline(yintercept = ybreaks[2], linetype = "dotted", size = 0.7) +
  geom_hline(yintercept = ybreaks[3], linetype = "dotted", size = 0.7) +
  geom_hline(yintercept = ybreaks[4], linetype = "dotted", size = 0.7) +
  geom_hline(yintercept = ybreaks[5], linetype = "dotted", size = 0.7) +
  geom_ribbon(aes(x = timegroups, ymin = (nmfreqbyspec - nmsebyspec*1.96),
                  ymax = (nmfreqbyspec + nmsebyspec*1.96)), fill = scol, colour = NA, alpha = 0.3) +
  xlab("years") +
  ylab("change in eBird abundance index")

xbreaks1 = temp$timegroups[1:13]
lbreaks1 = temp$timegroupsf[1:13]
lbreaks1[1:3] = c(paste(sprintf('\u2190')," before 2000"),"2000-06","2007-10")
lbreaks1[c(4,6,8,10,12)] = ""

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
  scale_x_continuous(breaks = xbreaks1, labels = lbreaks1) +
  scale_y_continuous(expand=c(0,0),
                     breaks = c(ybreaks[1],ybreaks[2],ybreaks[3],ybreaks[4],ybreaks[5]),
                     labels = c(ybreaksl[1],ybreaksl[2],ybreaksl[3],
                                ybreaksl[4],ybreaksl[5]))+
  coord_cartesian(ylim = c(liml-0.1*range,limu+0.1*range), clip="off")+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        plot.margin=unit(c(0,0,0,0), "cm"),
        plot.title = element_text(hjust = 0.5),
        plot.background = element_rect(fill = "transparent",colour = NA),
        panel.background = element_rect(fill = "transparent",colour = NA))+
  theme(legend.position = "none")

rect1 = rectGrob(
  x = unit(7.74, "in"),
  y = unit(0.64, "in"),
  width = unit(0.15, "in"),
  height = unit(0.15, "in"),
  hjust = 0, vjust = 1,
  gp = gpar(fill = "white", col = "white", alpha = 1)
)
rect2 = rectGrob(
  x = unit(8.35, "in"),
  y = unit(0.64, "in"),
  width = unit(0.15, "in"),
  height = unit(0.15, "in"),
  hjust = 0, vjust = 1,
  gp = gpar(fill = "white", col = "white", alpha = 1)
)
rect3 = rectGrob(
  x = unit(8.97, "in"),
  y = unit(0.64, "in"),
  width = unit(0.15, "in"),
  height = unit(0.15, "in"),
  hjust = 0, vjust = 1,
  gp = gpar(fill = "white", col = "white", alpha = 1)
)
rect4 = rectGrob(
  x = unit(9.58, "in"),
  y = unit(0.64, "in"),
  width = unit(0.15, "in"),
  height = unit(0.15, "in"),
  hjust = 0, vjust = 1,
  gp = gpar(fill = "white", col = "white", alpha = 1)
)
rect5 = rectGrob(
  x = unit(10.19, "in"),
  y = unit(0.64, "in"),
  width = unit(0.15, "in"),
  height = unit(0.15, "in"),
  hjust = 0, vjust = 1,
  gp = gpar(fill = "white", col = "white", alpha = 1)
)

ggpx1 = ggdraw(ggpx) +
  draw_grob(rect1) + draw_grob(rect2) + draw_grob(rect3) + draw_grob(rect4) +
  draw_grob(rect5)

sps = as.character(trends$species[1])
name = paste(sps,"_","eBird_trend_graph.jpg",sep="")

jpeg(name, units="in", width=11, height=7, res=1000, bg="transparent")
grid::grid.draw(ggpx1)
dev.off()