source('SoIB_v2 functions.R')
library(tidyverse)
library(grid)
library(cowplot)
library(extrafont)


trends = read.csv("trends.csv")
temp = trends %>% 
  mutate(lci_std = lci_std_2014,mean_std = mean_std_2014,rci_std = rci_std_2014) %>%
  filter(timegroups >= 2014 & timegroups <= 2021) %>%
  filter(COMMON.NAME %in% c("Silver-eared Mesia"))

scol = "#869B27"
  #loadfonts(device = "win")

maxci = temp$rci_std
minci = temp$lci_std

liml = min(minci[!is.na(minci)])
liml = plyr::round_any(liml,50,floor)

limu = max(maxci[!is.na(maxci)])
limu = plyr::round_any(limu,50,ceiling)

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
    liml = plyr::round_any(ybreaks[1],50,floor)
  }
  if (min(tmp1) < 0 & any(tmp1 == 100))
  {
    ybreaks = ybreaks + tmpx[2]
    limu = plyr::round_any(ybreaks[5],50,ceiling)
    limu = limu + round(0.01*(limu-liml))
  }
  if (any(tmp2 == 100))
  {
    ybreaks = tmp2
    limu = plyr::round_any(ybreaks[5],50,ceiling)
    limu = limu + round(0.01*(limu-liml))
  }
  
  ybreaks = plyr::round_any(ybreaks,10,round)
}

ybreaksl = rep("",5)

for (j in 1:5)
{
  ybreaksl[j] = paste("+",(ybreaks[j]-100),"%",sep="")
  if (ybreaks[j] <= 100)
    ybreaksl[j] = paste((ybreaks[j]-100),"%",sep="")
}


ggp = ggplot(temp, aes(x=timegroups, y=mean_std)) + 
  geom_point(size = 3, colour = scol) +
  geom_line(size = 1.5, colour = scol) +
  geom_hline(yintercept = ybreaks[1], linetype = "dotted", size = 0.7) +
  geom_hline(yintercept = ybreaks[2], linetype = "dotted", size = 0.7) +
  geom_hline(yintercept = ybreaks[3], linetype = "dotted", size = 0.7) +
  geom_hline(yintercept = ybreaks[4], linetype = "dotted", size = 0.7) +
  geom_hline(yintercept = ybreaks[5], linetype = "dotted", size = 0.7) +
  geom_ribbon(aes(x = timegroups, ymin = lci_std,
                  ymax = rci_std), fill = scol, colour = NA, alpha = 0.3) +
  xlab("years") +
  ylab("change in eBird abundance index")

xbreaks1 = temp$timegroups[1:8]
lbreaks1 = temp$timegroupsf[1:8]


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

ggpx1 = ggdraw(ggpx)

sps = as.character(temp$COMMON.NAME[1])
name = paste(sps,"_","eBird_trend_graph.jpg",sep="")

jpeg(name, units="in", width=11, height=7, res=1000, bg="transparent")
grid::grid.draw(ggpx1)
dev.off()