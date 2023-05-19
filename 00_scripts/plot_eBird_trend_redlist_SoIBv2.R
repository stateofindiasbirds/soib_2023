source('SoIB_v2 functions.R')
library(tidyverse)
library(ggdist)
library(ggridges)
library(ggpubr)
library(cowplot)
library(extrafont)


trends = read.csv("trends.csv")
temp = trends %>% 
  mutate(lci_std = lci_comb_std,mean_std = mean_comb_std,rci_std = rci_comb_std) %>%
  filter(timegroups >= 2014 & timegroups <= 2027) %>%
  filter(COMMON.NAME %in% c("Kentish Plover"))


scol = "#869B27"
  #loadfonts(device = "win")

tg = c("2014", "2015", "2016", "2017", "2018", "2019", "2020", "2021",
       "2022", "2023", "2024", "2025", "2026", "2027")

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

ybreaksl[ybreaks == 100] = ""



######################### get the x-axis right


x_tick_preBas = seq(2014, 2027) + 0.5

ggp = ggplot(temp, aes(x = timegroups, y = mean_std, ymin = lci_std, ymax = rci_std)) +
  geom_lineribbon(fill = scol, col = "black", linewidth = 0.7) +
  geom_point(size = 3, color = "black") +
  geom_bracket(
    inherit.aes = FALSE, 
    xmin = c(seq(2014, 2026)) + 0.5, 
    xmax = c(seq(2015, 2027)) + 0.5,
    y.position = liml-0.02*range,
    bracket.shorten = 0.15,
    tip.length = 0.05,
    vjust = 3,
    label = tg[-1],
    label.size = 3) +
  scale_x_continuous(
    breaks = c(seq(2015, 2027), x_tick_preBas),
    labels = c(paste0(seq(2015, 2027)), rep(c(""), length(x_tick_preBas))),
    limits = c(2014.5, 2027.5)) +
  geom_hline(yintercept = ybreaks[1], linetype = "dotted", linewidth = 0.7) +
  geom_hline(yintercept = ybreaks[2], linetype = "dotted", linewidth = 0.7) +
  geom_hline(yintercept = ybreaks[3], linetype = "dotted", linewidth = 0.7) +
  geom_hline(yintercept = ybreaks[4], linetype = "dotted", linewidth = 0.7) +
  geom_hline(yintercept = ybreaks[5], linetype = "dotted", linewidth = 0.7) +
  geom_hline(yintercept = 100, linetype = "solid", linewidth = 0.9) +
  xlab("time-steps") +
  ylab("change in eBird abundance index")

ggpx = ggp +
  theme(axis.title.x = element_blank(), 
        axis.title.y = element_text(size = 22, colour = "#56697B",
                                    margin = margin(0, 0.8, 0, 0.4, 'cm')), 
        axis.text.y = element_text(size = 20, colour = "#56697B", vjust = -0.4, hjust = 1, 
                                   margin = margin(0, -0.8, 0, 0, 'cm')),
        axis.ticks.y = element_blank()) +
  theme(legend.title = element_blank(), legend.text = element_text(size = 12)) +
  theme(text=element_text(family="Gill Sans MT")) +
  scale_y_continuous(expand=c(0,0),
                     breaks = c(ybreaks[1],ybreaks[2],ybreaks[3],ybreaks[4],ybreaks[5]),
                     labels = c(ybreaksl[1],ybreaksl[2],ybreaksl[3],
                                ybreaksl[4],ybreaksl[5]))+
  annotate("text", x = 2014.5, y = 100 + range*0.055, label = "2014", 
           colour = "black", family="Gill Sans MT", size = 5)+
  annotate("text", x = 2014.5, y = 100 + range*0.025, label = "baseline", 
           colour = "black", family="Gill Sans MT", size = 5)+
  coord_cartesian(ylim = c(liml-0.1*range,limu+0.1*range), clip="off")+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        plot.margin=unit(c(0,0,0,0), "cm"),
        legend.title = element_blank(),
        axis.line.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        plot.background = element_rect(fill = "transparent",colour = NA),
        panel.background = element_rect(fill = "transparent",colour = NA))+
  theme(legend.position = "none")

ggpx1 = ggdraw(ggpx)

sps = as.character(temp$COMMON.NAME[1])
name = paste(sps,"_","eBird_trend_redlist_SoIBv2.jpg",sep="")

jpeg(name, units="in", width=11, height=7, res=1000, bg="transparent")
grid::grid.draw(ggpx1)
dev.off()