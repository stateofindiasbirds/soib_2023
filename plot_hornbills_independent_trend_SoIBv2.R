source('SoIB_v2 functions.R')
library(tidyverse)
library(ggdist)
library(ggridges)
library(ggpubr)
library(cowplot)
library(extrafont)


## independent (encounter rate - detections per km)

indtrends = read.csv("data_independent/hornbills_papumrf.csv")
sps = "Hornbills"

temp = indtrends

t1 = temp[temp$timegroups == 2022,]
t1 = t1 %>% arrange(desc(mean))
order = t1$species


temp$species = factor(temp$species, 
                      levels = order)

cols = c("#869B27", "#31954E", "#E49B36", "#CC6666", "#78CAE0", "#A13E2B", "#EA5599", "#493F3D",
         "#B69AC9", "#CC6666", "#9999CC", "#000000", "#66CC99")
#cols = c("#41726c","#2d809b","#e0d27b","#8cc48c","#55bfaf")
tcol = "black"

ns = length(unique(temp$species))


cols1 = cols[c(1:ns)]
bks1 = sort(unique(temp$species))
lbs1 = sort(unique(temp$species))

tg = c("2015", "2016", "2017", "2018", "2019", "2020", "2021", "2022")

#loadfonts(device = "win")

range = max(temp$cir)-min(temp$cil)

liml = min(temp$cil)-0.1*range
if (liml < 0)
  liml = 0
lm = liml
limu = max(temp$cir)+0.1*range
um = limu

ybreaks = seq(liml,limu,length.out=5)
ybreaksl = round(ybreaks,2)


######################### get the x-axis right


x_tick_preBas = seq(2015, 2022) + 0.5

pd = position_dodge(0.3)

ggp = ggplot(temp, aes(x = timegroups, y = mean,col = species)) +
  geom_line(linewidth = 2, position = pd) +
  geom_errorbar(aes(ymin = cil, ymax = cir), linewidth = 1, width = 0.2, position = pd) +
  #geom_point(size = 3) +
  ggtitle(sps) +
  #geom_bracket(
  #  inherit.aes = FALSE, 
  #  xmin = c(seq(2015, 2021)) + 0.5, 
  #  xmax = c(seq(2016, 2022)) + 0.5,
  #  y.position = lm-0.01*range,
  #  bracket.shorten = 0.15,
  #  tip.length = 0.04,
  #  vjust = 3,
  #  label = tg[-1],
  #  label.size = 3) +
  scale_colour_manual(breaks = bks1, 
                      labels = lbs1,
                      values = cols1) +
  scale_fill_manual(breaks = bks1, 
                    labels = lbs1,
                    values = cols1) +
  scale_x_continuous(
    breaks = c(seq(2016, 2022), x_tick_preBas),
    labels = c(paste0(seq(2016, 2022)), rep(c(""), length(x_tick_preBas))),
    limits = c(2015.5, 2022.5)) +
  geom_hline(yintercept = ybreaks[1], linetype = "dotted", linewidth = 0.7) +
  geom_hline(yintercept = ybreaks[2], linetype = "dotted", linewidth = 0.7) +
  geom_hline(yintercept = ybreaks[3], linetype = "dotted", linewidth = 0.7) +
  geom_hline(yintercept = ybreaks[4], linetype = "dotted", linewidth = 0.7) +
  geom_hline(yintercept = ybreaks[5], linetype = "dotted", linewidth = 0.7) +
  #geom_hline(yintercept = 100, linetype = "solid", linewidth = 0.9) +
  xlab("time-steps") +
  ylab("detections per km in Papum RF")

ggpt = ggplot(temp, aes(x = timegroups, y = mean,col = species)) +
  geom_line(linewidth = 2, position = pd) +
  geom_errorbar(aes(ymin = cil, ymax = cir), linewidth = 1, width = 0.2, position = pd) +
  #geom_point(size = 3) +
  ggtitle(sps) +
  geom_bracket(
    inherit.aes = FALSE, 
    xmin = c(seq(2015, 2021)) + 0.5, 
    xmax = c(seq(2016, 2022)) + 0.5,
    y.position = lm-0.01*range,
    bracket.shorten = 0.15,
    tip.length = 0.04,
    vjust = 3,
    label = tg[-1],
    label.size = 3) +
  scale_colour_manual(breaks = bks1, 
                      labels = lbs1,
                      values = cols1) +
  scale_fill_manual(breaks = bks1, 
                    labels = lbs1,
                    values = cols1) +
  scale_x_continuous(
    breaks = c(seq(2016, 2022), x_tick_preBas),
    labels = c(paste0(seq(2016, 2022)), rep(c(""), length(x_tick_preBas))),
    limits = c(2015.5, 2022.5)) +
  geom_hline(yintercept = ybreaks[1], linetype = "dotted", linewidth = 0.7) +
  geom_hline(yintercept = ybreaks[2], linetype = "dotted", linewidth = 0.7) +
  geom_hline(yintercept = ybreaks[3], linetype = "dotted", linewidth = 0.7) +
  geom_hline(yintercept = ybreaks[4], linetype = "dotted", linewidth = 0.7) +
  geom_hline(yintercept = ybreaks[5], linetype = "dotted", linewidth = 0.7) +
  #geom_hline(yintercept = 100, linetype = "solid", linewidth = 0.9) +
  xlab("time-steps") +
  ylab("detections per km in Papum RF")

ggpx = ggp +
  theme(axis.title.x = element_blank(), 
        axis.title.y = element_text(size = 22, colour = "#56697B",
                                    margin = margin(0, 0.8, 0, 0.4, 'cm')), 
        axis.text.y = element_text(size = 20, colour = "#56697B", vjust = -0.4, hjust = 1, 
                                   margin = margin(0, -0.8, 0, 0, 'cm')),
        axis.ticks.y = element_blank()) +
  theme(legend.title = element_blank(), legend.text = element_text(size = 12),
        plot.title = element_text(face = 'italic', size = 20, hjust = 0.5, vjust = 0.5),
        legend.key = element_blank())+
  theme(text=element_text(family="Gill Sans MT")) +
  scale_y_continuous(expand=c(0,0),
                     breaks = c(ybreaks[1],ybreaks[2],ybreaks[3],ybreaks[4],ybreaks[5]),
                     labels = c(ybreaksl[1],ybreaksl[2],ybreaksl[3],
                                ybreaksl[4],ybreaksl[5]))+
  coord_cartesian(ylim = c(lm-0.1*range,um+0.1*range), clip="off")+
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
  guides(colour = guide_legend(nrow = 1))+
  theme(legend.position = "none")

ggptx = ggpt +
  theme(axis.title.x = element_blank(), 
        axis.title.y = element_text(size = 22, colour = "#56697B",
                                    margin = margin(0, 0.8, 0, 0.4, 'cm')), 
        axis.text.y = element_text(size = 20, colour = "#56697B", vjust = -0.4, hjust = 1, 
                                   margin = margin(0, -0.8, 0, 0, 'cm')),
        axis.ticks.y = element_blank()) +
  theme(legend.title = element_blank(), legend.text = element_text(size = 12),
        plot.title = element_text(face = 'italic', size = 20, hjust = 0.5, vjust = 0.5),
        legend.key = element_blank())+
  theme(text=element_text(family="Gill Sans MT")) +
  scale_y_continuous(expand=c(0,0),
                     breaks = c(ybreaks[1],ybreaks[2],ybreaks[3],ybreaks[4],ybreaks[5]),
                     labels = c(ybreaksl[1],ybreaksl[2],ybreaksl[3],
                                ybreaksl[4],ybreaksl[5]))+
  coord_cartesian(ylim = c(lm-0.1*range,um+0.1*range), clip="off")+
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
  guides(colour = guide_legend(nrow = 1))+
  theme(legend.position = "none")

ggpx3 = ggdraw(ggpx)
ggptx3 = ggdraw(ggptx)

name = paste("trends_graphs/independent trends/",sps,"_","independent.jpg",sep="")
jpeg(name, units="in", width=11, height=7, res=1000, bg="transparent")
grid::grid.draw(ggptx3)
dev.off()




## eBird trend


trends = read.csv("trends.csv")
trends = trends %>% filter(timegroups >= 2015 & timegroups <= 2022)

species = c("Great Hornbill","Wreathed Hornbill","Oriental Pied-Hornbill")

temp = trends %>% 
  mutate(lci_std = lci_std_recent,mean_std = mean_std_recent,rci_std = rci_std_recent) %>%
  filter(COMMON.NAME %in% species)

t1 = temp[temp$timegroups == 2022,]
t1 = t1 %>% arrange(desc(mean_std))

temp$COMMON.NAME = factor(temp$COMMON.NAME, 
                          levels = order)


#loadfonts(device = "win")

cols = c("#869B27", "#31954E", "#E49B36", "#CC6666", "#78CAE0", "#A13E2B", "#EA5599", "#493F3D",
         "#B69AC9", "#CC6666", "#9999CC", "#000000", "#66CC99")
#cols = c("#41726c","#2d809b","#e0d27b","#8cc48c","#55bfaf")
tcol = "black"

ns = length(unique(temp$COMMON.NAME))


cols1 = cols[c(1:ns)]
bks1 = sort(unique(temp$COMMON.NAME))
lbs1 = sort(unique(temp$COMMON.NAME))

tg = c("2015", "2016", "2017", "2018", "2019", "2020", "2021", "2022")

temp$rci_std = temp$lci_std = temp$mean_std

maxci = temp$rci_std
minci = temp$lci_std

liml = min(minci[!is.na(minci)])
lm = liml
liml = plyr::round_any(liml,50,floor)

limu = max(maxci[!is.na(maxci)])
um = limu
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






######################### fix the one extra, important line

for (i in 1:length(t1$COMMON.NAME))
{
  if (t1$lci_std[i] <= 100 & t1$rci_std[i] >= 100)
    comp = 100
  
  if (t1$lci_std[i] > 100 & t1$lci_std[i] <= 125)
    comp = 125
  if (t1$lci_std[i] > 125 & t1$lci_std[i] <= 150)
    comp = 125
  if (t1$lci_std[i] > 150 & t1$lci_std[i] <= 200)
    comp = 150  
  if (t1$lci_std[i] > 200)
    comp = 200  
  
  if (t1$rci_std[i] < 100 & t1$rci_std[i] >= 75)
    comp = 75
  if (t1$rci_std[i] < 75 & t1$rci_std[i] >= 50)
    comp = 75
  if (t1$rci_std[i] < 50)
    comp = 50
  
  target.index = which(abs(ybreaks - comp) == min(abs(ybreaks - comp)))
  target.index = target.index[1]
  ybreaks[target.index] = comp
  
  ybreaksl[target.index] = paste("+",(ybreaks[target.index]-100),"%",sep="")
  if (ybreaks[target.index] <= 100)
    ybreaksl[target.index] = paste((ybreaks[target.index]-100),"%",sep="")
}

ybreaksl[ybreaks == 100] = ""

if (min(ybreaks) < lm)
  lm = min(ybreaks)
if (max(ybreaks) > um)
  um = max(ybreaks)






######################### get the x-axis right


x_tick_preBas = seq(2015, 2022) + 0.5

ggp = ggplot(temp, aes(x = timegroups, y = mean_std,col = COMMON.NAME)) +
  geom_line(linewidth = 2) +
  #geom_point(size = 3) +
  #ggtitle(sps) +
  geom_bracket(
    inherit.aes = FALSE, 
    xmin = c(seq(2015, 2021)) + 0.5, 
    xmax = c(seq(2016, 2022)) + 0.5,
    y.position = lm-0.01*range,
    bracket.shorten = 0.15,
    tip.length = 0.08,
    vjust = 3,
    label = tg[-1],
    label.size = 4) +
  scale_colour_manual(breaks = bks1, 
                      labels = lbs1,
                      values = cols1) +
  scale_fill_manual(breaks = bks1, 
                    labels = lbs1,
                    values = cols1) +
  scale_x_continuous(
    breaks = c(seq(2016, 2022), x_tick_preBas),
    labels = c(paste0(seq(2016, 2022)), rep(c(""), length(x_tick_preBas))),
    limits = c(2015.5, 2022.5)) +
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
  theme(legend.title = element_blank(), legend.text = element_text(size = 16),
        plot.title = element_text(face = 'italic', size = 20, hjust = 0.5, vjust = 0.5),
        legend.key = element_blank())+
  theme(text=element_text(family="Gill Sans MT")) +
  scale_y_continuous(expand=c(0,0),
                     breaks = c(ybreaks[1],ybreaks[2],ybreaks[3],ybreaks[4],ybreaks[5]),
                     labels = c(ybreaksl[1],ybreaksl[2],ybreaksl[3],
                                ybreaksl[4],ybreaksl[5]))+
  annotate("text", x = 2015.5, y = 100 + range*0.08, label = "2015 (CT)", 
           colour = "black", family="Gill Sans MT", size = 6)+
  annotate("text", x = 2015.5, y = 100 + range*0.03, label = "baseline", 
           colour = "black", family="Gill Sans MT", size = 6)+
  coord_cartesian(ylim = c(lm-0.1*range,um+0.1*range), clip="off")+
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
  guides(colour = guide_legend(nrow = 1))+
  theme(legend.position = "bottom")

ggpx4 = ggdraw(ggpx)

g1 = plot_grid(ggpx3,ggpx4,nrow=2,ncol=1,rel_widths = c(1/2, 1/2))

name = paste("trends_graphs/independent trends/",sps,"_","independent_soib_comparison.jpg",sep="")

jpeg(name, units="in", width=11, height=11, res=1000, bg="transparent")
grid::grid.draw(g1)
dev.off()









