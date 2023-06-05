source('00_scripts/SoIBv2_functions.R')
library(tidyverse)
library(ggdist)
library(ggridges)
library(ggpubr)
library(ggrepel)
library(cowplot)
library(extrafont)
library(stringr)


## independent (encounter rate - detections per km)

indtrends = read.csv("10_sys-mon/data/hornbills_papumrf.csv")
sps = "Hornbills in Papum RF"

temp = indtrends

t1 = temp[temp$timegroups == 2022,]
t1 = t1 %>% arrange(desc(mean))
order = t1$COMMON.NAME


temp$COMMON.NAME = factor(temp$COMMON.NAME, 
                      levels = order)

#loadfonts(device = "win")

cols = c("#869B27", "#31954E", "#E49B36", "#CC6666", "#78CAE0", "#9999CC", "#493F3D",
         "#B69AC9", "#A13E2B", "#EA5599", "#000000", "#66CC99")
#cols = c("#41726c","#2d809b","#e0d27b","#8cc48c","#55bfaf")
tcol = "black"
pcol = "#A13E2B"

ns = length(unique(temp$COMMON.NAME))


cols1 = cols[c(1:ns)]
bks1 = sort(unique(temp$COMMON.NAME))
lbs1 = sort(unique(temp$COMMON.NAME))

tg = c("2015", "2016", "2017", "2018", "2019", "2020", "2021", "2022")

#loadfonts(device = "win")

range = max(temp$cir)-min(temp$cil)

liml = min(temp$cil)-0.1*range
if (liml < 0)
  liml = 0
lm = liml
limu = max(temp$cir)+0.1*range
um = limu

ybreaks = round(seq(liml,limu,length.out=5),1)
ybreaksl = round(ybreaks,1)


######################### get the x-axis right


x_tick_preBas = seq(2015, 2022) + 0.5

temp$COMMON.NAMEy = as.character(temp$COMMON.NAME)
temp$COMMON.NAMEz = as.character(temp$COMMON.NAME)
temp$COMMON.NAMEx = ""


for (k in 1:length(temp$COMMON.NAME))
{
  
  temp$COMMON.NAMEx[k] = as.character(temp$COMMON.NAME[k])
  
  if (nchar(temp$COMMON.NAMEy[k]) > 18)
  {
    temp$COMMON.NAMEy[k] = word(temp$COMMON.NAME[k],1,-2)
    temp$COMMON.NAMEz[k] = word(temp$COMMON.NAME[k],-1)
    
    if (nchar(temp$COMMON.NAMEy[k]) > 18)
    {
      temp$COMMON.NAMEy[k] = word(temp$COMMON.NAME[k],1,-3)
      temp$COMMON.NAMEz[k] = word(temp$COMMON.NAME[k],-2,-1)
    }
    
    temp$COMMON.NAMEx[k] = paste(temp$COMMON.NAMEy[k],"\n",temp$COMMON.NAMEz[k],sep="")
  }
}

temp$COMMON.NAMEx[temp$timegroups != "2016"] = ""

tlow = temp %>%
  select(COMMON.NAME,timegroups) %>%
  arrange(COMMON.NAME,timegroups) %>%
  group_by(COMMON.NAME) %>% slice(1) %>% ungroup()

tlow = max(tlow$timegroups)



pd = position_dodge(0.3)

ggp = ggplot(temp, aes(x = timegroups, y = mean, col = COMMON.NAME, label = COMMON.NAMEx)) +
  geom_line(linewidth = 2, position = pd) +
  geom_errorbar(aes(ymin = cil, ymax = cir), linewidth = 1, width = 0.2, position = pd) +
  #geom_point(size = 3) +
  geom_text_repel(nudge_x = -0.65, direction = "y", hjust = "center", size = 4, 
                  min.segment.length = Inf, fontface = c("bold")) +
  #geom_point(size = 3) +
  ggtitle(sps) +
  geom_bracket(
    inherit.aes = FALSE, 
    xmin = c(seq(2015, 2021)) + 0.5, 
    xmax = c(seq(2016, 2022)) + 0.5,
    y.position = lm-0.01*range,
    bracket.shorten = 0.15,
    tip.length = 0.0004,
    vjust = 2,
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
    limits = c(2015.09, 2022.7)) +
  geom_segment(x = tlow, y = ybreaks[1], xend = 2022, yend = ybreaks[1], linetype = "dotted", linewidth = 0.7, col = tcol) +
  geom_segment(x = tlow, y = ybreaks[2], xend = 2022, yend = ybreaks[2], linetype = "dotted", linewidth = 0.7, col = tcol) +
  geom_segment(x = tlow, y = ybreaks[3], xend = 2022, yend = ybreaks[3], linetype = "dotted", linewidth = 0.7, col = tcol) +
  geom_segment(x = tlow, y = ybreaks[4], xend = 2022, yend = ybreaks[4], linetype = "dotted", linewidth = 0.7, col = tcol) +
  geom_segment(x = tlow, y = ybreaks[5], xend = 2022, yend = ybreaks[5], linetype = "dotted", linewidth = 0.7, col = tcol) +
  geom_segment(x = tlow, y = 100, xend = 2022, yend = 100, linetype = "solid", linewidth = 0.9, col = tcol) +
  xlab("Time-steps") +
  ylab("Encounter rate (per km)")

ggpx = ggp +
  theme(axis.title.x = element_blank(), 
        axis.title.y = element_text(size = 22, colour = "#56697B",
                                    margin = margin(0, 0.6, 0, 0.4, 'cm')), 
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  theme(plot.title = element_text(face = 'bold', size = 20, hjust = 0.5, vjust = -2, colour = pcol))+
  theme(text=element_text(family="Gill Sans MT")) +
  scale_y_continuous(expand=c(0,0),
                     breaks = c(ybreaks[1],ybreaks[2],ybreaks[3],ybreaks[4],ybreaks[5]),
                     labels = c(ybreaksl[1],ybreaksl[2],ybreaksl[3],
                                ybreaksl[4],ybreaksl[5]),
                     position = "left")+
  annotate("text", x = 2022.7, y = ybreaks[1], label = ybreaksl[1], 
           colour = "#56697B", family="Gill Sans MT", size = 6)+
  annotate("text", x = 2022.7, y = ybreaks[2], label = ybreaksl[2], 
           colour = "#56697B", family="Gill Sans MT", size = 6)+
  annotate("text", x = 2022.7, y = ybreaks[3], label = ybreaksl[3], 
           colour = "#56697B", family="Gill Sans MT", size = 6)+
  annotate("text", x = 2022.7, y = ybreaks[4], label = ybreaksl[4], 
           colour = "#56697B", family="Gill Sans MT", size = 6)+
  annotate("text", x = 2022.7, y = ybreaks[5], label = ybreaksl[5], 
           colour = "#56697B", family="Gill Sans MT", size = 6)+
  annotate("text", x = 2022.7, y = 100, label = "2015\nbaseline", 
           colour = "black", family="Gill Sans MT", size = 5)+
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
  guides(colour = "none")

ggpx3 = ggdraw(ggpx)

name = paste("trends_graphs/independent trends/",sps,"_","independent.jpg",sep="")
jpeg(name, units="in", width=11, height=7, res=1000, bg="transparent")
grid::grid.draw(ggpx3)
dev.off()
