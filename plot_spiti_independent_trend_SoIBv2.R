source('SoIB_v2 functions.R')
library(tidyverse)
library(ggdist)
library(ggridges)
library(ggpubr)
library(cowplot)
library(extrafont)


## independent density

indtrends = read.csv("data_independent/spiti_forplot_spec.csv")
selectspecies = unique(indtrends$species)

tg = as.character(indtrends$timegroups)
indtrends = indtrends %>% filter(!is.na(mean), cir < 1)

for (i in 1:length(selectspecies))
{
  sps = selectspecies[i]
  temp = indtrends %>% filter(species %in% sps)
  
  scol = "#869B27"
  
  #loadfonts(device = "win")
  
  range = max(temp$cir[!is.na(temp$cir)])-min(temp$cil[!is.na(temp$cil)])
  
  liml = min(temp$cil[!is.na(temp$cil)])-0.1*range
  if (liml < 0)
    liml = 0
  lm = liml
  limu = max(temp$cir[!is.na(temp$cir)])+0.1*range
  um = limu
  
  ybreaks = seq(liml,limu,length.out=5)
  ybreaksl = round(ybreaks,2)
  
  
  ######################### get the x-axis right
  
  
  x_tick_preBas = seq(2001, 2022) + 0.5
  
  ggpt = ggplot(temp, aes(x = timegroups, y = mean)) +
    geom_line(linewidth = 0.7, col = scol) +
    geom_errorbar(aes(ymin = cil, ymax = cir), linewidth = 0.5, width = 0.2, col = scol) +
    #geom_point(size = 3) +
    ggtitle(sps) +
    geom_bracket(
      inherit.aes = FALSE, 
      xmin = c(seq(2001, 2021)) + 0.5, 
      xmax = c(seq(2002, 2022)) + 0.5,
      y.position = lm-0.01*range,
      bracket.shorten = 0.15,
      tip.length = 0.04,
      vjust = 3,
      label = tg,
      label.size = 3) +
    scale_x_continuous(
      breaks = c(seq(2002, 2022), x_tick_preBas),
      labels = c(paste0(seq(2002, 2022)), rep(c(""), length(x_tick_preBas))),
      limits = c(2001.5, 2022.5)) +
    geom_hline(yintercept = ybreaks[1], linetype = "dotted", linewidth = 0.7) +
    geom_hline(yintercept = ybreaks[2], linetype = "dotted", linewidth = 0.7) +
    geom_hline(yintercept = ybreaks[3], linetype = "dotted", linewidth = 0.7) +
    geom_hline(yintercept = ybreaks[4], linetype = "dotted", linewidth = 0.7) +
    geom_hline(yintercept = ybreaks[5], linetype = "dotted", linewidth = 0.7) +
    #geom_hline(yintercept = 100, linetype = "solid", linewidth = 0.9) +
    xlab("time-steps") +
    ylab("density in Spiti")
  
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
  
  ggptx3 = ggdraw(ggptx)
  
  name = paste("trends_graphs/independent trends/spiti monitoring/",sps,".jpg",sep="")
  jpeg(name, units="in", width=11, height=7, res=1000, bg="transparent")
  grid::grid.draw(ggptx3)
  dev.off()
}




