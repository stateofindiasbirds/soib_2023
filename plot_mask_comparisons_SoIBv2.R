## compare for a species across masks

source('SoIB_v2 functions.R')
library(tidyverse)
library(ggdist)
library(ggridges)
library(ggpubr)
library(cowplot)
library(extrafont)

sps = "Yellow-browed Bulbul"

main = read.csv("SoIB_main.csv")
main$Mask = "All areas"
trends = read.csv("trends.csv")
trends$Mask = "All areas"

main.woodland = read.csv("SoIB_main_mask_woodland.csv")
main.woodland$Mask = "Woodland"
trends.woodland = read.csv("trends_mask_woodland.csv")
trends.woodland$Mask = "Woodland"

main.pa = read.csv("SoIB_main_mask_pa.csv")
main.pa$Mask = "PAs"
trends.pa = read.csv("trends_mask_pa.csv")
trends.pa$Mask = "PAs"

main.crop = read.csv("SoIB_main_mask_cropland.csv")
main.crop$Mask = "Cropland"
trends.crop = read.csv("trends_mask_cropland.csv")
trends.crop$Mask = "Cropland"

main.one = read.csv("SoIB_main_mask_oneland.csv")
main.one$Mask = "ONEs"
trends.one = read.csv("trends_mask_oneland.csv")
trends.one$Mask = "ONEs"

qualifying.species = main$eBird.English.Name.2022[!main$SOIB.Long.Term.Status %in% 
                                                    c("eBird Data Indecisive","eBird Data Deficient") & 
                                                    main$Long.Term.Analysis == "X"]
qualifying.species.woodland = main.woodland$eBird.English.Name.2022[!main.woodland$SOIB.Long.Term.Status %in% 
                                                                      c("eBird Data Indecisive","eBird Data Deficient") & 
                                                                      main.woodland$Long.Term.Analysis == "X"]
qualifying.species.pa = main.pa$eBird.English.Name.2022[!main.pa$SOIB.Long.Term.Status %in% 
                                                          c("eBird Data Indecisive","eBird Data Deficient") & 
                                                          main.pa$Long.Term.Analysis == "X"]
qualifying.species.crop = main.crop$eBird.English.Name.2022[!main.crop$SOIB.Long.Term.Status %in% 
                                                              c("eBird Data Indecisive","eBird Data Deficient") & 
                                                              main.crop$Long.Term.Analysis == "X"]
qualifying.species.one = main.one$eBird.English.Name.2022[!main.one$SOIB.Long.Term.Status %in% 
                                                            c("eBird Data Indecisive","eBird Data Deficient") & 
                                                            main.one$Long.Term.Analysis == "X"]


trends = trends %>% filter(COMMON.NAME %in% qualifying.species) %>% filter(COMMON.NAME %in% sps)
trends.woodland = trends.woodland %>% filter(COMMON.NAME %in% qualifying.species.woodland)  %>% filter(COMMON.NAME %in% sps)
trends.pa = trends.pa %>% filter(COMMON.NAME %in% qualifying.species.pa)  %>% filter(COMMON.NAME %in% sps)
trends.crop = trends.crop %>% filter(COMMON.NAME %in% qualifying.species.crop)  %>% filter(COMMON.NAME %in% sps)
trends.one = trends.one %>% filter(COMMON.NAME %in% qualifying.species.one)  %>% filter(COMMON.NAME %in% sps)

trends = rbind(trends,trends.woodland,trends.pa,trends.crop,trends.one)

cols.masks = data.frame(Mask = c("All areas","Woodland","PAs","Cropland","ONEs"),
                        cols = c("#A13E2B","#869B27","#66CC99","#B69AC9","#E49B36"))




temp = trends %>% 
  filter(timegroups <= 2022)

temp$COMMON.NAME = temp$Mask

cols.masks = cols.masks %>% filter(Mask %in% temp$COMMON.NAME)


t1 = temp[temp$timegroups == 2022,]
t1 = t1 %>% arrange(desc(mean_std))
order = t1$COMMON.NAME

cols.masks$Mask = factor(cols.masks$Mask, levels = order)
cols.masks = cols.masks %>% arrange(Mask)


temp$COMMON.NAME = factor(temp$COMMON.NAME, 
                          levels = order)


#loadfonts(device = "win")

cols = c("#869B27", "#31954E", "#E49B36", "#CC6666", "#78CAE0", "#A13E2B", "#EA5599", "#493F3D",
         "#B69AC9", "#CC6666", "#9999CC", "#000000", "#66CC99")
#cols = c("#41726c","#2d809b","#e0d27b","#8cc48c","#55bfaf")
tcol = "black"


cols = cols.masks$cols

ns = length(unique(temp$COMMON.NAME))


cols1 = cols[c(1:ns)]
bks1 = sort(unique(temp$COMMON.NAME))
lbs1 = sort(unique(temp$COMMON.NAME))

tg = c("before 2000", "2000-2006", "2007-2010", "2011-2012", "2013", "2014", "CT", 
       "2016", "2017", "2018", "2019", "2020", "2021", "2022")

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

x_tick_pre2000Bas = seq(1999, 2022) + 0.5

ggp = ggplot(temp, aes(x = timegroups, y = mean_std,col = COMMON.NAME)) +
  geom_line(linewidth = 2) +
  #geom_point(size = 3) +
  ggtitle(sps) +
  geom_bracket(
    inherit.aes = FALSE, 
    xmin = c(2000, 2006, 2010, 2012, seq(2013, 2021)) + 0.5, 
    xmax = c(2006, 2010, 2012, seq(2013, 2022)) + 0.5,
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
    breaks = c(seq(1999, 2022), x_tick_pre2000Bas),
    labels = c("", "2000", "2001", rep(c(""), 2006-2000-2), 
               "2006", "2007", rep(c(""), 2010-2006-2), 
               "2010", "2011", rep(c(""), 2012-2010-2), 
               paste0(seq(2012, 2022)), rep(c(""), length(x_tick_pre2000Bas))),
    limits = c(1999.5, 2022.5)) +
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
  theme(legend.title = element_blank(), legend.text = element_text(size = 12),
        plot.title = element_text(face = 'italic', size = 20, hjust = 0.5, vjust = 0.5),
        legend.key = element_blank())+
  theme(text=element_text(family="Gill Sans MT")) +
  scale_y_continuous(expand=c(0,0),
                     breaks = c(ybreaks[1],ybreaks[2],ybreaks[3],ybreaks[4],ybreaks[5]),
                     labels = c(ybreaksl[1],ybreaksl[2],ybreaksl[3],
                                ybreaksl[4],ybreaksl[5]))+
  annotate("text", x = 1999.5, y = 100 + range*0.035, label = "Pre-2000 (LT) baseline", 
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
  guides(colour = guide_legend(nrow = 1))+
  theme(legend.position = "bottom")




ggpx3 = ggdraw(ggpx)

name1 = paste("trends_graphs/long-term trends - masks/",sps,"_","mask_comparison_eBird_trend_SoIBv2.jpg",sep="")

jpeg(name1, units="in", width=11, height=7, res=1000, bg="transparent")
grid::grid.draw(ggpx3)
dev.off()