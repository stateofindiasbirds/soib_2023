source('SoIB_v2 functions.R')
library(tidyverse)
library(ggdist)
library(ggridges)
library(ggpubr)
library(cowplot)
library(extrafont)

main = read.csv("trends_results/full_results/SoIB_main.csv")
trends = read.csv("trends_results/full_results/trends.csv")

qualifying.species = main$eBird.English.Name.2022[!main$SOIB.Long.Term.Status %in% 
                                                    c("eBird Data Indecisive","eBird Data Deficient") & 
                                                    main$Long.Term.Analysis == "X"]

trends = trends %>% filter(COMMON.NAME %in% qualifying.species) %>%
  filter(timegroups <= 2022)
main = main %>% filter(eBird.English.Name.2022 %in% qualifying.species)






## diet composite

sps = "Composite Diet Guilds"

temp = main
temp$Diet.Guild[temp$Diet.Guild == ""] = NA

groups = unique(temp$Diet.Guild)
ct = 0
for (i in groups)
{
  ct = ct+1
  specs = temp$eBird.English.Name.2022[temp$Diet.Guild == i]
  temp_trends = trends %>% filter(COMMON.NAME %in% specs)
  temp_trends = temp_trends %>%
    group_by(timegroups) %>% 
    reframe(mean_std = mean(mean_std),
            lci_std = mean(lci_std),
            rci_std = mean(rci_std))
  temp_trends$COMMON.NAME = i
  if (ct == 1)
    trend_group = temp_trends
  if (ct > 1)
    trend_group = rbind(trend_group,temp_trends)
}

temp = trend_group

t1 = temp[temp$timegroups == 2022,]
t1 = t1 %>% arrange(desc(mean_std))
order = t1$COMMON.NAME


temp$COMMON.NAME = factor(temp$COMMON.NAME, 
                          levels = order)


cols = c("#869B27", "#31954E", "#E49B36", "#CC6666", "#78CAE0", "#A13E2B", "#EA5599", "#493F3D",
                  "#B69AC9", "#CC6666", "#9999CC", "#000000", "#66CC99")
                  #cols = c("#41726c","#2d809b","#e0d27b","#8cc48c","#55bfaf")
tcol = "black"

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

# without CIs
t1$lci_std = t1$rci_std = t1$mean_std


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
  #geom_lineribbon(linewidth = 0.7, alpha = 0.7) +
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
  annotate("text", x = 1999.5, y = 100 + range*0.03, label = "Pre-2000 (LT) baseline", 
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

name1 = "trends_graphs/composites/dietguilds_composite_trend_SoIBv2.jpg"
#name2 = "dietguilds_composite_trend_SoIBv2.svg"

jpeg(name1, units="in", width=11, height=7, res=1000, bg="transparent")
grid::grid.draw(ggpx3)
dev.off()

#print(ggpx3)
#ggsave(file=name2, units="in", width=11, height=7)





## habitat composite

sps = "Composite Habitat Specializations"

temp = main
temp$Habitat.Specialization[temp$Habitat.Specialization == "Grassland"] = "Grassland & Scrub"
temp$Habitat.Specialization[temp$Habitat.Specialization == "Alpine & Cold Desert"] = "Open Habitat"
temp$Habitat.Specialization[temp$Habitat.Specialization == ""] = NA

groups = unique(temp$Habitat.Specialization)
ct = 0
for (i in groups)
{
  ct = ct+1
  specs = temp$eBird.English.Name.2022[temp$Habitat.Specialization == i]
  temp_trends = trends %>% filter(COMMON.NAME %in% specs)
  temp_trends = temp_trends %>%
    group_by(timegroups) %>% 
    reframe(mean_std = mean(mean_std),
            lci_std = mean(lci_std),
            rci_std = mean(rci_std))
  temp_trends$COMMON.NAME = i
  if (ct == 1)
    trend_group = temp_trends
  if (ct > 1)
    trend_group = rbind(trend_group,temp_trends)
}

temp = trend_group

t1 = temp[temp$timegroups == 2022,]
t1 = t1 %>% arrange(desc(mean_std))
order = t1$COMMON.NAME


temp$COMMON.NAME = factor(temp$COMMON.NAME, 
                          levels = order)


cols = c("#869B27", "#31954E", "#E49B36", "#CC6666", "#78CAE0", "#A13E2B", "#EA5599", "#493F3D",
         "#B69AC9", "#CC6666", "#9999CC", "#000000", "#66CC99")
#cols = c("#41726c","#2d809b","#e0d27b","#8cc48c","#55bfaf")
tcol = "black"

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

# without CIs
t1$lci_std = t1$rci_std = t1$mean_std


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
  #geom_lineribbon(linewidth = 0.7, alpha = 0.7) +
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
  annotate("text", x = 1999.5, y = 100 + range*0.03, label = "Pre-2000 (LT) baseline", 
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

name1 = "trends_graphs/composites/habitats_composite_trend_SoIBv2.jpg"
#name2 = "habitats_composite_trend_SoIBv2.svg"

jpeg(name1, units="in", width=11, height=7, res=1000, bg="transparent")
grid::grid.draw(ggpx3)
dev.off()

#print(ggpx3)
#ggsave(file=name2, units="in", width=11, height=7)






## endemic composite

sps = "Composite Endemic Regions"


temp = main
temp$Endemic.Region[temp$Endemic.Region == "Eastern Himalayas"] = "Himalayas"
temp$Endemic.Region[temp$Endemic.Region == "Western Himalayas"] = "Himalayas"
temp$Endemic.Region[temp$Endemic.Region == "Western Ghats"] = "Western Ghats & Sri Lanka"
temp$Endemic.Region[temp$Endemic.Region == "Mainland India"] = "Indian Subcontinent"
temp$Endemic.Region[temp$Endemic.Region == "Andaman and Nicobar Islands"] = NA
temp$Endemic.Region[temp$Endemic.Region == ""] = NA

groups = unique(temp$Endemic.Region)
ct = 0
for (i in groups)
{
  ct = ct+1
  specs = temp$eBird.English.Name.2022[temp$Endemic.Region == i]
  temp_trends = trends %>% filter(COMMON.NAME %in% specs)
  temp_trends = temp_trends %>%
    group_by(timegroups) %>% 
    reframe(mean_std = mean(mean_std),
            lci_std = mean(lci_std),
            rci_std = mean(rci_std))
  temp_trends$COMMON.NAME = i
  if (ct == 1)
    trend_group = temp_trends
  if (ct > 1)
    trend_group = rbind(trend_group,temp_trends)
}

temp = trend_group

t1 = temp[temp$timegroups == 2022,]
t1 = t1 %>% arrange(desc(mean_std))
order = t1$COMMON.NAME


temp$COMMON.NAME = factor(temp$COMMON.NAME, 
                          levels = order)


cols = c("#869B27", "#31954E", "#E49B36", "#CC6666", "#78CAE0", "#A13E2B", "#EA5599", "#493F3D",
         "#B69AC9", "#CC6666", "#9999CC", "#000000", "#66CC99")
#cols = c("#41726c","#2d809b","#e0d27b","#8cc48c","#55bfaf")
tcol = "black"

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

# without CIs
t1$lci_std = t1$rci_std = t1$mean_std


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
  #geom_lineribbon(linewidth = 0.7, alpha = 0.7) +
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
  annotate("text", x = 1999.5, y = 100 + range*0.03, label = "Pre-2000 (LT) baseline", 
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

name1 = "trends_graphs/composites/endemics_composite_trend_SoIBv2.jpg"
#name2 = "endemics_composite_trend_SoIBv2.svg"

jpeg(name1, units="in", width=11, height=7, res=1000, bg="transparent")
grid::grid.draw(ggpx3)
dev.off()

#print(ggpx3)
#ggsave(file=name2, units="in", width=11, height=7)








## migrants composite

sps = "Composite Migratory Behaviours"


temp = main
temp$Migratory.Status.Within.India[temp$Migratory.Status.Within.India == "Altitudinal Migrant"] = "Resident"
temp$Migratory.Status.Within.India[temp$Migratory.Status.Within.India == "Resident & Altitudinal Migrant"] = "Resident"
temp$Migratory.Status.Within.India[temp$Migratory.Status.Within.India == "Resident & Local Migrant"] = "Short-distance Migrant"
temp$Migratory.Status.Within.India[temp$Migratory.Status.Within.India == "Resident & Summer Migrant"] = "Short-distance Migrant"
temp$Migratory.Status.Within.India[temp$Migratory.Status.Within.India == "Resident & Winter Migrant"] = "Short-distance Migrant"
temp$Migratory.Status.Within.India[temp$Migratory.Status.Within.India == "Resident & Within-India Migrant"] = "Short-distance Migrant"
temp$Migratory.Status.Within.India[temp$Migratory.Status.Within.India == "Summer Migrant"] = "Long-distance Migrant"
temp$Migratory.Status.Within.India[temp$Migratory.Status.Within.India == "Summer Migrant & Localized Winter Migrant"] = "Long-distance Migrant"
temp$Migratory.Status.Within.India[temp$Migratory.Status.Within.India == "Summer Migrant & Passage Migrant"] = "Long-distance Migrant"
temp$Migratory.Status.Within.India[temp$Migratory.Status.Within.India == "Within-India Migrant & Winter Migrant"] = "Short-distance Migrant"
temp$Migratory.Status.Within.India[temp$Migratory.Status.Within.India == "Winter Migrant"] = "Long-distance Migrant"
temp$Migratory.Status.Within.India[temp$Migratory.Status.Within.India == "Winter Migrant & Localized Summer Migrant"] = "Long-distance Migrant"
temp$Migratory.Status.Within.India[temp$Migratory.Status.Within.India == "Within-India Migrant"] = "Short-distance Migrant"
temp$Migratory.Status.Within.India[temp$Migratory.Status.Within.India == "Within-India Migrant & Winter Migrant"] = "Short-distance Migrant"
temp$Migratory.Status.Within.India[temp$Migratory.Status.Within.India == "Whithin-India Migrant & Winter Migrant"] = "Short-distance Migrant"
temp$Migratory.Status.Within.India[temp$Migratory.Status.Within.India == "Uncertain"] = NA
temp$Migratory.Status.Within.India[temp$Migratory.Status.Within.India == ""] = NA

groups = unique(temp$Migratory.Status.Within.India)
ct = 0
for (i in groups)
{
  ct = ct+1
  specs = temp$eBird.English.Name.2022[temp$Migratory.Status.Within.India == i]
  temp_trends = trends %>% filter(COMMON.NAME %in% specs)
  temp_trends = temp_trends %>%
    group_by(timegroups) %>% 
    reframe(mean_std = mean(mean_std),
            lci_std = mean(lci_std),
            rci_std = mean(rci_std))
  temp_trends$COMMON.NAME = i
  if (ct == 1)
    trend_group = temp_trends
  if (ct > 1)
    trend_group = rbind(trend_group,temp_trends)
}

temp = trend_group

t1 = temp[temp$timegroups == 2022,]
t1 = t1 %>% arrange(desc(mean_std))
order = t1$COMMON.NAME


temp$COMMON.NAME = factor(temp$COMMON.NAME, 
                          levels = order)


cols = c("#869B27", "#31954E", "#E49B36", "#CC6666", "#78CAE0", "#A13E2B", "#EA5599", "#493F3D",
         "#B69AC9", "#CC6666", "#9999CC", "#000000", "#66CC99")
#cols = c("#41726c","#2d809b","#e0d27b","#8cc48c","#55bfaf")
tcol = "black"

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

# without CIs
t1$lci_std = t1$rci_std = t1$mean_std


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
  #geom_lineribbon(linewidth = 0.7, alpha = 0.7) +
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
  annotate("text", x = 1999.5, y = 100 + range*0.03, label = "Pre-2000 (LT) baseline", 
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

name1 = "trends_graphs/composites/migrants_composite_trend_SoIBv2.jpg"
#name2 = "migrants_composite_trend_SoIBv2.svg"

jpeg(name1, units="in", width=11, height=7, res=1000, bg="transparent")
grid::grid.draw(ggpx3)
dev.off()

#print(ggpx3)
#ggsave(file=name2, units="in", width=11, height=7)







## shorebirds composite


sps = "Composite Shorebird Migratory Behaviours"


r.shore = c("Indian Thick-knee","Great Thick-knee","Beach Thick-knee","Black-winged Stilt","Red-wattled Lapwing",
            "Little Ringed Plover","Greater Painted-Snipe","Pheasant-tailed Jacana","Bronze-winged Jacana",
            "Solitary Snipe","Barred Buttonquail","Indian Courser","Jerdon's Courser","Small Pratincole")
s.shore = c("Pied Avocet","Ibisbill","Eurasian Oystercatcher","Northern Lapwing","Gray-headed Lapwing",
            "Sociable Lapwing","White-tailed Lapwing","Lesser Sand-Plover","Greater Sand-Plover","Caspian Plover",
            "Kentish Plover","Common Ringed Plover","Long-billed Plover","Oriental Plover","Eurasian Curlew",
            "Black-tailed Godwit","Ruff","Jack Snipe","Eurasian Woodcock","Wood Snipe","Great Snipe","Common Snipe",
            "Pin-tailed Snipe","Swinhoe's Snipe","Common Sandpiper","Green Sandpiper","Spotted Redshank",
            "Common Greenshank","Nordmann's Greenshank","Marsh Sandpiper","Wood Sandpiper","Common Redshank",
            "Small Buttonquail","Yellow-legged Buttonquail","Crab-Plover","Cream-colored Courser",
            "Collared Pratincole","Oriental Pratincole")
a.shore = c("Black-bellied Plover","European Golden-Plover","American Golden-Plover","Pacific Golden-Plover",
            "Whimbrel","Bar-tailed Godwit","Ruddy Turnstone","Great Knot","Red Knot","Broad-billed Sandpiper",
            "Sharp-tailed Sandpiper","Curlew Sandpiper","Temminck's Stint","Long-toed Stint","Spoon-billed Sandpiper",
            "Red-necked Stint","Sanderling","Dunlin","Little Stint","Buff-breasted Sandpiper","Pectoral Sandpiper",
            "Asian Dowitcher","Long-billed Dowitcher","Terek Sandpiper","Red-necked Phalarope","Red Phalarope",
            "Gray-tailed Tattler")

temp = main
temp$Shorebird = NA
temp$Shorebird[temp$eBird.English.Name.2022 %in% r.shore] = "Near Resident"
temp$Shorebird[temp$eBird.English.Name.2022 %in% s.shore] = "Palearctic Migrant"
temp$Shorebird[temp$eBird.English.Name.2022 %in% a.shore] = "Arctic Migrant"


groups = unique(temp$Shorebird)
ct = 0
for (i in groups)
{
  ct = ct+1
  specs = temp$eBird.English.Name.2022[temp$Shorebird == i]
  temp_trends = trends %>% filter(COMMON.NAME %in% specs)
  temp_trends = temp_trends %>%
    group_by(timegroups) %>% 
    reframe(mean_std = mean(mean_std),
            lci_std = mean(lci_std),
            rci_std = mean(rci_std))
  temp_trends$COMMON.NAME = i
  if (ct == 1)
    trend_group = temp_trends
  if (ct > 1)
    trend_group = rbind(trend_group,temp_trends)
}

temp = trend_group

t1 = temp[temp$timegroups == 2022,]
t1 = t1 %>% arrange(desc(mean_std))
order = t1$COMMON.NAME


temp$COMMON.NAME = factor(temp$COMMON.NAME, 
                          levels = order)


cols = c("#869B27", "#31954E", "#E49B36", "#CC6666", "#78CAE0", "#A13E2B", "#EA5599", "#493F3D",
         "#B69AC9", "#CC6666", "#9999CC", "#000000", "#66CC99")
#cols = c("#41726c","#2d809b","#e0d27b","#8cc48c","#55bfaf")
tcol = "black"

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

# without CIs
t1$lci_std = t1$rci_std = t1$mean_std



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
  #geom_lineribbon(linewidth = 0.7, alpha = 0.7) +
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
  annotate("text", x = 1999.5, y = 100 + range*0.03, label = "Pre-2000 (LT) baseline", 
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

name1 = "trends_graphs/composites/shorebirds_composite_trend_SoIBv2.jpg"
#name2 = "shorebirds_composite_trend_SoIBv2.svg"

jpeg(name1, units="in", width=11, height=7, res=1000, bg="transparent")
grid::grid.draw(ggpx3)
dev.off()

#print(ggpx3)
#ggsave(file=name2, units="in", width=11, height=7)







## raptors habitat composite

sps = "Composite Raptor Habitat Specializations"


temp = main[main$Order == "Accipitriformes" | main$Order == "Falconiformes",]
temp$Habitat.Specialization[temp$Habitat.Specialization == "Grassland"] = "Grassland & Scrub"
temp$Habitat.Specialization[temp$Habitat.Specialization == "Grassland & Scrub"] = "Open Habitat"
temp$Habitat.Specialization[temp$Habitat.Specialization == "Wetland"] = "Open Habitat"
temp$Habitat.Specialization[temp$Habitat.Specialization == "Alpine & Cold Desert"] = "Open Habitat"
temp$Habitat.Specialization[temp$Habitat.Specialization == "Forest"] = "Forest & Plantation"
temp$Habitat.Specialization[temp$Habitat.Specialization == ""] = NA

groups = unique(temp$Habitat.Specialization)
ct = 0
for (i in groups)
{
  ct = ct+1
  specs = temp$eBird.English.Name.2022[temp$Habitat.Specialization == i]
  temp_trends = trends %>% filter(COMMON.NAME %in% specs)
  temp_trends = temp_trends %>%
    group_by(timegroups) %>% 
    reframe(mean_std = mean(mean_std),
            lci_std = mean(lci_std),
            rci_std = mean(rci_std))
  temp_trends$COMMON.NAME = i
  if (ct == 1)
    trend_group = temp_trends
  if (ct > 1)
    trend_group = rbind(trend_group,temp_trends)
}

temp = trend_group

t1 = temp[temp$timegroups == 2022,]
t1 = t1 %>% arrange(desc(mean_std))
order = t1$COMMON.NAME


temp$COMMON.NAME = factor(temp$COMMON.NAME, 
                          levels = order)


cols = c("#869B27", "#31954E", "#E49B36", "#CC6666", "#78CAE0", "#A13E2B", "#EA5599", "#493F3D",
         "#B69AC9", "#CC6666", "#9999CC", "#000000", "#66CC99")
#cols = c("#41726c","#2d809b","#e0d27b","#8cc48c","#55bfaf")
tcol = "black"

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

# without CIs
t1$lci_std = t1$rci_std = t1$mean_std


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
  #geom_lineribbon(linewidth = 0.7, alpha = 0.7) +
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
  annotate("text", x = 1999.5, y = 100 + range*0.03, label = "Pre-2000 (LT) baseline", 
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

name1 = "trends_graphs/composites/raptors_composite_trend_SoIBv2.jpg"
#name2 = "raptors_composite_trend_SoIBv2.svg"

jpeg(name1, units="in", width=11, height=7, res=1000, bg="transparent")
grid::grid.draw(ggpx3)
dev.off()

#print(ggpx3)
#ggsave(file=name2, units="in", width=11, height=7)






## woodland mask composite

sps = "Composite Woodland"


main = read.csv("trends_results/full_results/SoIB_main.csv")
main$Mask = "All habitats"
trends = read.csv("trends_results/full_results/trends.csv")
trends$Mask = "All habitats"

main.woodland = read.csv("trends_results/mask_woodland/SoIB_main_mask_woodland.csv")
main.woodland$Mask = "Woodland"
trends.woodland = read.csv("trends_results/mask_woodland/trends_mask_woodland.csv")
trends.woodland$Mask = "Woodland"

qualifying.species = main$eBird.English.Name.2022[!main$SOIB.Long.Term.Status %in% 
                                                    c("eBird Data Indecisive","eBird Data Deficient") & 
                                                    main$Long.Term.Analysis == "X"]
qualifying.species.woodland = main.woodland$eBird.English.Name.2022[!main.woodland$SOIB.Long.Term.Status %in% 
                                                    c("eBird Data Indecisive","eBird Data Deficient") & 
                                                    main.woodland$Long.Term.Analysis == "X"]
qualifying.species = intersect(qualifying.species,qualifying.species.woodland)

main = rbind(main,main.woodland)
main = main %>% filter(eBird.English.Name.2022 %in% qualifying.species)
main$eBird.English.Name.2022 = paste(main$eBird.English.Name.2022,main$Mask)


trends = rbind(trends,trends.woodland)
trends = trends %>% filter(COMMON.NAME %in% qualifying.species) %>%
  filter(timegroups <= 2022)
trends$COMMON.NAME = paste(trends$COMMON.NAME,trends$Mask)



temp = main


groups = unique(temp$Mask)
ct = 0
for (i in groups)
{
  ct = ct+1
  specs = temp$eBird.English.Name.2022[temp$Mask == i]
  temp_trends = trends %>% filter(COMMON.NAME %in% specs)
  temp_trends = temp_trends %>%
    group_by(timegroups) %>% 
    reframe(mean_std = mean(mean_std),
            lci_std = mean(lci_std),
            rci_std = mean(rci_std))
  temp_trends$COMMON.NAME = i
  if (ct == 1)
    trend_group = temp_trends
  if (ct > 1)
    trend_group = rbind(trend_group,temp_trends)
}

temp = trend_group


cols.masks = data.frame(Mask = c("All habitats","Woodland","PAs","Cropland","ONEs"),
                        cols = c("#A13E2B","#869B27","#66CC99","#B69AC9","#E49B36"))
cols.masks = cols.masks %>% filter(Mask %in% temp$COMMON.NAME)



t1 = temp[temp$timegroups == 2022,]
t1 = t1 %>% arrange(desc(mean_std))
order = t1$COMMON.NAME

cols.masks$Mask = factor(cols.masks$Mask, levels = order)
cols.masks = cols.masks %>% arrange(Mask)


temp$COMMON.NAME = factor(temp$COMMON.NAME, 
                          levels = order)


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

# without CIs
t1$lci_std = t1$rci_std = t1$mean_std


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
  #geom_lineribbon(linewidth = 0.7, alpha = 0.7) +
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
  annotate("text", x = 1999.5, y = 100 + range*0.03, label = "Pre-2000 (LT) baseline", 
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

name1 = "trends_graphs/composites/woodland_mask_composite_trend_SoIBv2.jpg"
#name2 = "woodland_mask_composite_trend_SoIBv2.svg"

jpeg(name1, units="in", width=11, height=7, res=1000, bg="transparent")
grid::grid.draw(ggpx3)
dev.off()

#print(ggpx3)
#ggsave(file=name2, units="in", width=11, height=7)








## protected area mask composite

sps = "Composite Protected Areas"


main = read.csv("trends_results/full_results/SoIB_main.csv")
main$Mask = "All areas"
trends = read.csv("trends_results/full_results/trends.csv")
trends$Mask = "All areas"

main.pa = read.csv("trends_results/mask_pa/SoIB_main_mask_pa.csv")
main.pa$Mask = "PAs"
trends.pa = read.csv("trends_results/mask_pa/trends_mask_pa.csv")
trends.pa$Mask = "PAs"

qualifying.species = main$eBird.English.Name.2022[!main$SOIB.Long.Term.Status %in% 
                                                    c("eBird Data Indecisive","eBird Data Deficient") & 
                                                    main$Long.Term.Analysis == "X"]
qualifying.species.pa = main.pa$eBird.English.Name.2022[!main.pa$SOIB.Long.Term.Status %in% 
                                                                      c("eBird Data Indecisive","eBird Data Deficient") & 
                                                                      main.pa$Long.Term.Analysis == "X"]
qualifying.species = intersect(qualifying.species,qualifying.species.pa)

main = rbind(main,main.pa)
main = main %>% filter(eBird.English.Name.2022 %in% qualifying.species)
main$eBird.English.Name.2022 = paste(main$eBird.English.Name.2022,main$Mask)


trends = rbind(trends,trends.pa)
trends = trends %>% filter(COMMON.NAME %in% qualifying.species) %>%
  filter(timegroups <= 2022)
trends$COMMON.NAME = paste(trends$COMMON.NAME,trends$Mask)



temp = main


groups = unique(temp$Mask)
ct = 0
for (i in groups)
{
  ct = ct+1
  specs = temp$eBird.English.Name.2022[temp$Mask == i]
  temp_trends = trends %>% filter(COMMON.NAME %in% specs)
  temp_trends = temp_trends %>%
    group_by(timegroups) %>% 
    reframe(mean_std = mean(mean_std),
            lci_std = mean(lci_std),
            rci_std = mean(rci_std))
  temp_trends$COMMON.NAME = i
  if (ct == 1)
    trend_group = temp_trends
  if (ct > 1)
    trend_group = rbind(trend_group,temp_trends)
}

temp = trend_group

cols.masks = data.frame(Mask = c("All areas","Woodland","PAs","Cropland","ONEs"),
                        cols = c("#A13E2B","#869B27","#66CC99","#B69AC9","#E49B36"))
cols.masks = cols.masks %>% filter(Mask %in% temp$COMMON.NAME)



t1 = temp[temp$timegroups == 2022,]
t1 = t1 %>% arrange(desc(mean_std))
order = t1$COMMON.NAME

cols.masks$Mask = factor(cols.masks$Mask, levels = order)
cols.masks = cols.masks %>% arrange(Mask)


temp$COMMON.NAME = factor(temp$COMMON.NAME, 
                          levels = order)


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

# without CIs
t1$lci_std = t1$rci_std = t1$mean_std


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
  #geom_lineribbon(linewidth = 0.7, alpha = 0.7) +
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
  annotate("text", x = 1999.5, y = 100 + range*0.03, label = "Pre-2000 (LT) baseline", 
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

name1 = "trends_graphs/composites/pa_mask_composite_trend_SoIBv2.jpg"
#name2 = "pa_mask_composite_trend_SoIBv2.svg"

jpeg(name1, units="in", width=11, height=7, res=1000, bg="transparent")
grid::grid.draw(ggpx3)
dev.off()

#print(ggpx3)
#ggsave(file=name2, units="in", width=11, height=7)







## cropland/one mask composite

sps = "Composite Open Natural Ecosystems & Croplands"


main = read.csv("trends_results/full_results/SoIB_main.csv")
main$Mask = "All habitats"
trends = read.csv("trends_results/full_results/trends.csv")
trends$Mask = "All habitats"

main.crop = read.csv("trends_results/mask_cropland/SoIB_main_mask_cropland.csv")
main.crop$Mask = "Cropland"
trends.crop = read.csv("trends_results/mask_cropland/trends_mask_cropland.csv")
trends.crop$Mask = "Cropland"

main.one = read.csv("trends_results/mask_one/SoIB_main_mask_oneland.csv")
main.one$Mask = "ONEs"
trends.one = read.csv("trends_results/mask_one/trends_mask_oneland.csv")
trends.one$Mask = "ONEs"

qualifying.species = main$eBird.English.Name.2022[!main$SOIB.Long.Term.Status %in% 
                                                    c("eBird Data Indecisive","eBird Data Deficient") & 
                                                    main$Long.Term.Analysis == "X"]
qualifying.species.crop = main.crop$eBird.English.Name.2022[!main.crop$SOIB.Long.Term.Status %in% 
                                                          c("eBird Data Indecisive","eBird Data Deficient") & 
                                                          main.crop$Long.Term.Analysis == "X"]
qualifying.species.one = main.one$eBird.English.Name.2022[!main.one$SOIB.Long.Term.Status %in% 
                                                             c("eBird Data Indecisive","eBird Data Deficient") & 
                                                             main.one$Long.Term.Analysis == "X"]
qualifying.species = intersect(qualifying.species,qualifying.species.crop)
qualifying.species = intersect(qualifying.species,qualifying.species.one)

main = rbind(main,main.crop,main.one)
main = main %>% filter(eBird.English.Name.2022 %in% qualifying.species)
main$eBird.English.Name.2022 = paste(main$eBird.English.Name.2022,main$Mask)


trends = rbind(trends,trends.crop,trends.one)
trends = trends %>% filter(COMMON.NAME %in% qualifying.species) %>%
  filter(timegroups <= 2022)
trends$COMMON.NAME = paste(trends$COMMON.NAME,trends$Mask)



temp = main


groups = unique(temp$Mask)
ct = 0
for (i in groups)
{
  ct = ct+1
  specs = temp$eBird.English.Name.2022[temp$Mask == i]
  temp_trends = trends %>% filter(COMMON.NAME %in% specs)
  temp_trends = temp_trends %>%
    group_by(timegroups) %>% 
    reframe(mean_std = mean(mean_std),
            lci_std = mean(lci_std),
            rci_std = mean(rci_std))
  temp_trends$COMMON.NAME = i
  if (ct == 1)
    trend_group = temp_trends
  if (ct > 1)
    trend_group = rbind(trend_group,temp_trends)
}

temp = trend_group

cols.masks = data.frame(Mask = c("All habitats","Woodland","PAs","Cropland","ONEs"),
                        cols = c("#A13E2B","#869B27","#66CC99","#B69AC9","#E49B36"))
cols.masks = cols.masks %>% filter(Mask %in% temp$COMMON.NAME)



t1 = temp[temp$timegroups == 2022,]
t1 = t1 %>% arrange(desc(mean_std))
order = t1$COMMON.NAME

cols.masks$Mask = factor(cols.masks$Mask, levels = order)
cols.masks = cols.masks %>% arrange(Mask)


temp$COMMON.NAME = factor(temp$COMMON.NAME, 
                          levels = order)


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

# without CIs
t1$lci_std = t1$mean_std
t1$rci_std = t1$mean_std


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
  #geom_lineribbon(linewidth = 0.7, alpha = 0.7) +
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
  annotate("text", x = 1999.5, y = 100 + range*0.03, label = "Pre-2000 (LT) baseline", 
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

name1 = "trends_graphs/composites/open_mask_composite_trend_SoIBv2.jpg"
#name2 = "open_mask_composite_trend_SoIBv2.svg"

jpeg(name1, units="in", width=11, height=7, res=1000, bg="transparent")
grid::grid.draw(ggpx3)
dev.off()

#print(ggpx3)
#ggsave(file=name2, units="in", width=11, height=7)
