source('00_scripts/00_functions.R')
library(tidyverse)
library(ggdist)
library(ggridges)
library(ggpubr)
library(ggrepel)
library(cowplot)
library(extrafont)
library(stringr)


main = read.csv("01_analyses_full/results/SoIB_main.csv")
trends = read.csv("01_analyses_full/results/trends.csv")
qualifying.species = main$eBird.English.Name.2022[!main$SOIBv2.Long.Term.Status %in% 
                                                    c("eBird Data Inconclusive","eBird Data Deficient") & 
                                                    main$Long.Term.Analysis == "X"]
trends = trends %>% filter(COMMON.NAME %in% qualifying.species) %>%
  filter(timegroups <= 2022)

cols = c("#718868", "#436b74", "#827f87", "#a26352")

cols = c("#869B27", "#E49B36", "#436b74", "#CC6666", "#B69AC9", "#78CAE0","#31954E","#493F3D",
         "#EA5599", "#9999CC", "#A13E2B", "#66CC99")




species = c("Black-rumped Flameback","White-naped Woodpecker","White-bellied Woodpecker",
            "Yellow-crowned Woodpecker","Brown-capped Pygmy Woodpecker")
#cols = c("#869B27", "#31954E", "#78CAE0", "#CC6666", "#E49B36", "#9999CC", "#493F3D",
#         "#B69AC9", "#A13E2B", "#EA5599", "#000000", "#66CC99")
species = c("Northern Pintail","Northern Shoveler","Indian Spot-billed Duck",
            "Cotton Pygmy-Goose")
species = c("Spot-billed Pelican","Black-headed Ibis","Glossy Ibis","Painted Stork",
            "Eurasian Spoonbill","Great Cormorant")
#cols = c("#869B27", "#31954E", "#CC6666", "#78CAE0", "#9999CC", "#493F3D",
#         "#B69AC9", "#A13E2B", "#EA5599", "#000000", "#66CC99")
species = c("Large-billed Crow","Olive-backed Pipit","Greater Coucal")
species = c("White-rumped Vulture","Indian Vulture","Red-headed Vulture",
            "Egyptian Vulture","Eurasian Griffon")
#cols = c("#869B27", "#78CAE0", "#CC6666", "#E49B36", "#31954E", "#9999CC", "#493F3D",
#         "#B69AC9", "#A13E2B", "#EA5599", "#000000", "#66CC99")
species = c("Isabelline Wheatear","Great Gray Shrike","Rufous-tailed Lark",
            "Yellow-billed Babbler","Eurasian Kestrel","Jerdon's Bushlark")
species = c("Little Ringed Plover","Little Tern","Great Thick-knee","Small Pratincole")
species = c("Lesser Sand-Plover","Terek Sandpiper","Whimbrel","Curlew Sandpiper",
            "Eurasian Curlew")
species = c("Indian Gray Hornbill","Oriental Pied-Hornbill")
species = c("Black-rumped Flameback","Lesser Yellownape","White-bellied Woodpecker",
            "Yellow-crowned Woodpecker","Brown-capped Pygmy Woodpecker",
            "Himalayan Woodpecker")
species = c("Northern Shoveler","Garganey","Cotton Pygmy-Goose",
            "Common Merganser","Ruddy Shelduck",
            "Tufted Duck")
#cols = c("#869B27", "#31954E", "#78CAE0", "#CC6666", "#E49B36", "#9999CC", "#493F3D",
#         "#B69AC9", "#A13E2B", "#EA5599", "#000000", "#66CC99")
species = c("Ashy Prinia","Indian Peafowl","Indian Courser","Red-necked Falcon")
species = c("Ashy Prinia","Rock Pigeon","Indian Peafowl","Asian Koel")
species = c("Spot-billed Pelican","Black-headed Ibis","Purple Heron","Painted Stork",
            "Eurasian Spoonbill","Glossy Ibis")
cols = c("#31954E", "#869B27", "#9999CC", "#CC6666", "#78CAE0", "#E49B36",
         "#B69AC9", "#A13E2B", "#493F3D", "#000000", "#66CC99")
species = c("Oriental Honey-buzzard","Black Kite","Eurasian Marsh-Harrier",
            "Short-toed Snake-Eagle","Pallid Harrier","Greater Spotted Eagle")
cols = c("#869B27", "#E49B36", "#436b74", "#CC6666", "#B69AC9", "#78CAE0","#31954E","#493F3D",
         "#EA5599", "#9999CC", "#A13E2B", "#66CC99")


sps = "Raptors"

temp = trends %>% 
  filter(COMMON.NAME %in% species)

t1 = temp[temp$timegroups == 2022,]
t1 = t1 %>% arrange(desc(mean_std))
order = t1$COMMON.NAME


temp$COMMON.NAME = factor(temp$COMMON.NAME, 
                          levels = order)


#loadfonts(device = "win")


#cols = c("#41726c","#2d809b","#e0d27b","#8cc48c","#55bfaf")
tcol = "#56697B"
pcol = "#A13E2B"

ns = length(unique(temp$COMMON.NAME))


cols1 = cols[c(1:ns)]
bks1 = sort(unique(temp$COMMON.NAME))
lbs1 = sort(unique(temp$COMMON.NAME))

tg = c("before 2000", "2000-2006", "2007-2010", "2011-2012", "2013", "2014", "2015", 
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

temp$COMMON.NAMEx[temp$timegroupsf != "2000-2006"] = ""

tlow = temp %>%
  select(COMMON.NAME,timegroups) %>%
  arrange(COMMON.NAME,timegroups) %>%
  group_by(COMMON.NAME) %>% slice(2) %>% ungroup()

tlow = max(tlow$timegroups)

ggp = ggplot(temp, aes(x = timegroups, y = mean_std, col = COMMON.NAME, label = COMMON.NAMEx)) +
  geom_line(linewidth = 1.5, lineend = "round") +
  geom_text_repel(nudge_x = -2, direction = "y", hjust = "center", size = 4, 
                  family = "Gandhi Sans", min.segment.length = Inf) +
  #geom_point(size = 3) +
  #ggtitle(sps) +
  geom_bracket(
    inherit.aes = FALSE, 
    xmin = c(2000, 2006, 2010, 2012, seq(2013, 2021)) + 0.5, 
    xmax = c(2006, 2010, 2012, seq(2013, 2022)) + 0.5,
    y.position = lm-0.01*range,
    bracket.shorten = 0.15,
    tip.length = 0,
    vjust = 2.5,
    label = tg[-1],
    label.size = 3.8,
    col = tcol,
    family = "Gandhi Sans") +
  geom_bracket(
    inherit.aes = FALSE, 
    xmin = c(2015) - 0.5, 
    xmax = c(2022) + 0.5,
    y.position = lm-0.07*range,
    bracket.shorten = 0.15,
    tip.length = -0.017,
    vjust = 2.8,
    label = "Current Trend",
    label.size = 5,
    col = tcol,
    fontface = 3,
    family = "Gandhi Sans") +
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
    limits = c(1999.5, 2023.5)) +
  geom_segment(x = tlow, y = ybreaks[1], xend = 2022, yend = ybreaks[1], linetype = "dotted", linewidth = 0.7, col = tcol) +
  geom_segment(x = tlow, y = ybreaks[2], xend = 2022, yend = ybreaks[2], linetype = "dotted", linewidth = 0.7, col = tcol) +
  geom_segment(x = tlow, y = ybreaks[3], xend = 2022, yend = ybreaks[3], linetype = "dotted", linewidth = 0.7, col = tcol) +
  geom_segment(x = tlow, y = ybreaks[4], xend = 2022, yend = ybreaks[4], linetype = "dotted", linewidth = 0.7, col = tcol) +
  geom_segment(x = tlow, y = ybreaks[5], xend = 2022, yend = ybreaks[5], linetype = "dotted", linewidth = 0.7, col = tcol) +
  geom_segment(x = tlow, y = 100, xend = 2022, yend = 100, linetype = "solid", linewidth = 0.9, col = tcol) +
  xlab("Time-steps") +
  ylab("Change in Abundance Index")

ggpx = ggp +
  theme(text=element_text(family="Gandhi Sans")) +
  scale_y_continuous(expand=c(0,0),
                     breaks = c(ybreaks[1],ybreaks[2],ybreaks[3],ybreaks[4],ybreaks[5]),
                     labels = c(ybreaksl[1],ybreaksl[2],ybreaksl[3],
                                ybreaksl[4],ybreaksl[5]),
                     position = "left")+
  annotate("text", x = 2023.5, y = ybreaks[1], label = ybreaksl[1], 
           colour = tcol, family="Gandhi Sans", size = 6)+
  annotate("text", x = 2023.5, y = ybreaks[2], label = ybreaksl[2], 
           colour = tcol, family="Gandhi Sans", size = 6)+
  annotate("text", x = 2023.5, y = ybreaks[3], label = ybreaksl[3], 
           colour = tcol, family="Gandhi Sans", size = 6)+
  annotate("text", x = 2023.5, y = ybreaks[4], label = ybreaksl[4], 
           colour = tcol, family="Gandhi Sans", size = 6)+
  annotate("text", x = 2023.5, y = ybreaks[5], label = ybreaksl[5], 
           colour = tcol, family="Gandhi Sans", size = 6)+
  annotate("text", x = 2023.5, y = 100, label = "Pre-2000\nbaseline", 
           colour = tcol, family="Gandhi Sans", fontface = 3, size = 5)+
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
  theme(axis.title.x = element_blank(), 
        axis.title.y = element_text(size = 22, colour = tcol,
                                    margin = margin(0, -0.6, 0, 0.4, 'cm')), 
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())+
  guides(colour = "none")




name1 = paste("02_graphs/long-term trends - multiple species/",sps,"_","multiple_species_eBird_trend_SoIBv2.png",sep="")

ggsave(name1,ggpx,units="in", width=11, height=7.5, dpi=1000, bg="transparent")
