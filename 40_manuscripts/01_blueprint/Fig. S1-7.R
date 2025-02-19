#### comparison of 50 species - supplementary material

load("40_manuscripts/01_blueprint/specieslists_2023.RData")
set.seed(20)
species = specieslist$COMMON.NAME[!is.na(specieslist$ht)]
select_specs = sample(species,50)

load("40_manuscripts/01_blueprint/dataforanalyses_extra_2023.RData")
library(tidyverse)
library(ggthemes)
library(cowplot)

theme_set(theme_tufte())
require(extrafont)

data = data0 %>% filter(ALL.SPECIES.REPORTED == 1)

## lla

spec = select_specs[1]

temp = data %>%
  filter(COMMON.NAME == spec) %>%
  distinct(gridg3,month)
data1 = temp %>% left_join(data)

# lla

datal = data1 %>%
  group_by(no.sp) %>% mutate(lists = n_distinct(group.id)) %>% ungroup %>%
  filter(COMMON.NAME == spec) %>%
  group_by(no.sp) %>% reframe(perc = n()/max(lists))
names(datal)[1] = "effort"
datal$type = "List Length"

datal = datal %>% filter(effort <= 100)
datal$species = spec
dataf = datal



for (i in 2:50)
{
  spec = select_specs[i]
  
  temp = data %>%
    filter(COMMON.NAME == spec) %>%
    distinct(gridg3,month)
  data1 = temp %>% left_join(data)
  
  # lla
  
  datal = data1 %>%
    group_by(no.sp) %>% mutate(lists = n_distinct(group.id)) %>% ungroup %>%
    filter(COMMON.NAME == spec) %>%
    group_by(no.sp) %>% reframe(perc = n()/max(lists))
  names(datal)[1] = "effort"
  datal$type = "List Length"
  
  datal = datal %>% filter(effort <= 100)
  datal$species = spec
  dataf = rbind(dataf,datal)
}


ggp = ggplot(dataf[dataf$species %in% select_specs[1:25],], aes(x=effort, y = perc)) +   facet_wrap(. ~ species, scale="free_y", nrow = 5, ncol = 5, strip.position="top") +
  geom_point() +
  xlab("List Length (no. of species in a checklist)") +
  ylab("Frequency of reporting")

ggp1 = ggp +
  theme(axis.title.x = element_text(size = 16), 
        axis.text.x = element_text(size = 10),
        axis.title.y = element_text(size = 16, angle = 90), 
        axis.text.y = element_text(size = 10)) +
  theme(plot.title = element_text(hjust = 0.5, size = 12, face = 'bold')) +
  theme(text=element_text(family="Gandhi Sans", face = 'bold')) +
  theme(strip.text.x = element_text(size = 8, colour = "black", face = 'italic')) +
  scale_x_continuous(breaks = c(0,20,40,60,80,100)) +
  scale_y_continuous(breaks = c(0.0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1.0)) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()
        ##panel.border = theme_blank(),
        ##panel.background = theme_blank()
  )

nm = "Fig. S1.jpg"
path_write_file = paste("40_manuscripts/01_blueprint/figs/",nm,sep="")

jpeg(path_write_file, units="in", width=9, height=9, res=600)
grid::grid.draw(ggp1)
dev.off()

ggp = ggplot(dataf[dataf$species %in% select_specs[26:50],], aes(x=effort, y = perc)) +
  facet_wrap(. ~ species, scale="free_y", nrow = 5, ncol = 5, strip.position="top") +
  geom_point() +
  xlab("List Length (no. of species in a checklist)") +
  ylab("Frequency of reporting")

ggp2 = ggp +
  theme(axis.title.x = element_text(size = 16), 
        axis.text.x = element_text(size = 10),
        axis.title.y = element_text(size = 16, angle = 90), 
        axis.text.y = element_text(size = 10)) +
  theme(plot.title = element_text(hjust = 0.5, size = 12, face = 'bold')) +
  theme(text=element_text(family="Gandhi Sans", face = 'bold')) +
  theme(strip.text.x = element_text(size = 8, colour = "black", face = 'italic')) +
  scale_x_continuous(breaks = c(0,20,40,60,80,100)) +
  scale_y_continuous(breaks = c(0.0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1.0)) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()
        ##panel.border = theme_blank(),
        ##panel.background = theme_blank()
  )

nm = "Fig. S2.jpg"
path_write_file = paste("40_manuscripts/01_blueprint/figs/",nm,sep="")

jpeg(path_write_file, units="in", width=9, height=9, res=600)
grid::grid.draw(ggp2)
dev.off()


###############################################################
## dur

spec = select_specs[1]

temp = data %>%
  filter(COMMON.NAME == spec) %>%
  distinct(gridg3,month)
data1 = temp %>% left_join(data)

data1$DURATION.MINUTES = data1$DURATION.MINUTES/60

data1$dur = ceiling(data1$DURATION.MINUTES*10)/10

# dur

datar = data1 %>%
  group_by(dur) %>% mutate(lists = n_distinct(group.id)) %>% ungroup %>%
  filter(COMMON.NAME == spec) %>%
  group_by(dur) %>% reframe(perc = n()/max(lists))
names(datar)[1] = "effort"
datar$type = "Duration (hrs)"

datar = datar %>% filter(effort <= 10)
datar$species = spec
datag = datar



for (i in 2:50)
{
  spec = select_specs[i]
  
  temp = data %>%
    filter(COMMON.NAME == spec) %>%
    distinct(gridg3,month)
  data1 = temp %>% left_join(data)
  
  data1$DURATION.MINUTES = data1$DURATION.MINUTES/60
  
  data1$dur = ceiling(data1$DURATION.MINUTES*10)/10
  
  # dur
  
  datar = data1 %>%
    group_by(dur) %>% mutate(lists = n_distinct(group.id)) %>% ungroup %>%
    filter(COMMON.NAME == spec) %>%
    group_by(dur) %>% reframe(perc = n()/max(lists))
  names(datar)[1] = "effort"
  datar$type = "Duration (hours)"
  
  datar = datar %>% filter(effort <= 10)
  datar$species = spec
  datag = rbind(datag,datar)
}


ggp = ggplot(datag[datag$species %in% select_specs[1:25],], aes(x=effort, y = perc)) + 
  facet_wrap(. ~ species, scale="free_y", nrow = 5, ncol = 5, strip.position="top") +
  geom_point() +
  xlab("Duration (hrs)") +
  ylab("Frequency of reporting")

ggp1 = ggp +
  theme(axis.title.x = element_text(size = 14), axis.text.x = element_text(size = 8),
        axis.title.y = element_text(size = 14, angle = 90), axis.text.y = element_text(size = 8)) +
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = 'bold')) +
  theme(text=element_text(family="Gandhi Sans", face = 'bold')) +
  theme(strip.text.x = element_text(size = 10, colour = "black", face = 'italic')) +
  scale_y_continuous(breaks = c(0.0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1.0)) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()
        ##panel.border = theme_blank(),
        ##panel.background = theme_blank()
  )

nm = "Fig. S3.jpg"
path_write_file = paste("40_manuscripts/01_blueprint/figs/",nm,sep="")

jpeg(path_write_file, units="in", width=9, height=9, res=600)
grid::grid.draw(ggp1)
dev.off()


ggp = ggplot(datag[datag$species %in% select_specs[26:50],], aes(x=effort, y = perc)) + 
  facet_wrap(. ~ species, scale="free_y", nrow = 5, ncol = 5, strip.position="top") +
  geom_point() +
  xlab("Duration (hrs)") +
  ylab("Frequency of reporting")

ggp2 = ggp +
  theme(axis.title.x = element_text(size = 14), axis.text.x = element_text(size = 8),
        axis.title.y = element_text(size = 14, angle = 90), axis.text.y = element_text(size = 8)) +
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = 'bold')) +
  theme(text=element_text(family="Gandhi Sans", face = 'bold')) +
  theme(strip.text.x = element_text(size = 10, colour = "black", face = 'italic')) +
  scale_y_continuous(breaks = c(0.0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1.0)) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()
        ##panel.border = theme_blank(),
        ##panel.background = theme_blank()
  )

nm = "Fig. S4.jpg"
path_write_file = paste("40_manuscripts/01_blueprint/figs/",nm,sep="")

jpeg(path_write_file, units="in", width=9, height=9, res=600)
grid::grid.draw(ggp2)
dev.off()

###############################################################
## dis

spec = select_specs[1]

temp = data %>%
  filter(COMMON.NAME == spec) %>%
  distinct(gridg3,month)
data1 = temp %>% left_join(data)

data1$dis = ceiling(data1$EFFORT.DISTANCE.KM*10)/10

# dis

datad = data1 %>%
  group_by(dis) %>% mutate(lists = n_distinct(group.id)) %>% ungroup %>%
  filter(COMMON.NAME == spec) %>%
  group_by(dis) %>% reframe(perc = n()/max(lists))
names(datad)[1] = "effort"
datad$type = "Distance (km)"

datad = datad %>% filter(effort <= 10)
datad$species = spec
datah = datad



for (i in 2:50)
{
  spec = select_specs[i]
  
  temp = data %>%
    filter(COMMON.NAME == spec) %>%
    distinct(gridg3,month)
  data1 = temp %>% left_join(data)
  
  data1$dis = ceiling(data1$EFFORT.DISTANCE.KM*10)/10
  
  # dis
  
  datad = data1 %>%
    group_by(dis) %>% mutate(lists = n_distinct(group.id)) %>% ungroup %>%
    filter(COMMON.NAME == spec) %>%
    group_by(dis) %>% reframe(perc = n()/max(lists))
  names(datad)[1] = "effort"
  datad$type = "Distance (km)"
  
  datad = datad %>% filter(effort <= 10)
  datad$species = spec
  datah = rbind(datah,datad)
}


ggp = ggplot(datah[datah$species %in% select_specs[1:25],], aes(x=effort, y = perc)) + 
  facet_wrap(. ~ species, scale="free_y", nrow = 5, ncol = 5, strip.position="top") +
  geom_point() +
  xlab("Distance (km)") +
  ylab("Frequency of reporting")

ggp1 = ggp +
  theme(axis.title.x = element_text(size = 14), axis.text.x = element_text(size = 8),
        axis.title.y = element_text(size = 14, angle = 90), axis.text.y = element_text(size = 8)) +
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = 'bold')) +
  theme(text=element_text(family="Gandhi Sans", face = 'bold')) +
  theme(strip.text.x = element_text(size = 10, colour = "black", face = 'italic')) +
  scale_y_continuous(breaks = c(0.0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1.0)) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()
        ##panel.border = theme_blank(),
        ##panel.background = theme_blank()
  )

nm = "Fig. S5.jpg"
path_write_file = paste("40_manuscripts/01_blueprint/figs/",nm,sep="")

jpeg(path_write_file, units="in", width=9, height=9, res=600)
grid::grid.draw(ggp1)
dev.off()


ggp = ggplot(datah[datah$species %in% select_specs[26:50],], aes(x=effort, y = perc)) + 
  facet_wrap(. ~ species, scale="free_y", nrow = 5, ncol = 5, strip.position="top") +
  geom_point() +
  xlab("Distance (km)") +
  ylab("Frequency of reporting")

ggp2 = ggp +
  theme(axis.title.x = element_text(size = 14), axis.text.x = element_text(size = 8),
        axis.title.y = element_text(size = 14, angle = 90), axis.text.y = element_text(size = 8)) +
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = 'bold')) +
  theme(text=element_text(family="Gandhi Sans", face = 'bold')) +
  theme(strip.text.x = element_text(size = 10, colour = "black", face = 'italic')) +
  scale_y_continuous(breaks = c(0.0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1.0)) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()
        ##panel.border = theme_blank(),
        ##panel.background = theme_blank()
  )

nm = "Fig. S6.jpg"
path_write_file = paste("40_manuscripts/01_blueprint/figs/",nm,sep="")

jpeg(path_write_file, units="in", width=9, height=9, res=600)
grid::grid.draw(ggp2)
dev.off()



# S7

regions = read.csv("00_data/eBird_district_region_mapping.csv") %>% 
  select(ST_NM,DISTRICT,region)
data = data %>% left_join(regions)

spec = "Jungle Babbler"

temp = data %>%
  filter(COMMON.NAME == spec) %>%
  distinct(gridg3,month)
data1 = temp %>% left_join(data)
#ss = data1 %>% filter(!is.na(region)) %>% 
#  group_by(group.id) %>% slice(1) %>% ungroup %>%
#  group_by(region) %>% reframe(n = n())
data1 = data1 %>%
  group_by(region,no.sp) %>% mutate(lists = n_distinct(group.id)) %>% ungroup %>%
  filter(COMMON.NAME == spec, region %in% c("Coast","RPeninsulas","WGhats")) %>%
  group_by(region,no.sp) %>% reframe(perc = n()/max(lists))


data1[data1$region == "Coast",]$region = "Coast (N=327,484)"
data1[data1$region == "RPeninsulas",]$region = "Rest of peninsula (N=586,107)"
data1[data1$region == "WGhats",]$region = "Western Ghats (N=274,386)"


ggp = ggplot(data1, aes(x=no.sp, y = perc)) + 
  ggtitle(spec) +
  facet_wrap(. ~ region, nrow = 3) +
  #geom_abline(intercept = 0, slope = 1, col = "blue") + 
  geom_point() +
  xlab("List Length (no. of species in a checklist)") +
  ylab("Frequency of reporting")

ggp1 = ggp +
  theme(axis.title.x = element_blank(), axis.text.x = element_text(size = 14),
        axis.title.y = element_blank(), axis.text.y = element_text(size = 14)) +
  theme(plot.title = element_text(hjust = 0.5, size = 20, face = 'bold')) +
  theme(text=element_text(family="Gandhi Sans", face = 'bold')) +
  theme(strip.text.x = element_text(size = 14, colour = "black", face = 'italic')) +
  scale_x_continuous(breaks = c(0,20,40,60,80,100), limits = c(0,100)) +
  scale_y_continuous(breaks = c(0.0,0.2,0.4,0.6,0.8,1.0)) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()
        ##panel.border = theme_blank(),
        ##panel.background = theme_blank()
  )

spec = "Black-winged Kite"

temp = data %>%
  filter(COMMON.NAME == spec) %>%
  distinct(gridg3,month)
data1 = temp %>% left_join(data)
#ss = data1 %>% filter(!is.na(region)) %>% 
#  group_by(group.id) %>% slice(1) %>% ungroup %>%
#  group_by(region) %>% reframe(n = n())
data1 = data1 %>%
  group_by(region,no.sp) %>% mutate(lists = n_distinct(group.id)) %>% ungroup %>%
  filter(COMMON.NAME == spec, region %in% c("Coast","RPeninsulas","WGhats")) %>%
  group_by(region,no.sp) %>% reframe(perc = n()/max(lists))


data1[data1$region == "Coast",]$region = "Coast (N=385,324)"
data1[data1$region == "RPeninsulas",]$region = "Rest of peninsula (N=611,294)"
data1[data1$region == "WGhats",]$region = "Western Ghats (N=272,133)"


ggp = ggplot(data1, aes(x=no.sp, y = perc)) + 
  ggtitle(spec) +
  facet_wrap(. ~ region, nrow = 3) +
  #geom_abline(intercept = 0, slope = 1, col = "blue") + 
  geom_point() +
  xlab("List Length (no. of species in a checklist)") +
  ylab("Frequency of reporting")

ggp2 = ggp +
  theme(axis.title.x = element_blank(), axis.text.x = element_text(size = 14),
        axis.title.y = element_blank(), axis.text.y = element_text(size = 14)) +
  theme(plot.title = element_text(hjust = 0.5, size = 20, face = 'bold')) +
  theme(text=element_text(family="Gandhi Sans", face = 'bold')) +
  theme(strip.text.x = element_text(size = 14, colour = "black", face = 'italic')) +
  scale_x_continuous(breaks = c(0,20,40,60,80,100), limits = c(0,100)) +
  scale_y_continuous(breaks = c(0.0,0.2,0.4,0.6,0.8,1.0)) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()
        ##panel.border = theme_blank(),
        ##panel.background = theme_blank()
  )

ggpp = plot_grid(ggp1,ggp2,nrow=1,ncol=2,rel_widths = c(1/2, 1/2))

require(grid)
require(gridExtra)

x.grob = textGrob("List Length (no. of species in a checklist)", 
                  gp = gpar(fontface = "bold", fontsize = 20,
                            fontfamily = "Gandhi Sans"))

y.grob = textGrob("Frequency of reporting", gp = gpar(fontface = "bold", fontsize = 20,
                                                      fontfamily = "Gandhi Sans"), 
                  rot = 90)

ggpp1 = grid.arrange(arrangeGrob(ggpp, bottom = x.grob, left = y.grob))

nm = "Fig. S7.jpg"
path_write_file = paste("40_manuscripts/01_blueprint/figs/",nm,sep="")

jpeg(path_write_file, units="in", width=9, height=11, res=600)
grid::grid.draw(ggpp1)
dev.off()