########### tightness of relationship between detectability and effort


load("40_manuscripts/01_blueprint/dataforanalyses_extra_2023.RData")
library(tidyverse)
library(ggthemes)
library(cowplot)

data0 = data0 %>% filter(ALL.SPECIES.REPORTED == 1)

theme_set(theme_tufte())
require(extrafont)

nm = "Fig. 2.jpg"
path_write_file = paste("40_manuscripts/01_blueprint/figs/",nm,sep="")

spec1 = "Jungle Babbler"

temp = data0 %>%
  filter(COMMON.NAME == spec) %>%
  distinct(gridg3,month)
data1 = temp %>% left_join(data0)

data1$DURATION.MINUTES = data1$DURATION.MINUTES/60

data1$dur = ceiling(data1$DURATION.MINUTES*10)/10

data1$dis = ceiling(data1$EFFORT.DISTANCE.KM*5)/5

data1$species = spec1


spec2 = "Black-winged Kite"

temp = data0 %>%
  filter(COMMON.NAME == spec) %>%
  distinct(gridg3,month)
data2 = temp %>% left_join(data0)

data2$DURATION.MINUTES = data2$DURATION.MINUTES/60

data2$dur = ceiling(data2$DURATION.MINUTES*10)/10

data2$dis = ceiling(data2$EFFORT.DISTANCE.KM*10)/10

data2$species = spec2


data_comb = rbind(data1,data2)




# lla

data = data_comb %>%
  group_by(species,no.sp) %>% mutate(lists = n_distinct(group.id)) %>% ungroup %>%
  filter((COMMON.NAME == spec1 & species == spec1) | (COMMON.NAME == spec2 & species == spec2)) %>%
  group_by(species,no.sp) %>% reframe(perc = n()/max(lists))
names(data)[2] = "effort"
data$species = factor(data$species, levels = c("Jungle Babbler","Black-winged Kite"))

data = data %>% filter(effort <= 100)

ggp = ggplot(data, aes(x=effort, y = perc)) +
  facet_wrap(. ~ species, nrow = 1, strip.position="top") +
  #geom_abline(intercept = 0, slope = 1, col = "blue") + 
  geom_point() +
  xlab("List Length (no. of species in a checklist)")

ggp_a = ggp +
  theme(axis.title.x = element_text(vjust = -0/8, size = 18, margin = margin(t = 10)),
        axis.text.x = element_text(size = 16),
        axis.title.y = element_blank(), axis.text.y = element_text(size = 16)) +
  theme(text=element_text(family="Gandhi Sans", face = 'bold')) +
  theme(strip.text.x = element_text(size = 20, face = 'bold')) +
  scale_x_continuous(breaks = c(0,20,40,60,80,100)) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()
        ##panel.border = theme_blank(),
        ##panel.background = theme_blank()
  )



# dur

data = data_comb %>%
  group_by(species,dur) %>% mutate(lists = n_distinct(group.id)) %>% ungroup %>%
  filter((COMMON.NAME == spec1 & species == spec1) | (COMMON.NAME == spec2 & species == spec2)) %>%
  group_by(species,dur) %>% reframe(perc = n()/max(lists))
names(data)[2] = "effort"
data$species = factor(data$species, levels = c("Jungle Babbler","Black-winged Kite"))

data = data %>% filter(effort <= 10)

ggp = ggplot(data, aes(x=effort, y = perc)) +
  facet_wrap(. ~ species, nrow = 1, strip.position="top") +
  #geom_abline(intercept = 0, slope = 1, col = "blue") + 
  geom_point() +
  xlab("Duration (hrs)")

ggp_b = ggp +
  theme(axis.title.x = element_text(vjust = -0/8, size = 18, margin = margin(t = 10)),
        axis.text.x = element_text(size = 16),
        axis.title.y = element_blank(), axis.text.y = element_text(size = 16)) +
  theme(text=element_text(family="Gandhi Sans", face = 'bold')) +
  theme(strip.text.x = element_text(size = 20, face = 'bold', colour = "white")) +
  scale_x_continuous(breaks = c(0,2,4,6,8,10)) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()
        ##panel.border = theme_blank(),
        ##panel.background = theme_blank()
  )



# dis

data = data_comb %>%
  group_by(species,dis) %>% mutate(lists = n_distinct(group.id)) %>% ungroup %>%
  filter((COMMON.NAME == spec1 & species == spec1) | (COMMON.NAME == spec2 & species == spec2)) %>%
  group_by(species,dis) %>% reframe(perc = n()/max(lists))
names(data)[2] = "effort"
data$species = factor(data$species, levels = c("Jungle Babbler","Black-winged Kite"))

data = data %>% filter(effort <= 10)

ggp = ggplot(data, aes(x=effort, y = perc)) +
  facet_wrap(. ~ species, nrow = 1, strip.position="top") +
  #geom_abline(intercept = 0, slope = 1, col = "blue") + 
  geom_point() +
  xlab("Distance (km)")

ggp_c = ggp +
  theme(axis.title.x = element_text(vjust = -0/8, size = 18, margin = margin(t = 10)),
        axis.text.x = element_text(size = 16),
        axis.title.y = element_blank(), axis.text.y = element_text(size = 16)) +
  theme(text=element_text(family="Gandhi Sans", face = 'bold')) +
  theme(strip.text.x = element_text(size = 20, face = 'bold', colour = "white")) +
  scale_x_continuous(breaks = c(0,2,4,6,8,10)) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()
        ##panel.border = theme_blank(),
        ##panel.background = theme_blank()
  )





ggpp = plot_grid(ggp_a,ggp_b,ggp_c,nrow=3,ncol=1,rel_heights = c(1/3, 1/3, 1/3))

require(grid)
require(gridExtra)

x.grob = textGrob("Effort", gp = gpar(fontface = "bold", fontsize = 20,
                                      fontfamily = "Gandhi Sans"))

y.grob = textGrob("Frequency of reporting", 
                  gp = gpar(fontface = "bold", fontsize = 20,
                            fontfamily = "Gandhi Sans"), rot = 90)

ggpp1 = grid.arrange(arrangeGrob(ggpp,left = y.grob))

jpeg(path_write_file, units="in", width=9, height=11, res=600)
grid::grid.draw(ggpp1)
dev.off()