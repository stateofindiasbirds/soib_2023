require(tidyverse)
require(raster)
require(ggsci)
library(sf)

occ.final = read.csv("01_analyses_full/data_to_plot_distribution_maps.csv")
vagrant_presence = read.csv("01_analyses_full/data_vagrant_for_distribution_maps.csv")
load("00_data/maps_sf.RData")
main = read.csv("01_analyses_full/results/SoIB_main.csv")

# read in TIF
indiatif = brick("00_data/IndiaDEM-Colour.tif")
r2 = mask(indiatif, india_sf)
indiatif = r2
indiatif = as.data.frame(indiatif, xy = TRUE)
indiatif$IndiaDEM.Colour_1 = indiatif$IndiaDEM.Colour_1/255
indiatif$IndiaDEM.Colour_2 = indiatif$IndiaDEM.Colour_2/255
indiatif$IndiaDEM.Colour_3 = indiatif$IndiaDEM.Colour_3/255
names(indiatif)[3:5] = c("r","g","b")
indiatif[is.na(indiatif)] = 0
indiatif$codes = rgb(indiatif$r,indiatif$g,indiatif$b)
indiatif = indiatif %>% mutate(codes = replace(codes, codes == "#000000", NA))

yearround = "#562377" 
summer = "#dc6f42"
passage = "#e4b73e"
winter = "#00858f"

basemap = ggplot() +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  geom_raster(data = indiatif , aes(x = x, y = y, fill = codes), alpha = 0.3) +
  scale_fill_grey(na.value = "transparent") +
  theme(text=element_text(family="Gandhi Sans")) +
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        plot.margin=unit(c(0.5,0,0.5,0), "cm"),
        plot.title = element_text(hjust = 0.5),
        plot.background = element_rect(fill = "transparent",colour = NA),
        panel.background = element_rect(fill = "transparent",colour = NA))+
  theme(legend.position = "none")+
  annotate("rect", xmin=c(82.2,82.2,82.2,82.2), xmax=c(83.7,83.7,83.7,83.7), ymin=c(13,11,9,7), 
                       ymax=c(14.5,12.5,10.5,8.5), alpha=0.6, fill = c(yearround,summer,passage,winter))+
  annotate("rect", xmin=c(82.4,82.4,82.4,82.4), xmax=c(83.5,83.5,83.5,83.5), ymin=c(13.2,11.2,9.2,7.2), 
                       ymax=c(14.3,12.3,10.3,8.3), alpha=1, fill=c(yearround,summer,passage,winter))+
  annotate("text", x = c(86.5,86,87.5,85.7), y = c(13.75,11.75,9.75,7.75), 
                       label = c("Year-round","Summer","Spring/Autumn","Winter") , 
           color="#56697B", size=5, family="Gandhi Sans")+
  coord_sf()


species = "Nilgiri Thrush" #run through all in specieslists

data.base = occ.final %>% filter(COMMON.NAME == species)
data.base$gridg1 = as.character(data.base$gridg1)
data.sf = g1_in_sf %>% left_join(data.base, by = c("GRID.G1" = "gridg1"))
vagrant.base = vagrant_presence %>% filter(COMMON.NAME == species) 

ggp = basemap +
  geom_sf(data = data.sf %>% filter(status == "YR"), aes(alpha = occupancy), fill = yearround, col = NA) +
  geom_sf(data = data.sf %>% filter(status == "S"), aes(alpha = occupancy), fill = summer, col = NA) +
  geom_sf(data = data.sf %>% filter(status == "P"), aes(alpha = occupancy), fill = passage, col = NA) +
  geom_sf(data = data.sf %>% filter(status == "W"), aes(alpha = occupancy), fill = winter, col = NA) +
  geom_point(data = vagrant.base %>% filter(status == "S"), aes(x = LONGITUDE, y = LATITUDE), 
             col = summer, shape = 4, size = 1, alpha = 1)+
  geom_point(data = vagrant.base %>% filter(status == "P"), aes(x = LONGITUDE, y = LATITUDE), 
             col = passage, shape = 4, size = 1, alpha = 1)+
  geom_point(data = vagrant.base %>% filter(status == "W"), aes(x = LONGITUDE, y = LATITUDE), 
             col = winter, shape = 4, size = 1, alpha = 1)+
  geom_sf(data = states_sf, colour = "white", fill = NA, size = 0.2)


sps = as.character(main$India.Checklist.Common.Name[main$eBird.English.Name.2022 == species])
name = paste(sps,".png",sep="")

print(ggp)
ggsave(file=name, units="in", width=6.3, height=7, bg = "white") #change to transparent if required
dev.off()
