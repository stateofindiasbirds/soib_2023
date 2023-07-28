plot_soib_trends("single", "none", "LTT", "Alpine Swift")



compar_metadata <- analyses_metadata %>% 
  filter(MASK.TYPE %in% c("country", "habitat", "conservation_area"))

# process all MASK data
data_processed <- map(unique(compar_metadata$MASK), 
                      maskcompar_read_data)

data_main <- map(data_processed, pluck, "data_main") %>% bind_rows()
data_trends <- map(data_processed, pluck, "data_trends") %>% bind_rows()

# Filter qualifying species
qualifying_species <- all_main_data %>%
  filter(!SOIBv2.Long.Term.Status %in% c("eBird Data Inconclusive", "eBird Data Deficient") &
           Long.Term.Analysis == "X") %>%
  pull(eBird.English.Name.2022)

# Filter trends data based on qualifying species and timegroups
trends <- all_trends_data %>%
  filter(COMMON.NAME %in% qualifying_species, timegroups <= 2022)


## compare for a species across masks


cur_metadata <- analyses_metadata %>% filter(MASK == "none")
main = read.csv(cur_metadata$SOIBMAIN.PATH)
main$Mask = "Country as a whole"
trends = read.csv(cur_metadata$TRENDS.OUTPATH)
trends$Mask = "Country as a whole"

cur_metadata <- analyses_metadata %>% filter(MASK == "woodland")
main.woodland = read.csv(cur_metadata$SOIBMAIN.PATH)
main.woodland$Mask = "Grids with threshold woodland"
trends.woodland = read.csv(cur_metadata$TRENDS.OUTPATH)
trends.woodland$Mask = "Grids with threshold woodland"

cur_metadata <- analyses_metadata %>% filter(MASK == "PA")
main.pa = read.csv(cur_metadata$SOIBMAIN.PATH)
main.pa$Mask = "Protected areas"
trends.pa = read.csv(cur_metadata$TRENDS.OUTPATH)
trends.pa$Mask = "Protected areas"

cur_metadata <- analyses_metadata %>% filter(MASK == "cropland")
main.crop = read.csv(cur_metadata$SOIBMAIN.PATH)
main.crop$Mask = "Grids with threshold cropland"
trends.crop = read.csv(cur_metadata$TRENDS.OUTPATH)
trends.crop$Mask = "Grids with threshold cropland"

cur_metadata <- analyses_metadata %>% filter(MASK == "ONEland")
main.one = read.csv(cur_metadata$SOIBMAIN.PATH)
main.one$Mask = "Grids with threshold ONEs"
trends.one = read.csv(cur_metadata$TRENDS.OUTPATH)
trends.one$Mask = "Grids with threshold ONEs"



qualifying.species = main$eBird.English.Name.2022[!main$SOIBv2.Long.Term.Status %in% 
                                                    c("eBird Data Inconclusive","eBird Data Deficient") & 
                                                    main$Long.Term.Analysis == "X"]
qualifying.species.woodland = main.woodland$eBird.English.Name.2022[!main.woodland$SOIBv2.Long.Term.Status %in% 
                                                                      c("eBird Data Inconclusive","eBird Data Deficient") & 
                                                                      main.woodland$Long.Term.Analysis == "X"]
qualifying.species.pa = main.pa$eBird.English.Name.2022[!main.pa$SOIBv2.Long.Term.Status %in% 
                                                          c("eBird Data Inconclusive","eBird Data Deficient") & 
                                                          main.pa$Long.Term.Analysis == "X"]
qualifying.species.crop = main.crop$eBird.English.Name.2022[!main.crop$SOIBv2.Long.Term.Status %in% 
                                                              c("eBird Data Inconclusive","eBird Data Deficient") & 
                                                              main.crop$Long.Term.Analysis == "X"]
qualifying.species.one = main.one$eBird.English.Name.2022[!main.one$SOIBv2.Long.Term.Status %in% 
                                                            c("eBird Data Inconclusive","eBird Data Deficient") & 
                                                            main.one$Long.Term.Analysis == "X"]


trends = trends %>% filter(COMMON.NAME %in% qualifying.species)
trends.woodland = trends.woodland %>% filter(COMMON.NAME %in% qualifying.species.woodland)
trends.pa = trends.pa %>% filter(COMMON.NAME %in% qualifying.species.pa)
trends.crop = trends.crop %>% filter(COMMON.NAME %in% qualifying.species.crop)
trends.one = trends.one %>% filter(COMMON.NAME %in% qualifying.species.one)

trends = rbind(trends,trends.woodland,trends.pa,trends.crop,trends.one)

qualifying.species.x = union(qualifying.species.woodland,qualifying.species.pa)
qualifying.species.x = union(qualifying.species.x,qualifying.species.crop)
qualifying.species.x = union(qualifying.species.x,qualifying.species.one)
qualifying.species = intersect(qualifying.species,qualifying.species.x)

trends = trends %>% filter(COMMON.NAME %in% qualifying.species) %>%
  filter(timegroups <= 2022)

for (z in qualifying.species)
{
  
  temp = trends %>% 
    filter(COMMON.NAME %in% z)
  
  temp$COMMON.NAME = temp$Mask
  
  cols.masks = data.frame(Mask = c("Country as a whole","Grids with threshold woodland","Protected areas","Grids with threshold cropland","Grids with threshold ONEs"),
                          cols = c("#CC6666","#869B27","#66CC99","#B69AC9","#E49B36"))
  cols.masks = cols.masks %>% filter(Mask %in% temp$COMMON.NAME)
  
  
  t1 = temp[temp$timegroups == 2022,]
  t1 = t1 %>% arrange(desc(mean_std))
  order = t1$COMMON.NAME
  
  cols.masks$Mask = factor(cols.masks$Mask, levels = order)
  cols.masks = cols.masks %>% arrange(Mask)
  
  
  temp$COMMON.NAME = factor(temp$COMMON.NAME, 
                            levels = order)
  
  
  #loadfonts(device = "win")
  
  cols = c("#869B27", "#31954E", "#E49B36", "#CC6666", "#78CAE0", "#9999CC", "#493F3D",
           "#B69AC9", "#A13E2B", "#EA5599", "#000000", "#66CC99")
  #cols = c("#41726c","#2d809b","#e0d27b","#8cc48c","#55bfaf")
  tcol = "black"
  pcol = "#A13E2B"
  
  
  cols = cols.masks$cols
  
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
  
  sps = z
  
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

  ggp = ggplot(temp, aes(x = timegroups, y = mean_std, col = COMMON.NAME, label = COMMON.NAMEx)) +
    geom_line(linewidth = 2) +
    geom_text_repel(nudge_x = -2, direction = "y", hjust = "center", size = 4, 
                    min.segment.length = Inf, fontface = "bold") +
    #geom_point(size = 3) +
    scale_colour_manual(breaks = bks1, 
                        labels = lbs1,
                        values = cols1) +
    scale_fill_manual(breaks = bks1, 
                      labels = lbs1,
                      values = cols1) +

}
