plot_soib_trends("single", "none", "LTT", "Alpine Swift")

# plot_type = "single"
cur_mask = "none"
cur_trend = "LTT"
cur_spec = "Alpine Swift"
# cur_spec <- "all"
#

plot_type <- "single_mask"


###########


for (z in qualifying.species)
{
  
  temp = trends %>% 
    filter(COMMON.NAME %in% z)
  
  temp$COMMON.NAME = temp$Mask
  

  
  t1 = temp[temp$timegroups == 2022,]
  t1 = t1 %>% arrange(desc(mean_std))
  order = t1$COMMON.NAME
  
  cols.masks$Mask = factor(cols.masks$Mask, levels = order)
  cols.masks = cols.masks %>% arrange(Mask)
  
  
  temp$COMMON.NAME = factor(temp$COMMON.NAME, 
                            levels = order)
  

  
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
