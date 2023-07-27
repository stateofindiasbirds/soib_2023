
# current species in the overall iteration
cur_spec <- .x

cur_data_trends <- data_trends


  temp = trends %>% 
    filter(COMMON.NAME %in% z)

  t1 = temp[temp$timegroups == 2022,]

  #
  
  temp = trends %>% 
    mutate(lci_std = lci_std_recent,mean_std = mean_std_recent,rci_std = rci_std_recent) %>%
    filter(COMMON.NAME %in% z)
  
  t1 = temp[temp$timegroups == 2022,]
  
  
  
  
  
  
  
  
  
  
  
  
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
  
  
  
  
}
  
# limits

# the extra, important line

# 

