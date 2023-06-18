
tic("for loops lt trends (full)")

for (i in unique(modtrends$COMMON.NAME))
{
  
  for (j in unique(modtrends$timegroups[modtrends$COMMON.NAME == i]))
  {
    mean_trans = modtrends$mean_trans[modtrends$COMMON.NAME == i & modtrends$timegroups == j]
    se_trans = modtrends$se_trans[modtrends$COMMON.NAME == i & modtrends$timegroups == j]
    m1 = modtrends$m1[modtrends$COMMON.NAME == i & modtrends$timegroups == j]
    s1 = modtrends$s1[modtrends$COMMON.NAME == i & modtrends$timegroups == j]
    
    set.seed(10)
    tp0 = simerrordiv(mean_trans,m1,se_trans,s1)
    tp0$timegroups = j
    
    l = quantile(tp0$rat,0.025)
    r = quantile(tp0$rat,0.975)
    
    modtrends$lci_std[modtrends$COMMON.NAME == i & modtrends$timegroups == j] = 
      100*as.numeric(l)
    modtrends$rci_std[modtrends$COMMON.NAME == i & modtrends$timegroups == j] = 
      100*as.numeric(r)
    
    if (j == 2022)
    {
      longtermlci = modtrends$lci_std[modtrends$COMMON.NAME == i & modtrends$timegroups == j]
      longtermmean = modtrends$mean_std[modtrends$COMMON.NAME == i & modtrends$timegroups == j]
      longtermrci = modtrends$rci_std[modtrends$COMMON.NAME == i & modtrends$timegroups == j]
      
      main$longtermlci[main$eBird.English.Name.2022 == i] = longtermlci
      main$longtermmean[main$eBird.English.Name.2022 == i] = longtermmean
      main$longtermrci[main$eBird.English.Name.2022 == i] = longtermrci
    }
  }
  print(i)
}

toc(quiet = TRUE, log = TRUE)
