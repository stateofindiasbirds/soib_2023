
tic("current trends: for loops (full but 5spp.) SENSSP")

flag = 0
newdata = data.frame(timegroups = extra.years)
newdata1 = data.frame(timegroups = unique(modtrends_recent$timegroups))

for (i in unique(modtrends_recent$COMMON.NAME)[1:5])
{
  ct = 0
  for (j in unique(modtrends_recent$timegroups[modtrends_recent$COMMON.NAME == i]))
  {
    ct = ct + 1
    mean_trans = modtrends_recent$mean_trans[modtrends_recent$COMMON.NAME == i & modtrends_recent$timegroups == j]
    se_trans = modtrends_recent$se_trans[modtrends_recent$COMMON.NAME == i & modtrends_recent$timegroups == j]
    m1 = modtrends_recent$m1[modtrends_recent$COMMON.NAME == i & modtrends_recent$timegroups == j]
    s1 = modtrends_recent$s1[modtrends_recent$COMMON.NAME == i & modtrends_recent$timegroups == j]
    
    set.seed(10)
    tp0 = simerrordiv(mean_trans,m1,se_trans,s1)
    tp0$timegroups = j
    
    l = quantile(tp0$rat,0.025)
    r = quantile(tp0$rat,0.975)
    
    modtrends_recent$lci_std_recent[modtrends_recent$COMMON.NAME == i & modtrends_recent$timegroups == j] = 
      100*as.numeric(l)
    modtrends_recent$rci_std_recent[modtrends_recent$COMMON.NAME == i & modtrends_recent$timegroups == j] = 
      100*as.numeric(r)
    
    if (ct == 1)
      tp = tp0
    if (ct > 1)
      tp = rbind(tp,tp0)
  }
  
  
  pred0 = newdata
  flag = flag + 1
  
  ct = 0
  sl = numeric(1000)
  slse = numeric(1000)
  
  sl.sens1 = sl.sens2 = sl.sens3 = sl.sens4 = sl.sens5 = sl.sens6 = sl.sens7 = sl.sens8 = numeric(1000)
  slse.sens1 = slse.sens2 = slse.sens3 = slse.sens4 = slse.sens5 = slse.sens6 = slse.sens7 = slse.sens8 = numeric(1000)
  
  set.seed(1)
  for (z in 1:1000)
  {
    ct = ct + 1
    temp = tp %>%
      group_by(timegroups) %>%
      reframe(val = sample(val,1))
    
    
    #################################### sensitivity analysis ####

    fit1.sens = with(temp[temp$timegroups != 2015,],lm(val~timegroups))

    pd1 = predict(fit1.sens,
                  data.frame(timegroups = unique(temp$timegroups[temp$timegroups != 2015])),se = T)
    num = pd1$fit[2]-pd1$fit[1]
    den = abs(pd1$fit[1])
    numse = sqrt(pd1$se.fit[1]^2 + pd1$se.fit[2]^2)
    dense = pd1$se.fit[1]
    sl.sens1[z] = 100*errordiv(num,den,numse,dense)[1]
    slse.sens1[z] = errordiv(num,den,numse,dense)[2]


    fit1.sens = with(temp[temp$timegroups != 2016,],lm(val~timegroups))

    pd1 = predict(fit1.sens,
                  data.frame(timegroups = unique(temp$timegroups[temp$timegroups != 2016])),se = T)
    num = pd1$fit[2]-pd1$fit[1]
    den = abs(pd1$fit[1])
    numse = sqrt(pd1$se.fit[1]^2 + pd1$se.fit[2]^2)
    dense = pd1$se.fit[1]
    sl.sens2[z] = 100*errordiv(num,den,numse,dense)[1]
    slse.sens2[z] = errordiv(num,den,numse,dense)[2]


    fit1.sens = with(temp[temp$timegroups != 2017,],lm(val~timegroups))

    pd1 = predict(fit1.sens,
                  data.frame(timegroups = unique(temp$timegroups[temp$timegroups != 2017])),se = T)
    num = pd1$fit[2]-pd1$fit[1]
    den = abs(pd1$fit[1])
    numse = sqrt(pd1$se.fit[1]^2 + pd1$se.fit[2]^2)
    dense = pd1$se.fit[1]
    sl.sens3[z] = 100*errordiv(num,den,numse,dense)[1]
    slse.sens3[z] = errordiv(num,den,numse,dense)[2]


    fit1.sens = with(temp[temp$timegroups != 2018,],lm(val~timegroups))

    pd1 = predict(fit1.sens,
                  data.frame(timegroups = unique(temp$timegroups[temp$timegroups != 2018])),se = T)
    num = pd1$fit[2]-pd1$fit[1]
    den = abs(pd1$fit[1])
    numse = sqrt(pd1$se.fit[1]^2 + pd1$se.fit[2]^2)
    dense = pd1$se.fit[1]
    sl.sens4[z] = 100*errordiv(num,den,numse,dense)[1]
    slse.sens4[z] = errordiv(num,den,numse,dense)[2]



    fit1.sens = with(temp[temp$timegroups != 2019,],lm(val~timegroups))

    pd1 = predict(fit1.sens,
                  data.frame(timegroups = unique(temp$timegroups[temp$timegroups != 2019])),se = T)
    num = pd1$fit[2]-pd1$fit[1]
    den = abs(pd1$fit[1])
    numse = sqrt(pd1$se.fit[1]^2 + pd1$se.fit[2]^2)
    dense = pd1$se.fit[1]
    sl.sens5[z] = 100*errordiv(num,den,numse,dense)[1]
    slse.sens5[z] = errordiv(num,den,numse,dense)[2]



    fit1.sens = with(temp[temp$timegroups != 2020,],lm(val~timegroups))

    pd1 = predict(fit1.sens,
                  data.frame(timegroups = unique(temp$timegroups[temp$timegroups != 2020])),se = T)
    num = pd1$fit[2]-pd1$fit[1]
    den = abs(pd1$fit[1])
    numse = sqrt(pd1$se.fit[1]^2 + pd1$se.fit[2]^2)
    dense = pd1$se.fit[1]
    sl.sens6[z] = 100*errordiv(num,den,numse,dense)[1]
    slse.sens6[z] = errordiv(num,den,numse,dense)[2]



    fit1.sens = with(temp[temp$timegroups != 2021,],lm(val~timegroups))

    pd1 = predict(fit1.sens,
                  data.frame(timegroups = unique(temp$timegroups[temp$timegroups != 2021])),se = T)
    num = pd1$fit[2]-pd1$fit[1]
    den = abs(pd1$fit[1])
    numse = sqrt(pd1$se.fit[1]^2 + pd1$se.fit[2]^2)
    dense = pd1$se.fit[1]
    sl.sens7[z] = 100*errordiv(num,den,numse,dense)[1]
    slse.sens7[z] = errordiv(num,den,numse,dense)[2]



    fit1.sens = with(temp[temp$timegroups != 2022,],lm(val~timegroups))

    pd1 = predict(fit1.sens,
                  data.frame(timegroups = unique(temp$timegroups[temp$timegroups != 2022])),se = T)
    num = pd1$fit[2]-pd1$fit[1]
    den = abs(pd1$fit[1])
    numse = sqrt(pd1$se.fit[1]^2 + pd1$se.fit[2]^2)
    dense = pd1$se.fit[1]
    sl.sens8[z] = 100*errordiv(num,den,numse,dense)[1]
    slse.sens8[z] = errordiv(num,den,numse,dense)[2]




    
  }
  
  
  ######################### sensitivity ####

  sl.sens1 = as.numeric(sl.sens1)
  slse.sens1 = as.numeric(slse.sens1)
  se.slope.sens1 = sd(sl.sens1) + sqrt(sum(slse.sens1^2)/length(slse.sens1))

  sens$currentslopelci1[sens$eBird.English.Name.2022 == i] = mean(sl.sens1) - 1.96*se.slope.sens1
  sens$currentslopemean1[sens$eBird.English.Name.2022 == i] = mean(sl.sens1)
  sens$currentsloperci1[sens$eBird.English.Name.2022 == i] = mean(sl.sens1) + 1.96*se.slope.sens1

  sl.sens2 = as.numeric(sl.sens2)
  slse.sens2 = as.numeric(slse.sens2)
  se.slope.sens2 = sd(sl.sens2) + sqrt(sum(slse.sens2^2)/length(slse.sens2))

  sens$currentslopelci2[sens$eBird.English.Name.2022 == i] = mean(sl.sens2) - 1.96*se.slope.sens2
  sens$currentslopemean2[sens$eBird.English.Name.2022 == i] = mean(sl.sens2)
  sens$currentsloperci2[sens$eBird.English.Name.2022 == i] = mean(sl.sens2) + 1.96*se.slope.sens2

  sl.sens3 = as.numeric(sl.sens3)
  slse.sens3 = as.numeric(slse.sens3)
  se.slope.sens3 = sd(sl.sens3) + sqrt(sum(slse.sens3^2)/length(slse.sens3))

  sens$currentslopelci3[sens$eBird.English.Name.2022 == i] = mean(sl.sens3) - 1.96*se.slope.sens3
  sens$currentslopemean3[sens$eBird.English.Name.2022 == i] = mean(sl.sens3)
  sens$currentsloperci3[sens$eBird.English.Name.2022 == i] = mean(sl.sens3) + 1.96*se.slope.sens3

  sl.sens4 = as.numeric(sl.sens4)
  slse.sens4 = as.numeric(slse.sens4)
  se.slope.sens4 = sd(sl.sens4) + sqrt(sum(slse.sens4^2)/length(slse.sens4))

  sens$currentslopelci4[sens$eBird.English.Name.2022 == i] = mean(sl.sens4) - 1.96*se.slope.sens4
  sens$currentslopemean4[sens$eBird.English.Name.2022 == i] = mean(sl.sens4)
  sens$currentsloperci4[sens$eBird.English.Name.2022 == i] = mean(sl.sens4) + 1.96*se.slope.sens4

  sl.sens5 = as.numeric(sl.sens5)
  slse.sens5 = as.numeric(slse.sens5)
  se.slope.sens5 = sd(sl.sens5) + sqrt(sum(slse.sens5^2)/length(slse.sens5))

  sens$currentslopelci5[sens$eBird.English.Name.2022 == i] = mean(sl.sens5) - 1.96*se.slope.sens5
  sens$currentslopemean5[sens$eBird.English.Name.2022 == i] = mean(sl.sens5)
  sens$currentsloperci5[sens$eBird.English.Name.2022 == i] = mean(sl.sens5) + 1.96*se.slope.sens5

  sl.sens6 = as.numeric(sl.sens6)
  slse.sens6 = as.numeric(slse.sens6)
  se.slope.sens6 = sd(sl.sens6) + sqrt(sum(slse.sens6^2)/length(slse.sens6))

  sens$currentslopelci6[sens$eBird.English.Name.2022 == i] = mean(sl.sens6) - 1.96*se.slope.sens6
  sens$currentslopemean6[sens$eBird.English.Name.2022 == i] = mean(sl.sens6)
  sens$currentsloperci6[sens$eBird.English.Name.2022 == i] = mean(sl.sens6) + 1.96*se.slope.sens6

  sl.sens7 = as.numeric(sl.sens7)
  slse.sens7 = as.numeric(slse.sens7)
  se.slope.sens7 = sd(sl.sens7) + sqrt(sum(slse.sens7^2)/length(slse.sens7))

  sens$currentslopelci7[sens$eBird.English.Name.2022 == i] = mean(sl.sens7) - 1.96*se.slope.sens7
  sens$currentslopemean7[sens$eBird.English.Name.2022 == i] = mean(sl.sens7)
  sens$currentsloperci7[sens$eBird.English.Name.2022 == i] = mean(sl.sens7) + 1.96*se.slope.sens7

  sl.sens8 = as.numeric(sl.sens8)
  slse.sens8 = as.numeric(slse.sens8)
  se.slope.sens8 = sd(sl.sens8) + sqrt(sum(slse.sens8^2)/length(slse.sens8))

  sens$currentslopelci8[sens$eBird.English.Name.2022 == i] = mean(sl.sens8) - 1.96*se.slope.sens8
  sens$currentslopemean8[sens$eBird.English.Name.2022 == i] = mean(sl.sens8)
  sens$currentsloperci8[sens$eBird.English.Name.2022 == i] = mean(sl.sens8) + 1.96*se.slope.sens8

  
  print(i) 
  
}

toc(quiet = TRUE, log = TRUE)