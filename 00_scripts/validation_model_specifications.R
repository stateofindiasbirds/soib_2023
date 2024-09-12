# demonstrate that model can be specified with interaction of two variables without
# individual effects of each variable

library(tidyverse)
# simplifying to three instead of four seasons

# Let's assume that the relationship between freq and listlength is linear
# During season 1, freq = x1 + y1*listlength
# During season 2, freq = x2 + y2*listlength
# During season 3, freq = x3 + y3*listlength

# model 1
# freq ~ season + season:listlength (6 parameters to estimate)
# freq = a1 + a2*season2 + a3*season3 + b1*listlength + b2*season2:listlength +b3*season3:listlength
# easy interpretation because after estimation,
# x1 = a1, x2 = a1 + a2, x3 = a1 + a3,  similarly for the 'b's

# model 2
# freq ~ season*listlength

# Simulating values for Xs anf Ys
x1 = rnorm(30,50,0.2) # x1 = 50
y1 = rnorm(30,0.6,0.1) # y1 = 0.6
x2 = rnorm(30,30,0.2) # x2 = 30
y2 = rnorm(30,1,0.1) # y2 = 1
x3 = rnorm(30,1,0.2) # x3 = 1
y3 = rnorm(30,2,0.1) # y3 = 2
listlength = rep(1:100, each = 30)

s1 = data.frame(season = "Summer", x = x1, y = y1)
s1 = s1[rep(1:nrow(s1),100),]
s1$listlength = listlength

s2 = data.frame(season = "Monsoon", x = x2, y = y2)
s2 = s2[rep(1:nrow(s2),100),]
s2$listlength = listlength

s3 = data.frame(season = "Winter", x = x3, y = y3)
s3 = s3[rep(1:nrow(s3),100),]
s3$listlength = listlength

s = rbind(s1,s2,s3)
s$freq = s$x + s$y*s$listlength
s$season = factor(s$season, levels = c("Summer","Monsoon","Winter"))

with(s,plot(freq~listlength))

# fit models

fit1 = lm(data = s, freq ~ season + season:listlength)
summary(fit1)

fit2 = lm(data = s, freq ~ season*listlength)
summary(fit2)

# Both models give identical results because standalone listlength is 
# redundant here and is just the association between one of the seasons and listlength
# Summer in this case

# We could have potentially even removed the intercept where the interpretation is 
# actually most straightforward

fit3 = lm(data = s, freq ~ 0 + season + season:listlength)
summary(fit3)
