library(ggplot2)
library(plm)
library(dplyr)
library(tidyr)
library(stargazer)
library(pcse)
mydata <- read.csv("https://raw.githubusercontent.com/haowang666/Veto-Fiscal/master/data/mydata.csv")
#add polity
mydata$polity <- mydata$e_democ - mydata$e_autoc


pmodel0 <- plm(stability_0 ~ 
                 e_h_polcon5 +  
                 v2xps_party +
                 v2x_corr + 
                 polity + 
                 v2mecenefm + 
                 e_peaveduc + 
                 e_migdppcln +
                 e_migdpgro , 
               data = mydata, index = c("cowcode", "year"), na.action = na.omit, model = "pooling")

#install.packages("lmtest")
library(lmtest)

## now correct the standard errors

coeftest(pmodel0, vcov=vcovBK)


## Lagrange Multiplier test for autocorrelation ##

LMresid <- pmodel0$residuals


mydata1 <- select(mydata,
                  stability_0,
                  cowcode, 
                  year, 
                  e_h_polcon5, 
                  v2xps_party,
                  v2x_corr, 
                  polity,
                  v2mecenefm, 
                  e_peaveduc, 
                  e_migdppcln,
                  e_migdpgro)
mydata1 <-na.omit(mydata1)

mydata1$LMresid <- LMresid
mydata1 <- pdata.frame(mydata1, index = c("cowcode", "year"))

LMtest1 <- plm(stability_0 ~
                 e_h_polcon5 +  
                 v2xps_party +
                 v2x_corr + 
                 polity + 
                 e_peaveduc + 
                 e_migdppcln +
                 e_migdpgro , 
                 data = mydata1, 
                 index = c("cowcode", "year"), 
                 na.action = na.omit, 
                 model = "pooling")

coeftest(LMtest1, vcov=vcovBK)


mydata2 <- filter(mydata1, polity < 7)

LMtest2 <- plm(stability_0 ~
                 e_h_polcon5 +  
                 v2xps_party +
                 v2x_corr + 
                 polity + 
                 e_peaveduc + 
                 e_migdppcln +
                 e_migdpgro , 
               data = mydata2, 
               index = c("cowcode", "year"), 
               na.action = na.omit, 
               model = "within")

a <- vcovBK(LMtest2)
robust_se <- sqrt(diag(a))

coeftest(LMtest2, vcov=vcovBK)

pFtest(LMtest2, LMtest1)
