library(ggplot2)
library(plm)
library(dplyr)
library(tidyr)
library(stargazer)
library(pcse)
mydata <- read.csv("https://raw.githubusercontent.com/haowang666/Veto-Fiscal/master/data/mydata.csv")
#add polity
mydata$polity <- mydata$e_democ - mydata$e_autoc



##Hausman test
pmodel0 <- plm(stability_0 ~ 
                 e_h_polcon5 +  
                 v2xps_party +
                 v2dlconslt +
                 v2x_feduni + 
                 v2x_corr + 
                 polity + 
                 v2mecenefm + 
                 e_peaveduc + 
                 e_migdppcln +
                 e_migdpgro +
                 e_Civil_War, 
               data = mydata, index = c("cowcode", "year"), na.action = na.omit, model = "within")


pmodel0_rd <- plm(stability_0_100 ~ 
                 e_h_polcon5 +  
                 v2xps_party +
                 v2x_feduni + 
                 v2x_corr, 
               data = mydata, index = c("cowcode", "year"), na.action = na.omit, model = "within", effect = "twoways")
summary(pmodel0_rd)



pFtest(pmodel0, pmodel0_rd)




## Fixed Effect
#-----------------------------------------------------------------------------
pmodel0 <- plm(stability_0_aj ~ 
                 e_h_polcon5 +  
                 v2xps_party +
                 v2dlconslt +
                 v2x_feduni + 
                 v2x_corr + 
                 polity + 
                 v2mecenefm + 
                 e_peaveduc + 
                 e_migdppcln +
                 e_migdpgro +
                 e_Civil_War, 
               data = mydata, index = c("cowcode", "year"), na.action = na.omit, model = "within")

pmodel1 <- plm(stability_1_aj ~ 
                 e_h_polcon5 +  
                 v2xps_party +
                 v2dlconslt +
                 v2x_feduni + 
                 v2x_corr + 
                 polity + 
                 v2mecenefm + 
                 e_peaveduc + 
                 e_migdppcln + 
                 e_migdpgro +
                 e_Civil_War ,  
               data = mydata, index = c("cowcode", "year"), na.action = na.omit, model = "within")


pmodel2 <- plm(stability_2_aj ~ 
                 e_h_polcon5 +  
                 v2xps_party +
                 v2dlconslt +
                 v2x_feduni + 
                 v2x_corr + 
                 polity + 
                 v2mecenefm + 
                 e_peaveduc + 
                 e_migdppcln + 
                 e_migdpgro +
                 e_Civil_War ,  
               data = mydata, index = c("cowcode", "year"), na.action = na.omit, model = "within")


pmodel3 <- plm(stability_3_aj ~ 
                 e_h_polcon5 +  
                 v2xps_party +
                 v2dlconslt +
                 v2x_feduni + 
                 v2x_corr + 
                 polity + 
                 v2mecenefm + 
                 e_peaveduc + 
                 e_migdppcln + 
                 e_migdpgro +
                 e_Civil_War ,  
               data = mydata, index = c("cowcode", "year"), na.action = na.omit, model = "within")


pmodel4 <- plm(stability_4_aj ~ 
                 e_h_polcon5 +  
                 v2xps_party +
                 v2dlconslt +
                 v2x_feduni + 
                 v2x_corr + 
                 polity + 
                 v2mecenefm + 
                 e_peaveduc + 
                 e_migdppcln + 
                 e_migdpgro +
                 e_Civil_War ,  
               data = mydata, index = c("cowcode", "year"), na.action = na.omit, model = "within")

pmodel5 <- plm(stability_5_aj ~ 
                 e_h_polcon5 +  
                 v2xps_party +
                 v2dlconslt +
                 v2x_feduni + 
                 v2x_corr + 
                 polity + 
                 v2mecenefm + 
                 e_peaveduc + 
                 e_migdppcln + 
                 e_migdpgro +
                 e_Civil_War ,  
               data = mydata, index = c("cowcode", "year"), na.action = na.omit, model = "within")


stargazer(pmodel0, pmodel1, pmodel2, pmodel3, pmodel4, pmodel5, 
          header = FALSE, 
          title = "Fixed Effect Regression Results", 
          label = "fe",
          omit = c("e_Civil_War", "e_migdpgro"), 
          dep.var.labels.include = FALSE,
          covariate.labels = c("Constraints", "Party", "Consult", 
                               "Federalism", "Corruption", "Polity", "Censorship",
                               "Education", "GDP pc"),
          omit.stat = c("adj.rsq", "f", "res.dev", "ser"),
          notes = "DV: Stability Index, higher value indicates more volatile policies")

#----------------------------------------------------------------------


#This is crazy

lm <- plm(formula = stability_0 ~ e_h_polcon5 + v2xps_party + 
      v2x_feduni + v2x_corr, data = mydata.trim1, na.action = na.omit, 
    effect = "twoway", model = "within", index = c("cowcode", 
                                                   "year"))
summary(lm)
vcov(lm)

polity <- seq(from = -10, to = 10, by = 0.01)
df <- as.data.frame(polity)
var <- vcov(lm)[1,1] + 2*vcov(lm)[1,6]*polity + vcov(lm)[2,2]*polity^2 
se  <- sqrt(var)
df$mar <- lm$coefficients[1] + lm$coefficients[6]*polity
#calculate the 95% CI of the marginal effect
df$up <- df$mar + 1.96*se
df$low <- df$mar - 1.96*se



#graphic presentation
ggplot(data=df, aes(x = polity))+ 
  geom_line(aes(y =mar)) + 
  geom_line(aes(y =up), linetype="dashed", color="blue") + 
  geom_line(aes(y =low), linetype="dashed", color="blue") + 
  geom_line(aes(y =0), color ="red", linetype="dashed") +
  xlab("Level of X (moderator) (95% CI)") + 
  ylab("Marginal Effects") + 
  ggtitle("Marginal Effect of D at Different Levels of X")+ 
  geom_rug(sides="b")+
  geom_ribbon(aes(ymin=up, ymax=low), alpha=0.2)


library(interflex)

inter.raw(Y = "stability_0", D = "e_h_polcon5", X = "polity", data = mydata, Ylabel = "Outcome", Dlabel = "Treatment", Xlabel="Moderator")



