mydata <- read.csv("https://raw.githubusercontent.com/haowang666/Veto-Fiscal/master/data/mydata.csv")
#add polity
mydata$polity <- mydata$e_democ - mydata$e_autoc

# Load LK score
d <- read.csv("https://raw.githubusercontent.com/haowang666/Veto-Fiscal/master/data/LKscore.csv")
# get rid of "X"
install.packages('dplyr')
library(dplyr)
d <- select(d, -X)
LK <- select(d, country, cowcode, y_lk0)
LK$polity <- d$e_democ - d$e_autoc
