
##Lab 3: Simple Regression
install.packages("devtools") 
devtools::install_github("MichaelJMahometa/SDSRegressionR")

library(SDSRegressionR)

resil <- read.csv("data/resil.csv", stringsAsFactors=FALSE)
resil <- as.data.frame(resil)
basic <- subset(resil, Group == 'Basic Sciences')


#Define variables that might be of interest
vars <- c("DREEM.A.SP", "MS.QoL")

#New way to get basic descriptives:
library(psych)
describe(basic[,vars], IQR = TRUE)

#Take an intial look at the variables 
cor(basic[,vars], use = "pairwise.complete.obs")

#Check linearity
simpleScatter(basic, DREEM.A.SP, MS.QoL , line=TRUE)

#Run the intial model
dm <- lm(MS.QoL ~ DREEM.A.SP, data=basic)
summary(dm)

#Look for outliers
threeOuts(dm)
cooks.distance(dm)

#Remove the offending outlier
good <- basic[!row.names(basic) %in% (c(680)), ]
cor(good[,vars], use = "pairwise.complete.obs")

#New scatterplot 
simpleScatter(good, DREEM.A.SP, MS.QoL , line=TRUE)

#Re-run the model
dm2 <- lm(MS.QoL ~ DREEM.A.SP, data=good)
summary(dm2)


#Grab confidence intervals
confint(dm2, level = 0.95)

#Test the older study against your hypothesis.
library(car)
linearHypothesis(dm2, "DREEM.A.SP = 0.175")

#Predict new single point
nw_dm <- data.frame(DREEM.A.SP = 10)
predict(dm2, nw_dm, interval="prediction")
