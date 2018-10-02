#### Here is the R script you will use:  (remember that # indicates a comment) ####
#Lab10: Mediation Models

library(SDSRegressionR)

#Load Data:
cal <- read.csv("data/cereal.csv", stringsAsFactors = FALSE)
names(cal)
View(cal)
#Full model to look for "baddness"
full <- lm(Calories ~ Sugars + Tot.Carbo, data=cal)
residFitted(full)
cooksPlot(full, print.obs=TRUE, sort.obs=TRUE)
threeOuts(full)

#Take out the Cook's D folks:
g_cal <- cal[!row.names(cal) %in% c(76, 2),]

#Total effect model
pathc <- lm(Calories ~ Tot.Carbo, data=g_cal)
summary(pathc)
lmBeta(pathc)

#Path A
patha <- lm(Sugars ~ Tot.Carbo, data=g_cal)
summary(patha)
lmBeta(patha)

#Path B and Cprime
cprime <- lm(Calories ~ Sugars + Tot.Carbo, data=g_cal)
summary(cprime)
lmBeta(cprime)

#Indirect Effect (Multiply paths a and b - your choice how)
ind <- summary(patha)$coef["Tot.Carbo", "Estimate"] * 
  summary(cprime)$coef["Sugars", "Estimate"]

#Sobel test
se <- sqrt((summary(cprime)$coef["Sugars", "Estimate"]^2 * 
              summary(patha)$coef["Tot.Carbo", "Std. Error"]^2) + 
             (summary(patha)$coef["Tot.Carbo", "Estimate"]^2 * 
                summary(cprime)$coef["Sugars", "Std. Error"]^2))
z <- ind/se
z
(1 - pnorm(z)) * 2

#CIs
lower <- ind - (1.96 * se)
lower
upper <- ind + (1.96 * se)
upper

#Bootstrapping...
K <- rep(NA, 1000)
aval <- rep(NA, 1000)
bval <- rep(NA, 1000)
for(i in 1:length(K)){
  thisdata <- g_cal[sample(nrow(g_cal), nrow(g_cal), replace=TRUE),]
  a <- lm(Sugars ~ Tot.Carbo, data=thisdata)
  b <- lm(Calories ~ Sugars + Tot.Carbo, data=thisdata)
  aval[i] <- summary(a)$coef["Tot.Carbo", "Estimate"]
  bval[i] <- summary(b)$coef["Sugars", "Estimate"]
  K[i] <- summary(a)$coef["Tot.Carbo", "Estimate"] * 
    summary(b)$coef["Sugars", "Estimate"]
}
K <- K[order(K)]
K[0.025*length(K)]
K[0.975*length(K)]
hist(K)
#RMediate (for fun)
library(RMediation)
#Path A
patha <- lm(Sugars ~ Tot.Carbo, data=g_cal)
cprime <- lm(Calories ~ Sugars + Tot.Carbo, data=g_cal)
aval <- summary(patha)$coef["Tot.Carbo", "Estimate"] 
bval <- summary(cprime)$coef["Sugars", "Estimate"]
aval_se <- summary(patha)$coef["Tot.Carbo", "Std. Error"] 
bval_se <- summary(cprime)$coef["Sugars", "Std. Error"]
medci(aval, bval, aval_se, bval_se, plot=TRUE, plotCI=TRUE)
