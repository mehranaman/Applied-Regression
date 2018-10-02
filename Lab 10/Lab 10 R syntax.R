#### Here is the R script you will use:  (remember that # indicates a comment) ####
#Lab10: Mediation Models

library(SDSRegressionR)

#Load Data:
sci <- read.csv("data/hsKnowledge.csv", stringsAsFactors = FALSE)
names(sci)
View(sci)
#Full model to look for "baddness"
full <- lm(science ~ math + read + write + socst, data=sci)
residFitted(full)
cooksPlot(full, print.obs=TRUE, sort.obs=TRUE)
threeOuts(full)

#Take out the Cook's D folks:
g_sci <- sci[!row.names(sci) %in% c(81, 64),]

#Total effect model
pathc <- lm(science ~ math + write + socst, data=g_sci)
summary(pathc)
lmBeta(pathc)

#Path A
patha <- lm(read ~ math + write + socst, data=g_sci)
summary(patha)
lmBeta(patha)

#Path B and Cprime
cprime <- lm(science ~ read + math +write + socst, data=g_sci)
summary(cprime)
lmBeta(cprime)

#Indirect Effect (Multiply paths a and b - your choice how)
ind <- summary(patha)$coef["math", "Estimate"] * 
  summary(cprime)$coef["read", "Estimate"]

#Sobel test
se <- sqrt((summary(cprime)$coef["read", "Estimate"]^2 * 
              summary(patha)$coef["math", "Std. Error"]^2) + 
             (summary(patha)$coef["math", "Estimate"]^2 * 
                summary(cprime)$coef["read", "Std. Error"]^2))
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
  thisdata <- g_sci[sample(nrow(g_sci), nrow(g_sci), replace=TRUE),]
  a <- lm(read ~math, data=thisdata)
  b <- lm(science ~ read + math, data=thisdata)
  aval[i] <- summary(a)$coef["math", "Estimate"]
  bval[i] <- summary(b)$coef["read", "Estimate"]
  K[i] <- summary(a)$coef["math", "Estimate"] * 
    summary(b)$coef["read", "Estimate"]
}
K <- K[order(K)]
K[0.025*length(K)]
K[0.975*length(K)]
hist(K)
#RMediate (for fun)
library(RMediation)
#Path A
patha <- lm(read ~ math, data=g_sci)
cprime <- lm(science ~ read + math, data=g_sci)
aval <- summary(patha)$coef["math", "Estimate"] 
bval <- summary(cprime)$coef["read", "Estimate"]
aval_se <- summary(patha)$coef["math", "Std. Error"] 
bval_se <- summary(cprime)$coef["read", "Std. Error"]
medci(aval, bval, aval_se, bval_se, plot=TRUE, plotCI=TRUE)

