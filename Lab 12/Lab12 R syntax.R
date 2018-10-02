#Lab12: Multiple Logistic Regression

library(SDSRegressionR)
#Bring in data
prostate <- read.csv("data/prostate.csv", stringsAsFactors = FALSE)
names(prostate)

#Intital Model
b_mod <- glm(CAPSULE ~ AGE + PSA + VOL + GLEASON, data=prostate, family="binomial")
summary(b_mod)

library(car)
vif(b_mod)
cooksPlot(b_mod, print.obs = TRUE, save.cutoff = TRUE, sort.obs = TRUE)
threeOuts(b_mod)
cooksCutOff*2

#Get good data...
g_prostate <- prostate[!row.names(prostate) %in% c(101, 130, 158, 247, 259),]

#Re-run
b_mod2 <- glm(CAPSULE ~ AGE + PSA + VOL + GLEASON, data=g_prostate, family="binomial")
summary(b_mod2)

#Odds-ratios
exp(b_mod2$coef)
exp(confint.default(b_mod2))

#Stats
library(rms)
b_mod2.2 <- lrm(CAPSULE ~ AGE + PSA + VOL + GLEASON, g_prostate)
b_mod2.2

#Examine the variables of interest graphically...
#Look at ranges...
summary(g_prostate)  

#PSA score that marks the 50% probability
alpha <- summary(b_mod2)$coef["(Intercept)", "Estimate"]
beta <- summary(b_mod2)$coef["PSA", "Estimate"]
beta2 <-  (summary(b_mod2)$coef["AGE", "Estimate"] * mean(g_prostate$AGE, na.rm=TRUE)) +
  (summary(b_mod2)$coef["VOL", "Estimate"] * mean(g_prostate$VOL, na.rm=TRUE)) +
  (summary(b_mod2)$coef["GLEASON", "Estimate"] * mean(g_prostate$GLEASON, na.rm=TRUE))

mark <- (-alpha - beta2)/beta

x <- (log(.5 / (1-.5)) - alpha - beta2) / beta 
x  

#Gleason score that marks the 50% probability
alpha <- summary(b_mod2)$coef["(Intercept)", "Estimate"]
beta <- summary(b_mod2)$coef["GLEASON", "Estimate"]
beta2 <-  (summary(b_mod2)$coef["AGE", "Estimate"] * mean(g_prostate$AGE, na.rm=TRUE)) +
  (summary(b_mod2)$coef["VOL", "Estimate"] * mean(g_prostate$VOL, na.rm=TRUE)) +
  (summary(b_mod2)$coef["PSA", "Estimate"] * mean(g_prostate$PSA, na.rm=TRUE))

mark <- (-alpha - beta2)/beta

x <- (log(.5 / (1-.5)) - alpha - beta2) / beta 
x

#Predict for PSA
new <- data.frame(AGE = mean(g_prostate$AGE, na.rm=TRUE),
                  PSA = seq(0.30, 126, 10),
                  GLEASON = mean(g_prostate$GLEASON, na.rm=TRUE),
                  VOL = mean(g_prostate$VOL, na.rm=TRUE))
pred <- data.frame(new, pred = predict(b_mod2, newdata = new, type="link", se.fit=TRUE))
pred <- data.frame(pred, p.fit = plogis(pred$pred.fit), 
                   LL = plogis(pred$pred.fit - (1.96*pred$pred.se.fit)),
                   UL = plogis(pred$pred.fit + (1.96*pred$pred.se.fit)))

#Graph for PSA
g <- simpleScatter(g_prostate, PSA, CAPSULE, title="Prostatic Specific Atigen Value", xlab="PSA", ylab="Probability of Tumor Cell Penetration")
g + 
  geom_line(data=pred, aes(x=PSA, y=p.fit), color="red") + 
  geom_line(data=pred, aes(x=PSA, y=LL), linetype="dashed") +
  geom_line(data=pred, aes(x=PSA, y=UL), linetype="dashed")  

#Predict for GLEASON
new <- data.frame(AGE = mean(g_prostate$AGE, na.rm=TRUE),
                  GLEASON = seq(0, 9.0, 1),
                  VOL = mean(g_prostate$VOL, na.rm=TRUE), 
                  PSA = mean(g_prostate$PSA, na.rm=TRUE))
pred <- data.frame(new, pred = predict(b_mod2, newdata = new, type="link", se.fit=TRUE))
pred <- data.frame(pred, p.fit = plogis(pred$pred.fit), 
                   LL = plogis(pred$pred.fit - (1.96*pred$pred.se.fit)),
                   UL = plogis(pred$pred.fit + (1.96*pred$pred.se.fit)))

#Graph for GLEASON
g <- simpleScatter(g_prostate, GLEASON, CAPSULE, title="GLEASON SCORE", xlab="GLEASON", ylab="Probability of Tumor Cell Penetration ")
g + 
  geom_line(data=pred, aes(x=GLEASON, y=p.fit), color="red") + 
  geom_line(data=pred, aes(x=GLEASON, y=LL), linetype="dashed") +
  geom_line(data=pred, aes(x=GLEASON, y=UL), linetype="dashed")
