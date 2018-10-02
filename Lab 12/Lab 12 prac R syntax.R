#### Here is the R script you will use:  (remember that # indicates a comment) ####
#Lab12: Multiple Logistic Regression

getwd()

library(SDSRegressionR)
#Bring in data
birth <- read.csv("data/LowBirth.csv", stringsAsFactors = FALSE)
names(birth)

#Intital Model
b_mod <- glm(LOW ~ AGE + LWT + SMOKE, data=birth, family="binomial")
summary(b_mod)

library(car)
vif(b_mod)
cooksPlot(b_mod, print.obs = TRUE, sort.obs = TRUE)
threeOuts(b_mod)

#Get good data...
g_birth <- birth[!row.names(birth) %in% c(21, 140, 11, 159),]

#Re-run
g_birth$SMOKE <- factor(g_birth$SMOKE)
b_mod2 <- glm(LOW ~ AGE + LWT + SMOKE, data=g_birth, family="binomial")
summary(b_mod2)

#Odds-ratios
exp(b_mod2$coef)
exp(confint.default(b_mod2))

#Stats
library(rms)
b_mod2.2 <- lrm(LOW ~ AGE + LWT + SMOKE, g_birth)
b_mod2.2

#Examine the variables of interest graphically...
#Look at ranges...
summary(g_birth)

#Predict
new <- data.frame(AGE = mean(g_birth$AGE, na.rm=TRUE),
                  LWT = seq(80, 250, 10),
                  SMOKE = mean(g_birth$SMOKE, na.rm=TRUE))
pred <- data.frame(new, pred = predict(b_mod2, newdata = new, type="link", se.fit=TRUE))
pred <- data.frame(pred, p.fit = plogis(pred$pred.fit), 
                   LL = plogis(pred$pred.fit - (1.96*pred$pred.se.fit)),
                   UL = plogis(pred$pred.fit + (1.96*pred$pred.se.fit)))

#Graph
g <- simpleScatter(g_birth, LWT, LOW, title="Low birth weight", xlab="Mother's weight", ylab="Low birth weight probability")
g + 
  geom_line(data=pred, aes(x=LWT, y=p.fit), color="red") + 
  geom_line(data=pred, aes(x=LWT, y=LL), linetype="dashed") +
  geom_line(data=pred, aes(x=LWT, y=UL), linetype="dashed")

#Out of curiosity...
new <- data.frame(AGE = mean(g_birth$AGE, na.rm=TRUE),
                  LWT = mean(g_birth$LWT, na.rm=TRUE),
                  SMOKE = c(0,1))
pred <- data.frame(new, pred = predict(b_mod2, newdata = new, type="link", se.fit=TRUE))
pred <- data.frame(pred, p.fit = plogis(pred$pred.fit), 
                   LL = plogis(pred$pred.fit - (1.96*pred$pred.se.fit)),
                   UL = plogis(pred$pred.fit + (1.96*pred$pred.se.fit)))
pred

ggplot(pred, aes(y=p.fit, x=factor(SMOKE))) +
  geom_point(size=2) +
  geom_errorbar(aes(ymin=LL, ymax=UL), width=.1) +
  ylim(0,1) +
  labs(title="Smoking and low birth \n 95% CI") +
  geom_hline(yintercept = 0.5, color="red") + 
  theme_bw()