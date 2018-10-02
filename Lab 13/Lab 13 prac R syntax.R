#### Here is the R script you will use:  (remember that # indicates a comment) ####
#Lab13: Multinomial Logistic Regression

library(SDSRegressionR)

#Data
buy <- read.csv("data/purchases.csv", stringsAsFactors = FALSE)
table(buy$Purchase)

#Model
install.packages("nnet")
library(nnet)
buy$Purchase_f <- factor(buy$Purchase, levles = c("Leave Item", "Buy Item","Wish List"))
m_buy <- multinom(Purchase ~ Usefulness + Packaging + Price, data = buy)
summary(m_buy)

#Odds-ratios
exp(coef(m_buy))

#Individual paramerters
z <- summary(m_buy)$coefficients/summary(m_buy)$standard.errors
z
p <- (1 - pnorm(abs(z), 0, 1))*2
p
round(p, 6)

#Overall
x2 <- deviance(multinom(Purchase~1, data=buy)) - deviance(m_buy)
x2
pchisq(x2, 6, lower.tail=FALSE)

#Change the baseline:
buy$Purchase_Leave <- factor(buy$Purchase, levels=c("Leave Item", "Buy Item", "Wish List"))

#Model_Leave
m_buy_l <- multinom(Purchase_Leave ~ Usefulness + Packaging + Price, data = buy)
summary(m_buy_l)

#Odds-ratios_Leave
exp(coef(m_buy_l))

#Individual paramerters_Leave
z <- summary(m_buy_l)$coefficients/summary(m_buy_l)$standard.errors
z
p <- (1 - pnorm(abs(z), 0, 1))*2
p
round(p, 6)

#Overall_Leave
x2 <- deviance(multinom(Purchase_Leave~1, data=buy)) - deviance(m_buy_l)
x2
pchisq(x2, 6, lower.tail=FALSE)

#Graphing (Original)
fivenum(buy$Usefulness)
new <- data.frame(Usefulness = seq(0, 10, 1),
                  Packaging = mean(buy$Packaging, na.rm=TRUE), 
                  Price = mean(buy$Price, na.rm=TRUE))
pred <- data.frame(new, pred = predict(m_buy, newdata = new, type="prob"))
head(pred)

library(reshape2)
pred_long <- melt(pred, id.vars=c("Usefulness"),
                  measure.vars=c("pred.Buy.Item", 
                                 "pred.Leave.Item", 
                                 "pred.Wish.List"),
                  variable.name = "outcome",
                  value.name = "pred")

levels(pred_long$outcome)[levels(pred_long$outcome)=="pred.Buy.Item"] <- "Buy Item"
levels(pred_long$outcome)[levels(pred_long$outcome)=="pred.Leave.Item"] <- "Leave Item"
levels(pred_long$outcome)[levels(pred_long$outcome)=="pred.Wish.List"] <- "Wish List"

ggplot(pred_long, aes(y=pred, x=Usefulness, color=outcome)) +
  geom_line() +
  labs(title="Usefulness impact") +
  theme_bw()

fivenum(buy$Price)
new <- data.frame(Price = seq(0, 10, 1),
                  Packaging = mean(buy$Packaging, na.rm=TRUE), 
                  Usefulness = mean(buy$Usefulness, na.rm=TRUE))
pred <- data.frame(new, pred = predict(m_buy, newdata = new, type="prob"))
head(pred)

library(reshape2)
pred_long <- melt(pred, id.vars=c("Price"),
                  measure.vars=c("pred.Buy.Item", 
                                 "pred.Leave.Item", 
                                 "pred.Wish.List"),
                  variable.name = "outcome",
                  value.name = "pred")

levels(pred_long$outcome)[levels(pred_long$outcome)=="pred.Buy.Item"] <- "Buy Item"
levels(pred_long$outcome)[levels(pred_long$outcome)=="pred.Leave.Item"] <- "Leave Item"
levels(pred_long$outcome)[levels(pred_long$outcome)=="pred.Wish.List"] <- "Wish List"

ggplot(pred_long, aes(y=pred, x=Price, color=outcome)) +
  geom_line() +
  labs(title="Price impact") +
  theme_bw()