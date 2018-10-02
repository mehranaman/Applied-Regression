#### Here is the R script you will use:  (remember that # indicates a comment) ####
#Lab13: Multinomial Logistic Regression

install.packages("devtools") #if needed...
devtools::install_github("MichaelJMahometa/SDSRegressionR")
library(SDSRegressionR) 
install.packages("reshape2")

#Data
employ <- read.csv("data/employ.csv", stringsAsFactors = FALSE)
table(employ$empl)

#Model
library(nnet)  
employ$empl_f <- factor(employ$empl, levels=c("0", "1", "2"))
m_employ <- multinom(empl_f ~ age + mhs + fhs + adjinc + wtest, data = employ)
summary(m_employ)

#Odds-ratios
exp(coef(m_employ))

#Individual paramerters
z <- summary(m_employ)$coefficients/summary(m_employ)$standard.errors
z
p <- (1 - pnorm(abs(z), 0, 1))*2
p
round(p, 6)

#Overall
x2 <- deviance(multinom(empl~1, data=employ)) - deviance(m_employ)
summary(multinom(empl~1, data=employ))
x2
pchisq(x2, 6, lower.tail=FALSE)

#Change the baseline:
employ$empl_0 <- factor(employ$empl, levels=c("0", "1", "2"))

#Model_0
m_employ_l <- multinom(empl_0 ~ age + mhs + fhs + adjinc + wtest, data = employ)
summary(m_employ_l)

#Odds-ratios_0
exp(coef(m_employ_l))

#Individual parameters_0
z <- summary(m_employ_l)$coefficients/summary(m_employ_l)$standard.errors
z
p <- (1 - pnorm(abs(z), 0, 1))*2
p
round(p, 6)

#Overall_0
x2 <- deviance(multinom(empl_0~1, data=employ)) - deviance(m_employ_l)
x2
pchisq(x2, 6, lower.tail=FALSE)

#Graphing (Original)
fivenum(employ$wtest)
new <- data.frame(wtest = seq(1),
                  age = mean(employ$age, na.rm=TRUE), 
                  mhs = mean(employ$mhs, na.rm=TRUE),
                  fhs = mean(employ$fhs, na.rm=TRUE),
                  adjinc = mean(employ$adjinc, na.rm=TRUE))
pred <- data.frame(new, pred = predict(m_employ, newdata = new, type="prob"))
head(pred)

