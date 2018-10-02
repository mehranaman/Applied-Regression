#### Here is the R script you will use:  (remember that # indicates a comment) ####
#Lab5: Sequential Regression
devtools::install_github("MichaelJMahometa/SDSRegressionR")
library(SDSRegressionR)

#import data...
int <- read.csv("data/introverts.csv", stringsAsFactors=FALSE)
names(int)
library(psych)
describe(int$Happiness)
#Determine and run the final model
full <- lm(Happiness ~ Age + ERA + QSR + Neuroticism + Extraversion, data=int)
View(int)
#Look for any issues:
library(car)
vif(full)
residFitted(full)
cooksPlot(full, print.obs = TRUE , save.cutoff = TRUE)
cooksCutOff*3
threeOuts(full)

#Clean up
good_int <- int[!row.names(int) %in% c(13, 140),]

#Re-run the final model
fullg <- lm(Happiness ~ Age + ERA + QSR + Neuroticism + Extraversion, data=good_int)

#Tag observations in the final model
good_int$in_fullg <- tagObs(fullg)
View(good_int)
#Keep just those in the model
good_int_fullg <- good_int[which(good_int$in_fullg == 1), ]
sum(good_int_fullg$in_fullg) #Double check

#Now for the Sequential Regression:
#Model 1:

m1_seq <- lm(Happiness ~ Age + ERA + QSR, data=good_int_fullg)
summary(m1_seq)
summary(m1_seq)$r.squared
lmBeta(m1_seq)
pCorr(m1_seq)

#Model 2:
m2_seq <- lm(Happiness ~ Age + ERA + QSR + Neuroticism + Extraversion, data=good_int_fullg)
summary(m2_seq)
summary(m2_seq)$r.squared
lmBeta(m2_seq)
pCorr(m2_seq)

#Now the Sequential Results
anova(m1_seq, m2_seq)
summary(m2_seq)$r.squared - summary(m1_seq)$r.squared
