#### Here is the R script you will use:  (remember that # indicates a comment) ####
#Lab6: Categorical Variables

library(SDSRegressionR)

#Import data...
work <- read.csv("data/workers.csv", stringsAsFactors=FALSE)
names(work)
sing <- subset(work, Marital.status=="Single")
View(sing)
#Examine the categorical variable:
table(sing$Job)

#Recode into dummy variables:
#Job Type
sing$Academic <- NA
sing$Academic[!is.na(sing$Job) ] <- 0
sing$Academic[sing$Job == "Academic"] <- 1

sing$Professional <- NA
sing$Professional[!is.na(sing$Job) ] <- 0
sing$Professional[sing$Job == "Professional"] <- 1

sing$SupportServices <- NA
sing$SupportServices[!is.na(sing$Job) ] <- 0
sing$SupportServices[sing$Job == "SupportServices"] <- 1


table(sing$Job, sing$Academic, useNA = "always")
#Run the model (SupportServices as reference)
hap <- lm(Happiness ~ Age + Female + Have.child + SupportServices + Professional + Social.support, data=sing)
summary(hap)

#Check the model...
library(car)
vif(hap)
residFitted(hap)
cooksPlot(hap, save.cutoff = TRUE)
threeOuts(hap)
changes <- dfBetas(hap)
View(changes)
#Drop the outliers
g_sing <- sing[!row.names(sing) %in% c(11),] #Let's discuss....

#Rerun
hap2 <- lm(Happiness ~ Age + Female + Have.child + Academic + Professional +
             Social.support, data=g_sing)
summary(hap2)

lmBeta(hap2)
pCorr(hap2)

#Evaluate the Job Type dummies
g_sing$in_fullmod <- tagObs(hap2)
g_sing_full <- g_sing[which(g_sing$in_fullmod == 1), ]

hap2.1 <- lm(Happiness ~ Age + Female + Have.child +
               Social.support, data=g_sing_full)
hap2.2 <- lm(Happiness ~ Age + Female + Have.child + Academic + Professional +
               Social.support, data=g_sing_full)
anova(hap2.1, hap2.2)

summary(hap2.2)$r.squared - summary(hap2.1)$r.squared


#Post-hoc exploration
hap2 <- lm(Happiness ~ Age + Female + Have.child + Academic + Professional +
             Social.support, data=g_sing_full)
library(multcomp)
jcomp <- summary(glht(hap2, linfct = c("Academic=0", "Professional=0")))
summary(jcomp, test = adjusted("holm"))

hap2b <- lm(Happiness ~ Age + Female + Have.child + Academic + SupportServices +
              Social.support, data=g_sing_full)
jcomp2 <- summary(glht(hap2b, linfct = c("Academic=0", "SupportServices=0")))
summary(jcomp2, test = adjusted("holm"))
