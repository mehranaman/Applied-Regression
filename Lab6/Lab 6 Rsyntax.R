#### Here is the R script you will use:  (remember that # indicates a comment) ####
#Lab6: Categorical Variables

library(SDSRegressionR)

#Import data...

dent <- read.csv("data/dentalAnxiety.csv", stringsAsFactors=FALSE)
names(dent)
dentsub1 <- subset(dent, Education=="1")
dentsub2 <- subset(dent, Education=="1")
dentsub3 <- subset(dent, Education=="1")
dentsub4 <- subset(dent, Education=="1")
View(dent)
#Examine the categorical variable:
table(sing$Job)
library(plyr)
dent$Education <- revalue(dent$Education, c("(1) 1"="Primary Education", "(2) 2"="Secondary school", "(3) 3"="High school ", "(4) 4"="University"))

my.data2$islamic_leviathan_score_1 <- as.numeric(as.character(my.data2$islamic_leviathan_score))
#Recode into dummy variables:
#Job Type
dent$1 <- NA
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
dentt <- lm(DFS ~ Age + Sex + BDI + , data=dent)
summary(dentt)

#Check the model...
library(car)
vif(dentt)
residFitted(dentt)
cooksPlot(dentt, save.cutoff = TRUE)
threeOuts(dentt)
cooksCutOff*2
changes <- dfBetas(dentt)
View(changes)
#Drop the outliers
g_dent <- dent[!row.names(dent) %in% c(51, 124),]

#Rerun
dentt2 <- lm(DFS ~ Age + Sex + BDI + Education, data=g_dent)
summary(dentt2)

lmBeta(dentt2)
pCorr(dentt2)

#Evaluate the Job Type dummies
g_dent$in_fullmod <- tagObs(dentt2)
g_dent_full <- g_dent[which(g_dent$in_fullmod == 1), ]

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
