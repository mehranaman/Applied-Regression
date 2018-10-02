  
#Lab6: Categorical Variables

library(SDSRegressionR)

#Import data...
dent <- read.csv("data/dentalAnxiety.csv", stringsAsFactors=FALSE)
names(dent)
View(dent)


#Recode into dummy variables:
#Education Type
dent$Primary <- NA
dent$Primary[!is.na(dent$Education) ] <- 0
dent$Primary[dent$Education == "1"] <- 1

dent$Secondary <- NA
dent$Secondary[!is.na(dent$Education) ] <- 0
dent$Secondary[dent$Education == "2"] <- 1

dent$High <- NA
dent$High[!is.na(dent$Education) ] <- 0
dent$High[dent$Education == "3"] <- 1

dent$University <- NA
dent$University[!is.na(dent$Education) ] <- 0
dent$University[dent$Education == "4"] <- 1


table(dent$Job, dent$Academic, useNA = "always")
#Run the model (Universtiy as reference)
dentfear <- lm(DFS ~ Age + Sex + BDI + Secondary + High + Primary , data=dent)
summary(dentfear)

#Check the model...
library(car)
vif(dentfear)
residFitted(dentfear)
cooksPlot(dentfear, save.cutoff = TRUE)
threeOuts(dentfear)
cooksCutOff*2
changes <- dfBetas(dentfear)
View(changes)
#Drop the outliers
g_dent <- dent[!row.names(dent) %in% c(22, 51, 48),]

#Rerun
dentfear2 <- lm(DFS ~ Age + Sex + BDI + Secondary + High + Primary , data=g_dent)
summary(dentfear2)

lmBeta(dentfear2)
pCorr(dentfear2)

#Evaluate the Education Type dummies
g_dent$in_fullmod <- tagObs(dentfear2)
g_dent_full <- g_dent[which(g_dent$in_fullmod == 1), ]

dentfear2.1 <- lm(DFS ~ Age + Sex + BDI  , data=g_dent_full)
dentfear2.2 <- lm(DFS ~ Age + Sex + BDI + Secondary + High + Primary , data=g_dent_full)
anova(dentfear2.1, dentfear2.2)

summary(dentfear2.2)$r.squared - summary(dentfear2.1)$r.squared


#Post-hoc exploration
dentfear2 <-lm(DFS ~ Age + Sex + BDI + Secondary + High + Primary , data=g_dent_full)
library(multcomp)
jcomp <- summary(glht(dentfear2, linfct = c("Primary = 0", "Secondary=0", "High=0")))
summary(jcomp, test = adjusted("holm"))

dentfear2b <- lm(DFS ~ Age + Sex + BDI + Secondary + High + University , data=g_dent_full)
jcomp2 <- summary(glht(dentfear2b, linfct = c("University=0", "Secondary=0", "High=0")))
summary(jcomp2, test = adjusted("holm"))
