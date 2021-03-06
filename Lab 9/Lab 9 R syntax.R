#Lab9: Quantitative Interaction

library(SDSRegressionR)
install.packages("devtools") #if needed...
devtools::install_github("MichaelJMahometa/SDSRegressionR")
#Load Data
vit <- read.csv("data/Connected.csv", stringsAsFactors=FALSE)
names(vit)
View(vit)
#Run first model for diagnostics FIRST
#First run
v_mod <- lm(Problem.Beh ~ Depressive.Sympt + School.Connectedness + Family.Connectedness + age + female + School.Connectedness*Depressive.Sympt, data=vit)
summary(v_mod)

#Diagnostics
library(car)
vif(v_mod)
residFitted(v_mod)
cooksPlot(v_mod, print.obs=TRUE, sort.obs=TRUE, save.cutoff = TRUE)
threeOuts(v_mod)
cooksCutOff*2


#Three bad outliers
g_vit <- vit[!row.names(vit) %in% c(21, 88),]

#Center Quant variables to be used in the interaction(s)
g_vit$cen_School.Connectedness <- g_vit$School.Connectedness - mean(g_vit$School.Connectedness, na.rm = TRUE)
g_vit$cen_Depressive.Sympt <- g_vit$Depressive.Sympt - mean(g_vit$Depressive.Sympt, na.rm = TRUE)
View(g_vit)
mean(g_vit$School.Connectedness)
mean(g_vit$Depressive.Sympt)
#Re-run

v_mod2 <- lm(Problem.Beh ~ cen_Depressive.Sympt + cen_School.Connectedness + 
                Family.Connectedness + age + female + cen_School.Connectedness*cen_Depressive.Sympt, data=g_vit)
summary(v_mod2)

#Find the Simple Slope locations (Pick-a-Point)
sd(g_vit$School.Connectedness, na.rm=TRUE)

# And Find the Regions of Significance
lmDecomp(v_mod2, "cen_Depressive.Sympt", "cen_School.Connectedness", mod.values=c(-1.69, 0, 1.69), print.ros=TRUE)

#Put into original scale for perspective
v_mod3 <- lm(D25 ~ D125 + ageyears + bmi + pth + bmi*ageyears + pth*ageyears, data=g_vit)
summary(v_mod3)

mean(g_vit$ageyears, na.rm=TRUE)
sd(g_vit$ageyears, na.rm=TRUE)
mean(g_vit$ageyears, na.rm=TRUE) - sd(g_vit$ageyears, na.rm=TRUE)
mean(g_vit$ageyears, na.rm=TRUE) + sd(g_vit$ageyears, na.rm=TRUE)

lmDecomp(v_mod3, "bmi", "ageyears", mod.values=c(8.19, 9.87, 11.56), print.ros=TRUE)
