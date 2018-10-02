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
v_mod <- lm(D25 ~ D125 + ageyears + bmi + pth + bmi*ageyears + pth*ageyears, data=vit)
summary(v_mod)

#Diagnostics
library(car)
vif(v_mod)
residFitted(v_mod)
cooksPlot(v_mod, print.obs=TRUE, sort.obs=TRUE)
threeOuts(v_mod)


#Three bad outliers
g_vit <- vit[!row.names(vit) %in% c(182, 28),]

#Center Quant variables to be used in the interaction(s)
g_vit$cen_age <- g_vit$ageyears - mean(g_vit$ageyears, na.rm = TRUE)
g_vit$cen_bmi <- g_vit$bmi - mean(g_vit$bmi, na.rm = TRUE)
g_vit$cen_pth <- g_vit$pth - mean(g_vit$pth, na.rm = TRUE)

#Re-run
v_mod2 <- lm(D25 ~ D125 + cen_age + cen_bmi + cen_pth + cen_bmi*cen_age + 
               cen_pth*cen_age, data=g_vit)
summary(v_mod2)

#Find the Simple Slope locations (Pick-a-Point)
sd(g_vit$ageyears, na.rm=TRUE)

# And Find the Regions of Significance
lmDecomp(v_mod2, "cen_bmi", "cen_age", mod.values=c(-1.68, 0, 1.68), print.ros=TRUE)

#Put into original scale for perspective
v_mod3 <- lm(D25 ~ D125 + ageyears + bmi + pth + bmi*ageyears + pth*ageyears, data=g_vit)
summary(v_mod3)

mean(g_vit$ageyears, na.rm=TRUE)
sd(g_vit$ageyears, na.rm=TRUE)
mean(g_vit$ageyears, na.rm=TRUE) - sd(g_vit$ageyears, na.rm=TRUE)
mean(g_vit$ageyears, na.rm=TRUE) + sd(g_vit$ageyears, na.rm=TRUE)

lmDecomp(v_mod3, "bmi", "ageyears", mod.values=c(8.19, 9.87, 11.56), print.ros=TRUE)
