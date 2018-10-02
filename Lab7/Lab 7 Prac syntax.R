#### Here is the R script you will use:  (remember that # indicates a comment) ####
#Lab7: Categorical Interaction

library(SDSRegressionR)

#import data...
getwd()
work <- read.csv("data/workers.csv", stringsAsFactors=FALSE)
names(work)
sing <- subset(work, Marital.status=="Single")
names(sing)
View(sing)
#Examine the categorical variable:
table(sing$Job)

#Recode into dummy variables:
#Job Type
sing$Academic <- NA
sing$Academic[!is.na(sing$Job) ] <- 0
sing$Academic[sing$Job == "Academic"] <- 1

sing$Profess <- NA
sing$Profess[!is.na(sing$Job) ] <- 0
sing$Profess[sing$Job == "Professional"] <- 1

sing$Support <- NA
sing$Support[!is.na(sing$Job) ] <- 0
sing$Support[sing$Job == "SupportServices"] <- 1

#Run the model (SupportServices as reference)
hap <- lm(Happiness ~ Age + Female + Academic + Profess + Social.support + Academic*Social.support + Profess*Social.support, data=sing)
summary(hap)

# Check the diagnostics/outliers
residFitted(hap)
library(car)
vif(hap)
cooksPlot(hap, save.cutoff = TRUE)
threeOuts(hap)

g_sing <- sing[!row.names(sing) %in% c(16),]

#Re-run the model
hap2 <- lm(Happiness ~ Age + Female + Academic + Profess + Social.support + 
Academic*Social.support + Profess*Social.support, data=g_sing)
summary(hap2)

#Check the overall interaction significance
hap2.1 <- lm(Happiness ~ Age + Female + Academic + Profess + Social.support, data=g_sing)
hap2.2 <- lm(Happiness ~ Age + Female + Academic + Profess + Social.support + 
               Academic*Social.support + Profess*Social.support, data=g_sing)
anova(hap2.1, hap2.2)

#Simple Slopes
hap2.sup <- lm(Happiness ~ Age + Female + Academic + Profess + Social.support + 
                 Academic*Social.support + Profess*Social.support, data=g_sing)
summary(hap2.sup)
hap2.acad <- lm(Happiness ~ Age + Female + Support + Profess + Social.support + 
                  Support*Social.support + Profess*Social.support, data=g_sing)
summary(hap2.acad)
hap2.prof <- lm(Happiness ~ Age + Female + Academic + Support + Social.support + 
                  Academic*Social.support + Support*Social.support, data=g_sing)
summary(hap2.sup)

#lmDecomp
lmDecomp(hap2.sup, "Social.support", "Academic", mod.type = 2)
lmDecomp(hap2.sup, "Social.support", "Profess", mod.type = 2)

#CI Plot (for fun)
new <- expand.grid(Social.support=0:60, Academic=0:1, Profess=0:1, 
                   Age=mean(g_sing$Age, na.rm=TRUE),
                   Female=mean(g_sing$Female, na.rm=TRUE))
new <- new[!(new$Academic ==1 & new$Profess ==1),]
p <- data.frame(new, pred=predict(hap2.sup, new))
plot(1, type="n", axes=T, xlab="Social Support", ylab="Happiness", 
     xlim=c(0,60), ylim=c(25,80), main="Job Type Interaction")
soc <- p[(p$Academic==0 & p$Profess==0),"Social.support"]
sup_fit <- p[(p$Academic==0 & p$Profess==0),"pred"]
acad_fit <- p[(p$Academic==1 & p$Profess==0),"pred"]
prof_fit <- p[(p$Academic==0 & p$Profess==1),"pred"]
lines(soc, sup_fit, col="blue")
lines(soc, acad_fit, col="red", lty=2)
lines(soc, prof_fit, col="purple", lty=4)
legend("bottomright", c("Support", "Academic", "Professional"), 
       col=c("blue", "red", "purple"), lty=c(1, 2, 3))

ggplot(g_sing,aes(x=Social.support,y=Happiness,color=factor(Job))) +
  stat_smooth(method=lm, se = FALSE, fullrange=TRUE) +
  geom_point() + 
  labs(title="Job Type Interaction", x="Social.support", y="Happiness") +
  theme_bw()
