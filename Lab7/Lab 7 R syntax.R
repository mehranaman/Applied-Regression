#Lab7: Categorical Interaction

library(SDSRegressionR)

#import data...
state <- read.csv("data/stateAnx.csv", stringsAsFactors=FALSE)
names(state)

View(state)
#Examine the categorical variable:
table(state$Group) 

#Recode into dummy variables:
#Group
state$Basic_Sciences <- NA
state$Basic_Sciences[!is.na(state$Group) ] <- 0
state$Basic_Sciences[state$Group == "Basic Sciences"] <- 1

state$Clinical_Sciences <- NA
state$Clinical_Sciences[!is.na(state$Group) ] <- 0
state$Clinical_Sciences[state$Group == "Clinical Sciences"] <- 1


#Run the model (Clincical_Sciences as reference)
hap <- lm(State.Anxiety ~ Age + BDI + Basic_Sciences + WHOQOL.PSY + Basic_Sciences*WHOQOL.PSY, data=state)
summary(hap)

# Check the diagnostics/outliers
residFitted(hap)
library(car)
vif(hap)
cooksPlot(hap, save.cutoff = TRUE)
threeOuts(hap)

g_state <- state[!row.names(state) %in% c(338, 42, 164, 152, 122),]

#Re-run the model
hap2 <- lm(State.Anxiety ~ Age + BDI + Basic_Sciences + WHOQOL.PSY + Basic_Sciences*WHOQOL.PSY, data= g_state)
summary(hap2)


#Check the overall interaction significance
hap2.1 <- lm(State.Anxiety ~ Age + BDI + Basic_Sciences + WHOQOL.PSY, data=g_state)
hap2.2 <- lm(State.Anxiety ~ Age + BDI + Basic_Sciences + WHOQOL.PSY + Basic_Sciences*WHOQOL.PSY, data= g_state)

anova(hap2.1, hap2.2)

#Simple Slopes
hap2.bs <- lm(State.Anxiety ~ Age + BDI + Clinical_Sciences + WHOQOL.PSY + Clinical_Sciences*WHOQOL.PSY, data= g_state)
summary(hap2.bs)
hap2.cs <- lm(State.Anxiety ~ Age + BDI + Basic_Sciences + WHOQOL.PSY + Basic_Sciences*WHOQOL.PSY, data= g_state)
summary(hap2.cs)


#lmDecomp
lmDecomp(hap2, "WHOQOL.PSY", "Basic_Sciences", mod.type=2, print.ros = TRUE)

ggplot(g_state,aes(x=WHOQOL.PSY,y=State.Anxiety,color=factor(Group))) +
  stat_smooth(method=lm, se = FALSE, fullrange=TRUE) +
  geom_point() + 
  labs(title="Group type interaction", x="WHOQOL.PSY", y="State.Anxiety") +
  theme_bw()


