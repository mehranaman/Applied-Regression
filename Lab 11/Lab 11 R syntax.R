#### Here is the R script you will use:  (remember that # indicates a comment) ####
#Lab11: Segmented Regression

library(SDSRegressionR)
#Bring in data
gss <- read.csv("data/gss.csv", stringsAsFactors = FALSE)
names(gss)

#Establish cut-off
cutoff <- 12
View(gss)
#Code the data for the cut-off
gss$educ1 <- gss$educ #New variable for segment 1 (redundant)
gss$educ2 <- gss$educ - cutoff #Variable for the second segment
gss$educ2[gss$educ <= cutoff] <- 0 #All before the change point to 0
gss$jump <- 0
gss$jump[gss$educ >= cutoff] <- 1

count(gss, c("number","number1","number2","jump"))

#Inital model run and diagnostics
full <- lm(realrinc ~ educ1 + jump + educ2, data=gss)
residFitted(full)
cooksPlot(full, print.obs=TRUE, sort.obs = TRUE)
threeOuts(full)

#Get good data...
g_gss <- gss[!row.names(gss) %in% c(327, 971,1178,338, 732, 1637),]

#Initial look
simpleScatter(g_gss, educ, realrinc, ptalpha = 0.3)

#Look with means
library(doBy)
s <- summaryBy(realrinc ~ educ, g_gss)
simpleScatter(s, educ, realrinc.mean, title="Means Plot")

#Run the model
seg <- lm(realrinc ~ educ1 + jump + educ2, data= g_gss)
summary(seg)

#Come up with prediction lines
p1 <- data.frame(educ1=c(min(g_gss$educ),cutoff), educ2=0, jump=0)
p1 <- data.frame(p1, predict(seg, p1))
names(p1)[length(p1)] <- "pred"
p2 <- data.frame(educ1=c(cutoff,max(g_gss$educ1)),educ2=c(0,max(g_gss$educ2)), jump=1)
p2 <- data.frame(p2, predict(seg, p2))
names(p2)[length(p2)] <- "pred"

#Graph it!
g <- simpleScatter(g_gss, educ, realrinc)
g + 
  labs(title="Segmented Regression") +
  geom_vline(xintercept = cutoff, linetype="dashed") +
  geom_line(data=p1, aes(x=educ1, y=pred), color="red") + 
  geom_line(data=p2, aes(x=educ1, y=pred), color="red")

#Code for slope of zero
g_gss$educ1_is <- g_gss$educ1
g_gss$educ1_is[g_gss$educ >= 12] <- 12

#Re-run model
seg_is <- lm(realrinc ~ educ1_is + jump + educ2, data=g_gss)
summary(seg_is)

vif(seg_is)
