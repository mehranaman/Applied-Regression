#### Here is the R script you will use:  (remember that # indicates a comment) ####
#Lab11: Segmented Regression

library(SDSRegressionR)
#Bring in data
math <- read.csv("data/mathComp.csv", stringsAsFactors = FALSE)
names(math)

#Establish cut-off
cutoff <- 215
View(math)
#Code the data for the cut-off
math$pre1 <- math$pretest #New variable for segment 1 (redundant)
math$pre2 <- math$pretest - cutoff #Variable for the second segment
math$pre2[math$pretest <= cutoff] <- 0 #All before the change point to 0
math$jump <- 0
math$jump[math$pretest >= cutoff] <- 1

count(math, c("pretest","pre1","pre2","jump"))

#Inital model run and diagnostics
full <- lm(posttest ~ pre1 + jump + pre2, data=math)
residFitted(full)
cooksPlot(full, print.obs=TRUE, sort.obs = TRUE)
threeOuts(full)

#Get good data...
g_math <- math[!row.names(math) %in% c(26),]

#Initial look
simpleScatter(math, pretest, posttest, ptalpha = 0.3)

#Look with means
library(doBy)
s <- summaryBy(posttest ~ pretest, math)
simpleScatter(s, pretest, posttest.mean, title="Means Plot")

#Run the model
seg <- lm(posttest ~ pre1 + jump + pre2, data= g_math)
summary(seg)

#Come up with prediction lines
p1 <- data.frame(pre1=c(min(g_math$pretest),cutoff), pre2=0, jump=0)
p1 <- data.frame(p1, predict(seg, p1))
names(p1)[length(p1)] <- "pred"
p2 <- data.frame(pre1=c(cutoff,max(g_math$pre1)), pre2=c(0,max(g_math$pre2)), jump=1)
p2 <- data.frame(p2, predict(seg, p2))
names(p2)[length(p2)] <- "pred"

#Graph it!
g <- simpleScatter(math, pretest, posttest)
g + 
  labs(title="Segmented Regression") +
  geom_vline(xintercept = cutoff, linetype="dashed") +
  geom_line(data=p1, aes(x=pre1, y=pred), color="red") + 
  geom_line(data=p2, aes(x=pre1, y=pred), color="red")

#Code for slope of zero
math$pre1_is <- math$pre1
math$pre1_is[math$pretest >= 215] <- 215

#Re-run model
seg_is <- lm(posttest ~ pre1_is + jump + pre2, data=math)
summary(seg_is)
