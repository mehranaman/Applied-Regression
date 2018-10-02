#### Here is the R script you will use:  (remember that # indicates a comment) ####
##Lab 3: Simple Regression
install.packages("devtools") #if needed...
devtools::install_github("MichaelJMahometa/SDSRegressionR")

library(SDSRegressionR)

intel <- read.csv("data/intelligence.csv", stringsAsFactors=FALSE)

#Scale MRI to something OK....
fivenum(intel$MRIcount)
intel$mri_100 <- intel$MRIcount / 100000

#Define variables that might be of interest
vars <- c("PIQ", "mri_100", "Weight", "Height")

#New way to get basic descriptives:
library(psych)
describe(intel[,vars], IQR = TRUE)

#Take an intial look at the variables (including PIQ and MRI)
cor(intel[,vars], use = "pairwise.complete.obs")

#Check linearity
simpleScatter(intel, mri_100, PIQ, line=TRUE)

#Run the intial model
iq <- lm(PIQ ~ mri_100, data=intel)
summary(iq)

#Look for outliers
threeOuts(iq)
cooks.distance(iq)
              
#Remove the offending outlier
intel_noout <- intel[!row.names(intel) %in% c(23), ]
intel[23,]
#Re-run the model
iq2 <- lm(PIQ ~ mri_100, data=intel_noout)
summary(iq2)
#Grab confidence intervals
confint(iq2)

#Test the additional question
library(car)
linearHypothesis(iq2, "mri_100 = 10.00")

#Predict new MRI of 1,000,000
nw_mri <- data.frame(mri_100 = 10)
predict(iq2, nw_mri, interval="prediction")
