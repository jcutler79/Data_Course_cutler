### Biostats Methods II

## Website for ALSM datasets:
# http://users.stat.ufl.edu/~rrandles/sta4210/Rclassnotes/data/textdatasets/Chapter%20%201%20Data%20Sets.html

## Website for R datasets:
# https://vincentarelbundock.github.io/Rdatasets/datasets.html 

# libraries:
library(modelr)
library(fitdistrplus)

library(tidyverse)
library(ggplot2)
library(dplyr)
library(tidyr)

library(sas7bdat)
library(data.table)
library(segmented)
library(car) # for type III sum of squares: Anova(lm or quadratic etc., type = "III")
library(MASS) # for studres(lm) - studentized residuals (aka jackknifed residuals)
library(lsmeans)
library(emmeans)
# install.packages("papeR") - https://cran.r-project.org/web/packages/papeR/vignettes/papeR_introduction.html
library(papeR)


## Assumption for t test for correlation:
# 1. You need to make sure both continuous variables are normal - bivariate normally distributed


## F test for linear regression:
# One of the variables needs to be regarded as the dependent, the other as the independent, variable.
# X (independent variable) can be a categorical variable.



## CH01 PR19
?read.csv
gpa = read.csv("/Users/jamescutler/Desktop/Biostats_II/CH01PR19.csv", header = FALSE)
gpa = gpa[,2:3]
plot(gpa$V3,gpa$V2)
plot(gpa$V2 ~ as.factor(gpa$V3))
mod19 = lm(gpa$V2 ~ gpa$V3); mod19


## CH01 PR20
copiers = read.csv("/Users/jamescutler/Desktop/Biostats_II/CH01PR20.csv", header = FALSE)
copiers = copiers[,2:3]
plot(copiers$V3,copiers$V2)
mod20 = lm(V2 ~ V3, data = copiers); mod20 # THE REASON IT DOESN'T WORK BELOW 
# IS BECAUSE I DIDN'T HAVE IT ENTERED IN THE RIGHT WAY HERE!!!!!! I HAVE NOW CORRECTED IT!
abline(mod20, col = "red")
# predict(mod20, newdata = data.frame(x=5)) # DOESN'T WORK (ACTUALLY, SEE ABOVE)

mod20$coefficients[[1]] + 5*mod20$coefficients[[2]]
newdf = data.frame(V3 = c(4,5,6))
predict(mod20,newdf) # NOW IT WORKS BECAUSE I USED DATA = COPIERS IN THE LM FUNCTION ABOVE!!!!!


## CH01 PR27

musc = read.csv("/Users/jamescutler/Desktop/Biostats_II/CH01PR27.csv", 
                header = FALSE)
musc = musc[2:3]
plot(musc$V3,musc$V2)
mod27 = lm(V2 ~ V3, data = musc); mod27
abline(mod27, col = "red")

ggplot(musc, aes(V3,V2)) +
  geom_point(col = "red") +
  geom_smooth(method = lm, se=TRUE)

summary(mod27)

# For the markdown pretty table:
install.packages("sjPlot") # SEE KutnerCH01PR27.Rmd for the rest of the code



## CH01 PR28
crime = read.csv("/Users/jamescutler/Desktop/Biostats_II/CH01PR28.csv", 
                 header = FALSE)
crime = crime[,2:3]
crime = crime[,2:1]
colnames(crime) = c("HS_diploma","crime_per100k")
plot(crime$HS_diploma,crime$crime_per100k)
mod28 = lm(crime_per100k ~ HS_diploma, data = crime); summary(mod28)
abline(mod28, col="red")


##############################################################################
##############################################################################

# James Cutler
# WK 1 homework Zhang 

# ALSM 1.11, 1.12, 1.29, 1.30

### 1.11
## The regression function relating production output by an employee after taking
# a training program (Y) to the production output before the training program (X)
# is E{Y} = 20 + .95X, where X ranges from 40 to 100. An observer concludes that
# the training program does not raise production output on average because beta1
# is not greather than 1.0. Comment.
X = 40:100
Y = 20 + .95*X
plot(X, ylim = c(0,140))
points(Y, col = "red", pch = 18)

## Based on the above plot, I would say that the training did work, because the 
# production is in fact higher. It's not really surprising that the improvement 
# would be increasingly smaller as one's prior production increases. Nonetheless, 
# the training appears to have been a success.



### 1.12
## In a study of the relationship for senior citizens between physical activity 
# and frequency of colds, participants were asked to monitor their weekly time 
# spent in exercise over a 5-year period and the frequency of colds. The study
# demonstrated a negative statistical relation exists between time spent in
# exercise and frequency of colds. The investigator conlcuded that increasing
# time spent in exercise is an effective strategy for reducing the frequency of
# colds for senior citizens. 

## a) This was observational data.

## b) The conclusion inferred a causal relationship between exercise and colds 
# (that exercise prevents colds); since this was an observational study, their
# are high standards to meet before expressing confidence about causality. 
# Experimental, rather than observational, data would better establish causality.

## c) 
# 1) Could it be that a hormone or some other genetically-determined factor
# both predisposes one to be more active and to have a healthier immune system
# (and thus less prone to getting colds).
# 2) Could it be that a larger portion of the participants who both exercized 
# more and got colds less were experiencing those two phenomena because they
# lived in warmer climates? The study design says nothing about where the 
# participants came from. If participants were recruited from many different
# places in the country, then perhaps warmer climate could explain higher
# exercise rates and lower infection rates (people are not cloistered indoors
# as often in warmer climates). 

## d) Randomly assign half of participants to a high-exercise group, and the 
# other half to a low-exercise group, and see if there is any association 
# between exercise and colds. Or create low-, medium-, and high-exercise groups.



### 1.29
## Model (1.1): Yi = beta0 + beta1*Xi + Ei
# This model is simple, linear in the parameters, and linear in the predictor
# variable:
# simple: only on predictor variable.
# linear parameters: no parameter appears as an exponent or is multipled/divided
# by another parameter.
# linear predictor variable: this variable appears only in the first power.

## Refer to regression model (1.1). Assume that X = 0 is within the scope of the
# model. What is the implication for the regression function if beta0 = 0 so
# that the model is Yi = beta1*Xi + Ei? How would the regression function plot
# on a graph? 

## I think the plot would have a straight line originating at 0,0, because that
# is what the Y intercept would be. 



### 1.30
## Refer to regression model (1.1). What is the implication for the regression 
# function if beta1 = 0 so that the model is Yi = beta0 + Ei?

## I think the function would be a horizontal line since the slope is zero, and
# that would mean that the predictor variable doesn't affect the dependent 
# variable in any visible way. 





##############################################################################
##############################################################################

# James Cutler
# WK 2 homework Zhang 

# ALSM 1.43, 2.2, 2.17, 2.62

### 1.43
library(ggplot2)
CDI = read.csv("/Users/jamescutler/Desktop/Biostats_II/APPENC02.csv", 
               header = FALSE)
CDI = CDI[3:ncol(CDI)]
colnames(CDI) = c("County","State","S_Area","Pop","Per18_34","Per65up","Physicians",
                  "Hosp_beds","Crimes","PerHSgrads","PerBach","PerPoor","PerUnemp",
                  "PerCapInc","PersonalInc","GeoReg")

# Plots in ggplot2:
ggplot(CDI, aes(Pop,Physicians)) +
  geom_point(alpha = .1, col = "red") +
  geom_smooth(method = "lm", se = FALSE, size = .3) + 
  theme_bw()

ggplot(CDI, aes(Hosp_beds,Physicians)) +
  geom_point(alpha = .1, col = "blue") +
  geom_smooth(aes(color = "red"),method = "lm", se = FALSE, size = .3) + 
  theme_bw()
ggplot(CDI, aes(Hosp_beds,Physicians)) +
  geom_point(alpha = .1, col = "blue") +
  geom_smooth(aes(color = "red"), se = FALSE, size = .3) +
  theme_bw()

ggplot(CDI, aes(PersonalInc,Physicians)) +
  geom_point(alpha = .1, col = "purple") +
  geom_smooth(aes(color = "red"),method = "lm", se = FALSE, size = .3) + 
  theme_bw()

mod1.43 = lm(Physicians ~ Pop+Hosp_beds+PersonalInc, data = CDI); summary(mod1.43)


## a) Number of physicians regressed on the three predictor 
### variables; estimated regression functions:
mod1.43_pop = lm(Physicians ~ Pop, data = CDI); summary(mod1.43_pop)
mod1.43_beds = lm(Physicians ~ Hosp_beds, data = CDI); summary(mod1.43_beds)
mod1.43_inc = lm(Physicians ~ PersonalInc, data = CDI); summary(mod1.43_inc)


## b) Plots of the estimated regression functions with their data:
# Regressed on population:
plot(CDI$Pop,CDI$Physicians, pch = 16, cex = .2, col = "red",
     main = "Physicians regressed on population",
     xlab = "Population", ylab = "Number of Physicians")
abline(mod1.43_pop, col = "green")

# Regressed on hospital beds:
plot(CDI$Hosp_beds,CDI$Physicians, pch = 16, cex = .2, col = "red",
     main = "Physicians regressed on hospital beds",
     xlab = "Hospital beds", ylab = "Number of Physicians")
abline(mod1.43_beds, col = "green")
# A linear model doesn't appear to be the best fit for hospital beds. It looks like
# it might be non-linear.

# Regressed on personal income:
plot(CDI$PersonalInc,CDI$Physicians, pch = 16, cex = .2, col = "red",
     main = "Physicians regressed on personal income",
     xlab = "Personal Income", ylab = "Number of Physicians")
abline(mod1.43_inc, col = "green")


## c) MSE for each of the predictor variables:
msePop = mean(mod1.43_pop$residuals^2); msePop
mseBeds = mean(mod1.43_beds$residuals^2); mseBeds
mseInc = mean(mod1.43_inc$residuals^2); mseInc



### 2.2
# In a test of the alternatives Ho: beta1 ≤ 0 versus Ha: beta1 > 0, an analyst 
## concluded Ho. Does this mean there is no linear association between X and Y?

# I would say that it doesn't mean the association is not linear or that there is
## no linear association. It could be that the association is linear and negative
## (i.e. that X and Y are negatively correlated in a linear way).



### 2.17
# The alpha level used was greater than .033. If the alpha had been .01, then
## the appropriate conclusion would have been Ho.



### 2.62
# Reviewing again the models from 1.43:
mod1.43_pop = lm(Physicians ~ Pop, data = CDI); summary(mod1.43_pop)
mod1.43_beds = lm(Physicians ~ Hosp_beds, data = CDI); summary(mod1.43_beds)
mod1.43_inc = lm(Physicians ~ PersonalInc, data = CDI); summary(mod1.43_inc)

# population R^2 :      .8838

# hospital beds R^2 :   .9032 
## This accounts for the largest reduction in the variability in the number 
## of active physicians

# personal income R^2 : .8987


#### Update - feedback from HW 2 key:
plot(c(372204,310192,324539)#,        # THESE ARE THE MSE's SHE GOT
     #ylim = c(100000,400000)
)
points(1:3,c(370511,308781,323064), col = "red") # THESE ARE THE ONES I GOT






##############################################################################
##############################################################################

# HW 3 James Cutler - Dr. Zhang
CDI = read.csv("/Users/jamescutler/Desktop/Biostats_II/APPENC02.csv", 
               header = FALSE)
CDI = CDI[,3:ncol(CDI)]
colnames(CDI) = c("County","State","S_Area","Pop","Per18_34","Per65up","Physicians",
                  "Hosp_beds","Crimes","PerHSgrads","PerBach","PerPoor","PerUnemp",
                  "PerCapInc","PersonalInc","GeoReg")

# The three linear models:
mod1.43_pop = lm(Physicians ~ Pop, data = CDI); summary(mod1.43_pop)
mod1.43_beds = lm(Physicians ~ Hosp_beds, data = CDI); summary(mod1.43_beds)
mod1.43_inc = lm(Physicians ~ PersonalInc, data = CDI); summary(mod1.43_inc)

# 1. (non-)linearity of the regression function:
## Residual plots:
plot(mod1.43_pop$residuals)
abline(h = 0, col = "red", lty = 2)
plot(mod1.43_beds$residuals)
abline(h = 0, col = "blue", lty = 2)
plot(mod1.43_inc$residuals)
abline(h = 0, col = "purple", lty = 2)

## Residuals vs independent variables:
plot(CDI$Pop,resid(mod1.43_pop))
abline(h = 0, col = "red", lty = 2)
plot(CDI$Hosp_beds,resid(mod1.43_beds))
abline(h = 0, col = "blue", lty = 2)
plot(CDI$PersonalInc,resid(mod1.43_inc))
abline(h = 0, col = "purple", lty = 2)

## Residuals vs fitted (predicted responses):
plot(fitted(mod1.43_pop),resid(mod1.43_pop))
abline(h = 0, col = "red", lty = 2)
plot(fitted(mod1.43_beds),resid(mod1.43_beds))
abline(h = 0, col = "blue", lty = 2)
plot(fitted(mod1.43_inc),resid(mod1.43_inc))
abline(h = 0, col = "purple", lty = 2)

# 2. non-constant error variance:
## If you look again at the residual plots from above, you can see they don't have 
## constant variance:
plot(mod1.43_pop$residuals)
abline(h = 0, col = "red", lty = 2)
plot(mod1.43_beds$residuals)
abline(h = 0, col = "blue", lty = 2)
plot(mod1.43_inc$residuals)
abline(h = 0, col = "purple", lty = 2)

# 3. The error terms appear to not have violated any independence criteria:
## We have no information on the order in which the observations were made.
## Multiple outcome observations were not made on the same subject
## Observations don't appear to have been made in clusters (clusters of counties)

# 4. Error terms don't appear to have been normally distributed:
qqnorm(resid(mod1.43_pop))
qqline(resid(mod1.43_pop), col = "red")
qqnorm(resid(mod1.43_beds))
qqline(resid(mod1.43_beds), col = "blue")
qqnorm(resid(mod1.43_inc))
qqline(resid(mod1.43_inc), col = "purple")

# 5. The models appear to fit most of the data, though they do not fit a few 
## outliers.
# Regressed on population:
plot(CDI$Pop,CDI$Physicians, pch = 16, cex = .2, col = "red",
     main = "Physicians regressed on population",
     xlab = "Population", ylab = "Number of Physicians")
abline(mod1.43_pop, col = "green")
# Regressed on hospital beds:
plot(CDI$Hosp_beds,CDI$Physicians, pch = 16, cex = .2, col = "red",
     main = "Physicians regressed on hospital beds",
     xlab = "Hospital beds", ylab = "Number of Physicians")
abline(mod1.43_beds, col = "green")
# Regressed on personal income:
plot(CDI$PersonalInc,CDI$Physicians, pch = 16, cex = .2, col = "red",
     main = "Physicians regressed on personal income",
     xlab = "Personal Income", ylab = "Number of Physicians")
abline(mod1.43_inc, col = "green")


# 6. I tried creating MR models to see what the regression coefficients were like
## for the proposed omitted variable (percent of population 65 and up):
MR65_pop = lm(Physicians ~ Pop+Per65up, data = CDI); summary(MR65_pop)
MR65_beds = lm(Physicians ~ Hosp_beds+Per65up, data = CDI); summary(MR65_beds)
MR65_inc = lm(Physicians ~ PersonalInc+Per65up, data = CDI); summary(MR65_inc)
## I'm not sure what the coefficients were supposed to indicate. None of them were
## zero. 

## Also, the covariance of the omitted and previously known independent variables
## is not zero in any of the three cases:
cov(CDI$Per65up,CDI$Pop)
plot(CDI$Per65up,CDI$Pop) # plot just for fun
cov(CDI$Per65up,CDI$Hosp_beds)
plot(CDI$Per65up,CDI$Hosp_beds) # plot just for fun
cov(CDI$Per65up,CDI$PersonalInc)
plot(CDI$Per65up,CDI$PersonalInc) # plot just for fun

# Plotting the residuals of each of the three models against 65 and up:
plot(CDI$Per65up,resid(mod1.43_pop))
abline(h = 0, col = "grey", lty = 2)
plot(CDI$Per65up,resid(mod1.43_beds))
abline(h = 0, col = "grey", lty = 2)
plot(CDI$Per65up,resid(mod1.43_inc))
abline(h = 0, col = "grey", lty = 2)





##############################################################################
##############################################################################

# Multiple Regression

## Grocery retailer CH06PR09:
G = read.csv("/Users/jamescutler/Desktop/Biostats_II/CH06PR09.csv",
             header = FALSE)
head(G,5)
colnames(G) = c("labor_hrs","cases","ICTLH_per","holiday")
plot(G$labor_hrs)
barplot(G$labor_hrs, 
        names.arg = as.character(G$holiday), cex.names = .8)

MRmod = lm(labor_hrs ~ cases+ICTLH_per+holiday, data = G); summary(MRmod)

casemod = lm(labor_hrs ~ cases, data = G)
plot(G$cases,G$labor_hrs,
     xlab = "cases",ylab = "labor hrs")
abline(casemod, col = "red")
cor(G$cases,G$labor_hrs)

indcostmod = lm(labor_hrs ~ ICTLH_per, data = G)
plot(G$ICTLH_per,G$labor_hrs,
     xlab = "indirect costs of the total labor hrs as a percentage",
     ylab = "labor hrs")
abline(indcostmod, col = "red")
cor(G$ICTLH_per,G$labor_hrs)

ggplot(G, aes(1:52,labor_hrs, col = holiday)) +
  geom_point(aes(shape = as.factor(holiday))) +
  coord_cartesian(ylim = c(0,5100))

ggplot(G, aes(1:52,cases, col = holiday)) +
  geom_point(aes(shape = as.factor(holiday)))

ggplot(G, aes(1:52,ICTLH_per, col = as.factor(holiday))) +  
  geom_point(aes(shape = as.factor(holiday)))


## RMR data in class:
RMR = read.sas7bdat("/Users/jamescutler/Desktop/Biostats_II/RMR.sas7bdat",
                    debug = TRUE)
head(RMR,5)
ggplot(RMR, aes(1:nrow(RMR),rmr, col = as.factor(athlete))) +
  geom_point(aes(shape = as.factor(athlete))) 

ggplot(RMR, aes(weight,rmr, col = as.factor(athlete))) +
  geom_point(aes(shape = as.factor(athlete))) 

ggplot(RMR, aes(age,rmr, col = as.factor(athlete))) +
  geom_point(aes(shape = as.factor(athlete))) 

ggplot(RMR, aes(height,rmr, col = as.factor(athlete))) +
  geom_point(aes(shape = as.factor(athlete))) +
  theme_bw()

RMR$athlete = as.factor(RMR$athlete)
basicmod = lm(rmr ~ weight+athlete+age+height, data = RMR); summary(basicmod)
confint(basicmod, level = .95)



#################



##########

### HW 4 - transformations; and pain relief
library(sas7bdat)
# Transformations on Appendix C.2 data (counties & physicians)

C = read.csv("/Users/jamescutler/Desktop/Biostats_II/APPENC02.csv",
             header = FALSE)
C = C[,3:ncol(C)]
colnames(C) = c("County","State","S_Area","Pop","Per18_34","Per65up","Physicians",
                  "Hosp_beds","Crimes","PerHSgrads","PerBach","PerPoor","PerUnemp",
                  "PerCapInc","PersonalInc","GeoReg")

# Untransformed linear model (for compare and contrast below):
modbeds = lm(Physicians ~ Hosp_beds, data = C)

# Log10 model:
modlog10 = lm(log10(Physicians) ~ log10(Hosp_beds), data = C); summary(modlog10)
# Log10 plot:
plot(log10(C$Hosp_beds),log10(C$Physicians))
abline(modlog10, col = "red")
# Log10 residuals vs order plot (not applicable to this dataset because we DON'T know the order in which the observations were taken):
plot(resid(modlog10))
abline(h=0, col = "red", lty = 2)
# Log10 residuals against the fitted values, contrasted with the untransformed plot:
par(mfrow=c(1,2))
par(mar=c(2,1,4,1))
plot(fitted(modlog10),resid(modlog10), main = "Transformed")
abline(h = 0, col = "red", lty = 2)
plot(fitted(modbeds),resid(modbeds), main = "Untransformed")
abline(h = 0, col = "red", lty = 2)
par(mfrow=c(1,1))
# Log10 qqplot of residuals:
qqnorm(resid(modlog10))
qqline(resid(modlog10), col = "red")
## The residual plot looks better than it did before transformation. Variance in the error terms is still not very constant, but it looks better than the untransformed plot.
## The qqplot shows that now our errors appear to satisfy the normality assumption.
## This is the best regression model of the three, as will be apparent below (sqrt and inverse don't look as good diagnostically).
## The coefficient for this model shows that for every log10(# of hospital beds) equal to 1, there are log10(# of physicians) equal to 1.0258; this comes out to about 10.6 physicians for every 10 beds.
## There appear to be 1 or 2 outliers, but the most extreme residual value is 1.3665, and I'm just not sure if that's considered very extreme or not.

# sqrt model:
modsqrt = lm(sqrt(Physicians) ~ sqrt(Hosp_beds), data = C); summary(modsqrt)
# sqrt plot:
plot(sqrt(C$Hosp_beds),sqrt(C$Physicians))
abline(modsqrt, col = "red")
# sqrt model residual plot:
plot(resid(modsqrt))
abline(h=0, col = "red", lty = 2)
# sqrt residuals against the fitted values:
plot(fitted(modsqrt),resid(modsqrt))
abline(h = 0, col = "red", lty = 2)
# sqrt qqplot of residuals:
qqnorm(resid(modsqrt))
qqline(resid(modsqrt), col = "red")
## The constant variance assumption is not as well-satisfied here as with the log10 transformation.
## The normality assumption is not as well met here either.

# Inverse (1/Y and 1/X) model:
invPhys = 1/C$Physicians
invBeds = 1/C$Hosp_beds
modinv = lm(invPhys ~ invBeds); summary(modinv)
# Inverse (1/Y and 1/X) plot:
plot(1/C$Hosp_beds,1/C$Physicians)
abline(modinv, col = "red")
# Inverse (1/Y and 1/X) residuals:
plot(resid(modinv))
abline(h=0, col = "red", lty = 2)
# Inverse residuals against the fitted values:
plot(fitted(modinv),resid(modinv))
abline(h = 0, col = "red", lty = 2)
# Inverse (1/Y and 1/X) qqplot of residuals:
qqnorm(resid(modinv))
qqline(resid(modinv), col = "red")
## Here the model seems a poor fit, and constant variance assumption is not met, and neither is the normality assumption.


# Pain relief:
PR = read.sas7bdat("/Users/jamescutler/Desktop/Biostats_II/PainRelief.sas7bdat",
                   debug = TRUE)
class(PR$DOSE)
head(PR,10)
PR$DOSE = as.factor(PR$DOSE) # Will need this for the boxplots; will be switched back to numeric afterwards
PR$MALE = as.factor(PR$MALE)
PR$DOSE3 = as.factor(PR$DOSE3)
PR$DOSE6 = as.factor(PR$DOSE6)
PR$DOSE9 = as.factor(PR$DOSE9)
PR$DOSELEV = as.factor(PR$DOSELEV)
summary(PR)

# 1.
# What percentage are male? Answer: 50%

# What is the average time to relief for each dose level? How variable are the observed relief times?
plot(PR$RELIEF ~ PR$DOSE, 
     xlab = "Dose", ylab = "Time to relief")
means = tapply(PR$RELIEF, PR$DOSE, mean)
points(means,col = "red", pch = 18)
## This is the average time to relief for each dose level:
means
## The relief times appear to vary significantly at first, then begin to level off a bit at higher doses, while still decreasing.

## Restore DOSE to a numeric class to enable proper analysis below:
PR$DOSE = as.numeric(PR$DOSE)

# Boxplots of time to relief by gender:
plot(PR$RELIEF ~ PR$MALE,
     xlab = "Gender (0 = F; 1 = M)", ylab = "Time to relief")
m_gender = tapply(PR$RELIEF, PR$MALE, mean)
points(m_gender, col = "red", pch = 18)
## The relationship I see suggests that the male time to relief may be slightly longer.

# 2.
# Multiple regression model with gender and dose as predictors of relief time:
mod1 = lm(RELIEF ~ MALE+DOSE, data = PR); summary(mod1)
## The coefficients tell me that relief time is inversely correlated with dose, but slightly positively correlated with male gender.
## The association between relief time and dose is significant, and technically not significant at alpha = .05 for gender.

# 3. 
# MR model for doses 3,6,9 and gender:
mod2 = lm(RELIEF ~ MALE+DOSE3+DOSE6+DOSE9, data = PR); summary(mod2)
## The association between relief time and dose is significant at all three levels tested here, but barely not for gender.

# 4.
# MR model for DOSELEV and gender:
mod3 = lm(RELIEF ~ MALE+DOSELEV, data = PR); summary(mod3)
## Same results as above in mod2.

# 5.
## Comparing the models above, 3 and 4 don't look different. We appear to be making the unique assumption in 2, compared to 3 and 4, not in 3, compared to 2 and 4. In 2, we're treating DOSE as a continuous predictor variable, whereas in 3 and 4 we are treating the dose levels as factors.

# 6.
## The coefficients for MALE in each of the models 2, 3, and 4 are all the same: 5.667.

# 7.
## As far as further analysis is concerned, I would think we could do an ANOVA. This would show whether there is at least one dose that is significantly different than the others. For specific comparisons, we can do Bonferroni or Tukey comparisons.




##############################################################################
##############################################################################

# Project 1

smoke = read.sas7bdat("/Users/jamescutler/Desktop/Biostats_II/SmokingAndFEV.sas7bdat",
                      debug = TRUE)
plot(smoke$fev ~ as.factor(smoke$smoke))
smokers = smoke[which(smoke$smoke == 1),"fev"]
nonsmokers = smoke[-which(smoke$smoke == 1),"fev"]
nonsmokers
t.test(smokers,nonsmokers,conf.level = .95)

plot(smoke$age ~ as.factor(smoke$smoke))
plot(smoke$age,smoke$fev)

S = smoke[which(smoke$smoke == 1),]
N = smoke[-which(smoke$smoke == 1),]
plot(N$age,N$fev)
points(S$age,S$fev, col = "red")

tenup = smoke[which(smoke$age >= 10),]
plot(tenup$fev ~ as.factor(tenup$smoke))
tsmoke = tenup[which(tenup$smoke == 1),"fev"]
tnon = tenup[which(tenup$smoke == 0),"fev"]
t.test(tsmoke,tnon)

plot(smoke$height,smoke$fev)
heightmod = lm(fev ~ height, data = smoke)
abline(heightmod, col = "red")

plot(N$height,N$fev)
points(S$height,S$fev, col = "red")

sixtyup = smoke[which(smoke$height >= 60),]
plot(sixtyup$fev ~ as.factor(sixtyup$smoke))
sismoke = sixtyup[which(sixtyup$smoke == 1),"fev"]
sinon = sixtyup[which(sixtyup$smoke == 0),"fev"]
t.test(sismoke,sinon)

plot(smoke$fev ~ as.factor(smoke$sex))
mymeans = tapply(smoke$fev, smoke$sex, mean); mymeans
points(mymeans, col = "red", pch = 18)
boys = smoke[which(smoke$sex == 1),"fev"]
girls = smoke[which(smoke$sex == 0),"fev"]
t.test(boys,girls)
boys
girls

length(which(S$sex == 1))
length(which(S$sex == 0))
26+39


################################

# James Cutler
# Project 1 

library(sas7bdat)
library(ggplot2)
library(dplyr)

smoke = read.sas7bdat("/Users/jamescutler/Desktop/Biostats_II/SmokingAndFEV.sas7bdat",
                      debug = TRUE)

smoke$sex = factor(smoke$sex,
                   levels = c(0,1),
                   labels = c("female","male"))
smoke$smoke = factor(smoke$smoke,
                     levels = c(0,1),
                     labels = c("nonsmoker","smoker"))

# Problem Description
## It is known that smoking impairs lung function in adults. There is a statistically significant association between FEV and smoking in adults, with adult smokers having a lower mean FEV. It would be of interest to know whether this association exists in children, since there is less data on children in regards to this question.

# Objective
## We want to find if there is an association between FEV and smoking status, in order to see if there is evidence that smoking impairs lung function in children, just like in adults.

# Available Data
## We have data on the FEV, sex, height, age, and smoking status of 654 children, aged 3 to 19, including 65 smokers. There are 318 girls in the sample () 

# Analysis Methods
## We are using a simple linear regression method to analyze the association between smoking status and FEV in children. 
## We will also run multiple regression on FEV and the following: smoking status, age, height, and sex. 
## We will run diagnostics with residual plots and QQ plots. 

# Results (with interpretation)

## Descriptive statistics:
summary(smoke)
smoke %>%
  count(sex,smoke) %>%
  group_by(sex) %>%
  mutate(prop = prop.table(n))
smoke %>%
  count(smoke,sex) %>%
  group_by(smoke) %>%
  mutate(prop = prop.table(n))
### There are more female smokers than male (39 to 26). There are more male nonsmokers than female (310 to 279).

## Descriptive plots:
plot(smoke$fev ~ smoke$smoke, main = "FEV by smoking status")
smkMeans = tapply(smoke$fev, smoke$sex, mean)
points(smkMeans, col = "red", pch = 18)
plot(smoke$fev ~ smoke$sex, main = "FEV by sex")
sxMeans = tapply(smoke$fev, smoke$sex, mean)
points(sxMeans, col = "red", pch = 18)
plot(smoke$age,smoke$fev, main = "FEV by age")
plot(smoke$height,smoke$fev, main = "FEV by height")
### There **appears** to be an association between smoking status and FEV, based on the relevant boxplot, above. There is also an apparent association between age/height and FEV, however. Sex and FEV might have a slight association as well.

## Hypothesis tests:
## Multiple regression (full and reduced models):
MRmod_full = lm(fev ~ smoke*sex*age*height, data = smoke); summary(MRmod_full)
MRmod_reduced = lm(fev ~ smoke+sex+age+height, data = smoke); summary(MRmod_reduced)
### The full model indicates that there is are interactions that are highly significant:
#### - sex and age
#### - sex, age and height
### The reduced model indicates that sex, age, and height all have a highly significant association with FEV, but that smoking status does not.
### Given the interaction between sex and age, it would be interesting to see that visually:
### Interaction between age and sex on FEV:
ggplot(smoke, aes(age,fev, col = sex)) +
  geom_point(alpha = .3)
plot(smoke$age ~ smoke$sex)
### Indeed, it looks like the older males have a higher FEV than the older females.

## Simple linear regression on the four variables:
modsmoke = lm(fev ~ smoke, data = smoke); summary(modsmoke)
confint(modsmoke, level = .95)
modage = lm(fev ~ age, data = smoke); summary(modage)
confint(modage, level = .95)
modheight = lm(fev ~ height, data = smoke); summary(modheight)
confint(modheight, level = .95)
modsex = lm(fev ~ sex, data = smoke); summary(modsex)
confint(modsex, level = .95)
### Solely based on the coefficients in these SLMs, smoking raises your FEV by .70 L/S (.49 to .93); FEV rises .22 L/S (.20 to .24) for every additional year of age; FEV rises .132 L/S with every additional inch (.126 to .138); male gender sees a .36 L/S rise in FEV (.23 to .49).

## Diagnostics on SLMs:
### FEV against smoking status - residual and qq plots:
plot(fitted(modsmoke),resid(modsmoke), main = "smoking status residuals")
abline(h = 0, col = "red", lty = 2)
qqnorm(resid(modsmoke), main = "smoking status QQ plot")
qqline(resid(modsmoke), col = "red")

### FEV against age - residual and qq plots:
plot(fitted(modage),resid(modage), main = "age residuals")
abline(h = 0, col = "red", lty = 2)
qqnorm(resid(modage), main = "age QQ plot")
qqline(resid(modage), col = "red")

### FEV against height - residual and qq plots:
plot(fitted(modheight),resid(modheight), main = "height residuals")
abline(h = 0, col = "red", lty = 2)
qqnorm(resid(modheight), main = "height QQ plot")
qqline(resid(modheight), col = "red")

### FEV against sex - residual and qq plots:
plot(fitted(modsex),resid(modsex), main = "sex residuals")
abline(h = 0, col = "red", lty = 2)
qqnorm(resid(modsex), main = "sex QQ plot")
qqline(resid(modsex), col = "red")

## Also, is FEV itself normally distributed?
qqnorm(smoke$fev, main = "Is FEV normally distributed? - only sort of")
qqline(smoke$fev, col = "red")


# Conclusions
## We can conclude that there appears to be an association between smoking status and FEV, when you ignore the possible confounding of the other variables. But when you consider those other variables, the association goes away (it is not significant), as seen in our reduced MR model.

# Limitations
## One limitation is that we don't know how long the smokers had the habit of smoking, and how much they smoked (one pack a day, two?).




##############################################################################
##############################################################################

# Week 7 in-class exercise 

data("mtcars")
summary(mtcars)
mtcars$vs = as.factor(mtcars$vs)
mtcars$am = as.factor(mtcars$am)

ggplot(mtcars, aes(mpg,hp, col = disp)) +
  geom_point()

plot(mtcars$hp,mtcars$disp)
plot(mtcars$mpg,mtcars$hp)
plot(mtcars$disp,mtcars$mpg)

quantile(disp, probs = c(.33,.66))
summary(disp)[3]

lhalf_disp = mtcars[which(mtcars$disp < summary(disp)[3]),]
uhalf_disp = mtcars[which(mtcars$disp > summary(disp)[3]),]
ltert_disp = mtcars[which(mtcars$disp < 145.391),]
mtert_disp = mtcars[which(between(mtcars$disp,145.391,287.392)),]
utert_disp = mtcars[which(mtcars$disp > 287.392),]
mt2 = mtcars
mt2[which(mtcars$disp < 145.391),"disp"] = 1 
mt2[which(between(mtcars$disp,145.391,287.392)),"disp"] = 2
mt2[which(mtcars$disp > 287.392),"disp"] = 3
disp.hp = lm(mpg ~ hp+disp, data = mt2); summary(disp.hp)


moduter = lm(mpg ~ hp, data = utert_disp)
modmter = lm(mpg ~ hp, data = mtert_disp)
modlter = lm(mpg ~ hp, data = ltert_disp)
plot(ltert_disp$hp,ltert_disp$mpg, ylim = c(10,35), xlim = c(50,340))
points(mtert_disp$hp,mtert_disp$mpg, col = "red")
points(utert_disp$hp,utert_disp$mpg, col = "blue")
abline(modlter)
abline(modmter, col = "red")
abline(moduter, col = "blue")


# Part II
plot(mtcars$mpg ~ mtcars$am)
ammeans = tapply(mtcars$mpg,mtcars$am, mean); ammeans
aovmod = aov(mpg ~ hp*am, data = mtcars); summary(aovmod)
lmmod = lm(mpg ~ hp*am - 1, data = mtcars); summary(lmmod)
ggplot(mtcars, aes(hp,mpg, col = am)) +
  geom_point()




##############################################################################
##############################################################################

# HW Week 9

# 16.3, 9, 42, 43; 17.6, 35

# 16.3
## Assumptions:
### 1) Each probability distribution is normal. Nothing in the description of the study appears to obviously violate this assumption.
### 2) Each proability distribution has the same variance. Same as above.
### 3) The responses for each factor level are random selections from their probability distributions and are independent of any other responses from other factor levels. This assumption is violated by the fact that respondents were able to hear (and thus potentially be influenced by) the responses of others.


# 16.9
## a)
below = c(29,42,38,40,43,40,30,42)
ave = c(30,35,39,28,31,31,29,35,29,33)
above = c(26,32,21,20,23,22)
rehab = data.frame(dias = c(below,ave,above),
                   fitness = rep(c("below","average","above"),
                                 c(length(below),length(ave),length(above))),
                   nums = rep(c(1,2,3),
                              c(length(below),length(ave),length(above))) )
plot(rehab$dias ~ as.factor(rehab$fitness))
plot(rehab$nums,rehab$dias,
     main = "Dot plot (i.e. box plot without the boxes) \nof each fitness group",
     xlab = "1=below average, 2=average, 3=above average")
means = tapply(rehab$dias,rehab$nums,mean)
points(means, pch = 18, col = "red")

### Yes, the means of each factor level appear to differ. The variability of the of the observations within each factor level appear to be approximately the same for all factor levels.

## b) The fitted values are these:
means

## c) The residuals, and their sum:
rehabmod = aov(dias ~ factor(fitness), data = rehab)
# rehabP = proj(rehabmod)
sum(resid(rehabmod))
plot(fitted(rehabmod),resid(rehabmod),
     main = "Residuals for each fitness group",
     xlab = "24 is the mean for group 3 (below average)\n38 is the mean for group 1 (above average)")
abline(h = 0, col = "red", lty = 2)

## d) 
summary(rehabmod)

## e) 
# qf(.01,df1 = 2, df2 = 21, lower.tail = FALSE) # F = 5.78
# pf(5.780416,2,21, lower.tail = FALSE)
### Alternative hypothesis: At least one of the groups is different.
### Decision rule: If F* <= 5.78, conclude H0. If F* > 5.78, conclude Ha.
### Conclusion: There is sufficient evidence to conclude that at least one of the factor levels differs from the others. In other words, the factor level means are not all equal, which means the three different levels of fitness do not lead to the same duration in rehab. Thus, we can conclude that there is a relation between fitness and rehab duration.

## f) p-value:
pf(16.96,2,21, lower.tail = FALSE)

### The same conclusion can be reached by stating that if the p-value >= .01, we will conclude the H0, but if < .01, we will conclude the Ha.

## g) It appears that the greater the level of fitness prior to surgery, the less time spent in rehab.



# 16.42





##############################################################################
##############################################################################

# Project 2 - ANOVA

# poison - tx dataset from Dean & Voss:

headers = read.csv("/Users/jamescutler/Desktop/Data_Course_cutler/survival_data_Dean_Voss.csv",
                   skip = 0, header = FALSE, nrows = 1, as.is = TRUE)

pois = read.csv("/Users/jamescutler/Desktop/Data_Course_cutler/survival_data_Dean_Voss.csv",
                header = FALSE)
pois = pois[-1,-c(1,5)]
colnames(pois) = headers

pois$group = rep(1:12,each=4)

ggplot(pois, aes(x=POISON,y=TIME,col=TRTMT)) +
  geom_point(alpha=.4) +
  stat_summary(fun.y = "mean", geom = "line", aes(group = factor(TRTMT)))

ggplot(pois, aes(x=POISON,y=TIME,col=TRTMT)) +
  geom_jitter(width = .1) +
  stat_summary(fun.y = "mean", geom = "line", aes(group = factor(TRTMT))) +
  labs(title = "Survival times by treatment and poison",
       x="Poison",y="Survival time in units of 10 hours")

modfull = aov(TIME ~ factor(POISON)*factor(TRTMT), data=pois)
anova(modfull)

plot(modfull$residuals)
plot(fitted(modfull),resid(modfull))
qqnorm(pois$TIME)
qqline(pois$TIME, col = "red")

modtr = aov(1/TIME ~ factor(POISON)+factor(TRTMT), data = pois)
anova(modtr)
plot(modtr$residuals)
plot(fitted(modtr),resid(modtr))
qqnorm(1/pois$TIME)
qqline(1/pois$TIME, col = "red")

tranov = Anova(modtr, type=2)
tranov
anova(modtr)


means = tapply(pois$TIME,factor(pois$group),mean)
means
means.mat = t(matrix(means, nrow = 3, ncol = 4, byrow = TRUE))
means.mat
plot(means.mat[1,], ylim = c(0.2,1), col = "red", type = "b")
points(means.mat[2,], col = "green", type = "b")
points(means.mat[3,], col = "blue", type = "b")
points(means.mat[4,], col = "purple", type = "b")


ls.trtTR = lsmeans(modtr, specs = ~TRTMT)
ls.trtTR
trt.means = tapply(pois$TIME,pois$TRTMT,mean)
trt.means

modAB = aov(TIME ~ factor(POISON)+factor(TRTMT), data = pois)
ls.trt = lsmeans(modAB, specs = ~TRTMT)
ls.trt

plot(ls.trt, main = "Survival time means by treatment",
     xlab = "Mean survival time in units of 10 hours",
     ylab = "Treatment factor")




##############################################################################
##############################################################################


# ANOVA formulas

mu11 = 2.7; mu21 = 2.5; mu31 = 3.2; mu12 = 3.1; mu22 = 2.9; mu32 = 4.2
sum(mu11,mu21,mu31,mu12,mu22,mu32)/6

# The easy way to check my answers for quiz 2 prep material:
I = c(1,1,0,0)
J = c(1,0,1,0)
mu = vector()
for (i in 1:length(I)){
  mu[i] = 356.36 - 56.16*I[i] - 11.65*J[i] + 18.51*I[i]*J[i]
}
mu




##############################################################################
##############################################################################

# In class exercse Kutner Ch 27 Problem 20

field = read.csv("/Users/jamescutler/Desktop/Biostats_II/CH27PR20.csv",
                 header = FALSE)
field = field[,-1]
colnames(field) = c("yield","campo","irrigation","fertilizer")

wheat = 
ggplot(field, aes(factor(campo),yield,col = factor(fertilizer))) +
  geom_point() +
  facet_grid(~factor(irrigation)) +
  labs(title = "Comparing irrigation method 1 and 2 side by side \nalong with fertilizer and field number",
       x="Field (numbered 1-5)",
       y="Wheat yield")
wheat

mod.ABC = aov(yield ~ factor(campo)+factor(fertilizer)+factor(irrigation),
              data = field)
summary(mod.ABC)
plot(fitted(mod.ABC),resid(mod.ABC))

ls.fert = lsmeans(mod.ABC, specs = ~factor(fertilizer))
ls.fert

ls.fld = lsmeans(mod.ABC, specs = ~factor(campo))
ls.fld

ls.irr = lsmeans(mod.ABC, specs = ~factor(irrigation))
ls.irr

plot(ls.fert,
     main = "Fertilizer 1 and 2 least squares means")
plot(ls.fld,
     main = "Fields 1-5 least squares means")
plot(ls.irr,
     main = "Irrigation 1 and 2 least squares means")

wheat


# Models:
## IS IT TRUE THAT YOU CAN'T DO A FULL MODEL WHERE YOU TEST ALL INTERACTIONS AT THE SAME TIME???
## THE OUTPUT FOR SUCH A MODEL DOESN'T EVEN HAVE P VALUES.
mod.irr_campo = aov(yield ~ factor(irrigation)*factor(campo)+factor(fertilizer),
                    data = field)
summary(mod.irr_campo)
mod.fert_campo = aov(yield ~ factor(fertilizer)*factor(campo)+factor(irrigation),
                     data = field)
summary(mod.fert_campo)

# Correlation matrix! Never used this before! Cool stuff!
mycormatrix = cor(field)
round(mycormatrix,2)


# sin(pi/4) = 1/sqrt(2)

# Radians to degrees and vice versa:
4*pi/180
(15/sin(4*pi/180))*sin(46*pi/180)

sin(90*pi/180)

154.68*sin(40*pi/180)


#############

.98*.0038/(.98*.0038 + .015*(1-.0038))

curve(log(x), from = -5, to = 10, n = 1000)
abline(h = 0, v = 0, col = "gray")




##############################################################################
##############################################################################


# Final

# Libraries needed for this:
# papeR - FREAKING AMAZING, see below ...
# xtable

sal = read.sas7bdat("/Users/jamescutler/Desktop/Biostats_II/salary.sas7bdat",
                    debug = TRUE)
head(sal,3)
colnames(sal) = c("Sex","Rank","Year","Degree","yr.dg","Salary")

S = data.table(sal)
# head(sal,3)
# S[sx == "male" & sl > 28000]
# plot(sal$sl ~ sal$sx)
# summary(sal$sl,sal$sx)
# S[, sl]
# S[, summary(sl)]
# 
# tapply(sal$sl, sal$sx, summary)
# 
# summarize(sal, type = "numeric")

edu.sal = sal[,c("Sex","Salary")]
summarize(edu.sal, type = "numeric", group = "Sex", test = FALSE) # FREAKING AMAZING

edu.rank = sal[,c("Sex","Rank")]; edu.rank
edu.rank = sal[,1:2]
summarize(edu.rank, type = "factor", group = "Sex", test = FALSE)


##### GIVING THE MODEL ALL THE CATEGORICAL VARIABLES LEAVES YOU WITH THE WRONG MEANS FOR GENDER:
modMainEff = aov(Salary ~ factor(Sex)+factor(Rank)+factor(Degree), data = sal)
lsmeans(modMainEff, specs = ~ Sex)

# THE RIGHT MEANS:
modSex = aov(Salary ~ factor(Sex), data = sal)
lsmeans(modSex, specs = ~ Sex)

sex.means = tapply(sal$Salary, sal$Sex, mean)
plot(sal$Salary ~ sal$Sex)
points(sex.means, col = "red", pch = 18)
abline(h = c(21357,24696), col = "red")
################################################

# There is no significant interaction between rank and sex:
modSexRank = aov(Salary ~ factor(Sex)*factor(Rank), data = sal)
summary(modSexRank)

# barplot(table(sal$Sex,sal$Rank)) # BUT HOW TO DO IT IN GGPLOT? ...
# ggplot(sal, aes(x=Rank, fill = Sex)) +
#   geom_bar(stat = "count") # ... GOOD TO KNOW HOW TO DO THIS REALLY SIMPLE BARPLOT IN GGPLOT!

# Interaction plot between rank and sex:
ggplot(sal, aes(x=factor(Rank),y=Salary, col = factor(Sex))) +
  geom_jitter(width = .1) +
  stat_summary(fun.y = "mean", geom = "line", aes(group = factor(Sex))) +
  labs(title = "Interaction plot for sex and rank", x="Academic Rank",y="Salary in USD",
       col = "Sex") +
  theme(panel.grid.major = element_line(colour = "gray"),
        panel.background = element_blank())

# Interaction plot between years in rank and years since degree? impossible to show:
ggplot(sal, aes(x=)) # what would x be? years in rank? or years since degree?

# Salary vs Year:
plot(sal$Year,sal$Salary)
ggplot(sal, aes(Year,Salary, col = Sex)) +
  geom_point(aes(shape = Rank),
             alpha = .7) +
  theme(panel.background = element_blank(),
        panel.grid.major = element_line(colour = "gray")) +
  labs(title = "Does gender vary with years in rank?",x="Years in current rank",
       y="Salary in USD")

head(sal,3)
unique(sal$Degree)

# Facet grid plot of Rank, Sex, and Degree:
ggplot(sal, aes(x=factor(Rank),y=Salary, col=factor(Sex))) +
  geom_point() +
  facet_grid(~factor(Degree))


  
# FULL ON COMPLETE (ANOVA) MODEL:
modFullAnova = aov(Salary ~ factor(Sex)*factor(Rank)*factor(Degree), data = sal)
summary(modFullAnova)

# Main effects:
modMainAnova = aov(Salary ~ factor(Sex)+factor(Rank)+factor(Degree), data = sal)
summary(modMainAnova)

# par(mfrow=c(1,1)) # THIS CAN'T CHANGE THE LSMEANS PLOTS! WTF!?!?!?
ls.MA = lsmeans(modMainAnova, specs = ~ Sex)
ls.SR = lsmeans(modSexRank, specs = ~ Sex)
plot(ls.MA,
     xlab = "Means and 95% CIs for salary by sex") 
title(main = "Means and 95% CIs for salary by sex")
plot(ls.SR)

# Plot lsmeans in ggplot:
ls.MAplot = summary(ls.MA)
summary(ls.MA)
ggplot(ls.MAplot, aes(Sex)) +
  geom_line(aes(y = lsmean)) + 
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL),
                width = .2) +
  geom_point(aes(y = lsmean), size = 3,
             shape = 21, fill = "white")

# Full MLR (years in rank and years since degree):
modFullMLR = lm(Salary ~ Year*yr.dg, data = sal)
summary(modFullMLR) # no interaction

# MAIN EFFECTS GLM WITH EVERYTHING:
modFullGLM = glm(Salary ~ factor(Sex) + factor(Rank) + Year + factor(Degree) + yr.dg,
                 data = sal)
summary(modFullGLM)

# sal$Sex = as.factor(sal$Sex)
# sal$Rank = as.factor(sal$Rank)
# sal$Degree = as.factor(sal$Degree)
# c = colnames(sal)[-6]
# c
# for (i in 2:length(colnames(sal))-1){
#   assign(paste0("modLS",c[i]), glm(Salary ~ eval(parse(text = c[i])), data = sal))
# }


# Problem 5: "arrive at an appropriately adjusted estimate of the difference in mean salary
## between male and female professors." Report methods and results. Provide a population
## estimate with CI that quantifies the relationship between salary and sex.

## First, are there interactions?
### Not between rank and degree, not between either of those and sex. 
#### So, not between any categorical variables.
### Not between years in rank and years since degree either.

# All categorical interactions:
modIntCat = aov(Salary ~ Sex*Rank*Degree, data = sal)
summary(modIntCat)

# Just rank and degree interactions:
modRankDeg = aov(Salary ~ Sex+Rank*Degree, data = sal)
summary(modRankDeg)

# Just rank and sex:

# Just degree and sex:



# Continuous interactions (years in rank and years since degree):
modYR.YD = lm(Salary ~ Year*yr.dg, data = sal)
summary(modYR.YD)

# GLM of all interactions (TOTALLY POINTLESS):
# modALLINT = glm(Salary ~ factor(Sex)*factor(Rank)*factor(Degree)*Year*yr.dg, data = sal)
# summary(modALLINT)

# GLM of interaction between sex and year (plus everything else):
modSexYr = glm(Salary ~ factor(Sex)*Year+Rank+Degree+yr.dg, data = sal)
summary(modSexYr)

# GLM of interaction between rank and years since degree (an actually interesting interaction):
modYD.Rank = glm(Salary ~ Rank*yr.dg+Sex+Degree+Year, data = sal)
summary(modYD.Rank)

ggplot(sal, aes(x=yr.dg,y=Salary, col = Rank)) +
  geom_point() +
  labs(title = "Does rank vary with years since degree?",x="Years since earning degree",
       y="Salary in USD")

# And all the rest of the interesting plots:
## Years since degree with rank and sex:
ggplot(sal, aes(x=yr.dg,y=Salary, col = Rank)) +
  geom_point(aes(shape = Sex)) +
  theme(panel.background = element_blank(),
        panel.grid.major = element_line(colour = "gray")) +
  labs(title = "Do rank and gender vary with years since degree?",
       x="Years since earning degree", y = "Salary in USD")

## Years in rank with rank (so people can see how rank is paid) and sex:
ggplot(sal, aes(Year,Salary, col = Sex)) +
  geom_point(aes(shape = Rank),
             alpha = .7) +
  theme(panel.background = element_blank(),
        panel.grid.major = element_line(colour = "gray")) +
  labs(title = "Does gender vary with years in rank?",x="Years in current rank",
       y="Salary in USD")



# Some numbers on sex and years in rank, and sex and years since degree, 
## followed by barplot of sex and rank:
fem5.10 =  S[Sex == "female" & Year %in% 5:10, length(Sex)]
fem0.4 =   S[Sex == "female" & Year %in% 0:4, length(Sex)]
male5.10 = S[Sex == "male" & Year %in% 5:10, length(Sex)]
male0.4 =  S[Sex == "male" & Year %in% 0:4, length(Sex)]
# 0% of professors are female after 10 years in rank
fem5.10/sum(fem5.10,male5.10) # 33% of professors with 5-10 years in current rank are female
fem0.4/sum(fem0.4,male0.4) # That rises to 40% with 0-4 years in rank

f.dg.0.9 = S[Sex == "female" & yr.dg < 10, length(Sex)]
f.dg.10.19 = S[Sex == "female" & yr.dg %in% 10:19, length(Sex)]
f.dg.20plus = S[Sex == "female" & yr.dg > 19, length(Sex)]
m.dg.0.9 = S[Sex == "male" & yr.dg < 10, length(Sex)]
m.dg.10.19 = S[Sex == "male" & yr.dg %in% 10:19, length(Sex)]
m.dg.20plus = S[Sex == "male" & yr.dg > 19, length(Sex)]
f.dg.0.9/sum(f.dg.0.9,m.dg.0.9)           # 37.5% under 10 years since degree are female
f.dg.10.19/sum(f.dg.10.19,m.dg.10.19)     # 18.75% between 10 and 19 years are female
f.dg.20plus/sum(f.dg.20plus,m.dg.20plus)  # 25% 20 years and over are female

# Sex and rank:
## Barplot of gender proportions in each rank:
ggplot(sal, aes(x=Rank, fill = Sex)) +
  geom_bar(stat = "count") +
  labs(title = "Figure 2b:Proportion of males and females in each rank") # IT LOOKS LIKE MORE WOMEN IN THE ASSISTANT RANK

# Interaction plot for gender and rank (THIS IS ONLY MISLEADING BECAUSE OF THE DISPROPORTIONATE AMOUNT OF WOMEN IN THE ASSISTANT RANK):
ggplot(sal, aes(x=factor(Rank),y=Salary, col = factor(Sex))) +
  geom_jitter(width = .1) +
  stat_summary(fun.y = "mean", geom = "line", aes(group = factor(Sex))) +
  labs(title = "Interaction plot for sex and rank", x="Academic Rank",y="Salary in USD",
       col = "Sex") +
  theme(panel.grid.major = element_line(colour = "gray"),
        panel.background = element_blank())

sal2 = sal
head(sal2)
class(sal2$Rank)
sal2$Rank = as.character(sal2$Rank)
sal2[which(sal2$Rank == "full"),2] = "3"
sal2[which(sal2$Rank == "associate"),2] = "2"
sal2[which(sal2$Rank == "assistant"),2] = "1"
sal2$Rank = as.numeric(sal2$Rank)

unique(sal2$Degree)
class(sal2$Degree)
sal2$Degree = as.character(sal2$Degree)
sal2[which(sal2$Degree == "masters"),4] = "1"
sal2[which(sal2$Degree == "doctorate"),4] = "2"
sal2$Degree = as.numeric(sal2$Degree)

head(sal)
# What would I like to adjust for?
## years in rank
## years since degree
## rank
## NOT for degree, since degree isn't significantly affecting salary

# What models will I need for these adjustments?
## modSexYR - includes: Sex + Year   ... and anything else?
## modSexYD - includes: Sex + yr.dg
## modSexRank includes: Sex + Rank   ... just combine all of these?

# modSexYR2 = lm(Salary ~ Sex + Year, data = sal2) 
# summary(modSexYR2) # EXACTLY THE SAME AS THE ONE BELOW:
# gmodSexYR = glm(Salary ~ Sex + Year, data = sal)
# summary(gmodSexYR) # EXACTLY THE SAME AS THE ONE BELOW:
# gmodSexYR = glm(Salary ~ factor(Sex) + Year, data = sal)
# summary(gmodSexYR) # EXACTLY THE SAME AS THE ONE BELOW:
modSexYR = lm(Salary ~ Sex + Year, data = sal)
summary(modSexYR)

# Creating categorical variables for years in rank and years since degree:
sal$yr.dg.cat = NA
sal$yr.rank.cat = NA

# Years in rank:
sal[which(sal$Year < 5),"yr.dg.cat"] = 1
sal[which(sal$Year %in% 5:10),"yr.dg.cat"] = 2
sal[which(sal$Year > 10),"yr.dg.cat"] = 3

# Years since degree:
sal[which(sal$yr.dg < 10),"yr.rank.cat"] = 1
sal[which(sal$yr.dg %in% 10:19),"yr.rank.cat"] = 2
sal[which(sal$yr.dg > 19),"yr.rank.cat"] = 3

sal$yr.dg.cat = as.factor(sal$yr.dg.cat)
sal$yr.rank.cat = as.factor(sal$yr.rank.cat)

# FIRST ANOVA ON RELATIONSHIP BETWEEN YEARS IN RANK OR YEARS SINCE DEGREE AND GENDER:
IntSex.YR.YD = aov(Salary ~ Sex*yr.rank.cat*yr.dg.cat, data = sal)
summary(IntSex.YR.YD)

ggplot(sal, aes(x=yr.rank.cat, fill = Sex)) +
  geom_bar(stat = "count") +
  labs(title = "Proportion of males and females by years in rank",
       x="1: 0-9 yrs; 2: 10-19 yrs; 3: 20 yrs and above")
ggplot(sal, aes(x=yr.dg.cat, fill = Sex)) +
  geom_bar(stat = "count") +
  labs(title = "Proportion of males and females by years since degree",
       x="1: 0-4 yrs; 2: 5-10 yrs; 3: over 10 yrs")
plot(sal$Salary ~ sal$yr.rank.cat,
     main = "Salary by (categorical) years in rank",
     xlab = "1: 0-9 yrs; 2: 10-19 yrs; 3: 20 yrs and above",
     ylab = "Salary")
YRmeans = tapply(sal$Salary,sal$yr.rank.cat, mean)
points(YRmeans, col = "red", pch = 18)
plot(sal$Salary ~ sal$yr.dg.cat,
     main = "Salary by (categorical) years since degree",
     xlab = "1: 0-4 yrs; 2: 5-10 yrs; 3: over 10 yrs",
     ylab = "Salary")
YDmeans = tapply(sal$Salary,sal$yr.dg.cat, mean)
points(YDmeans, col = "red", pch = 18)

# Main effects ANOVA:
modSex.YR.YD = aov(Salary ~ Sex+yr.rank.cat+yr.dg.cat, data = sal)
summary(modSex.YR.YD)

## Follow up on modSex.YR.YD:
kmodSex.YR.YD = Anova(modSex.YR.YD, type=2) # WOW. THE DIFFERENCE BETWEEN SEXES GOES AWAY.

### Adjusting for years since degree:
YD.Sexmeans = emmeans(modSex.YR.YD, ~ Sex | yr.dg.cat)
plot(YD.Sexmeans,
     # main = "Least squares means of salary by sex, adjusted for years since degree, with 95% CIs", # WHAT THE FUH!?!?!? WHY DOESN'T IT PRINT THE MAIN TITLE?!?!?!?!?
     xlab = "Salary in USD", ylab = "")

### Adjusting for years in rank:
YR.Sexmeans = emmeans(modSex.YR.YD, ~ Sex | yr.rank.cat)
plot(YR.Sexmeans,
     xlab = "Salary in USD", ylab = "")


# What about ANOVA on Sex with rank, in addition to yrs in rank and yrs since degree?
modSex.R.YR.YD = aov(Salary ~ Sex+Rank+yr.rank.cat+yr.dg.cat, data = sal)
summary(modSex.R.YR.YD)

## Follow up on modSex.R.YR.YD:
kmodSex.R.YR.YD = Anova(modSex.R.YR.YD, type=2)

### Adjusting for rank:
R.Sexmeans = emmeans(modSex.R.YR.YD, ~ Sex | Rank)
plot(R.Sexmeans,
     xlab = "Salary in USD",
     ylab="") # THE MOST BIZARRE RESULT OF ALL. COMPARE THESE TWO PLOTS AND YOU'LL SEE THE EMMEANS ACTUALLY THINKS THE MALE MEANS ARE EVER SO SLIGHTLY ***LOWER***, BUT THAT CAN'T BE TRUE.
ggplot(sal, aes(x=factor(Rank),y=Salary, col = factor(Sex))) +
  geom_jitter(width = .1) +
  stat_summary(fun.y = "mean", geom = "line", aes(group = factor(Sex))) +
  labs(title = "Interaction plot for sex and rank", x="Academic Rank",y="Salary in USD",
       col = "Sex")

# lsmeans and emmeans are exactly the same when used on Sex overall: 
## (I don't know how to use lsmeans on each individual adjustment)
ls.sexmeans = lsmeans(modSex.R.YR.YD, specs = ~ Sex)
plot(ls.sexmeans)
Sexmeans = emmeans(modSex.R.YR.YD, ~ Sex)
plot(Sexmeans)





# PART IV

hor = read.sas7bdat("/Users/jamescutler/Desktop/Biostats_II/concentration4.sas7bdat",
                    debug = TRUE)
head(hor)
plot(hor$conc ~ factor(hor$group))
plot(hor$conc ~ factor(hor$time))

ggplot(hor, aes(x=factor(time),y=conc, fill = factor(group))) +
  geom_boxplot() +
  stat_summary(fun.y = mean, colour = "yellow", geom = "point",
               shape = 18, size = 2, show.legend = FALSE) +
  labs(title = "Changes in hormone concentration before and \nafter injection in both groups",
       x="1: Before injection \n2-4: Three separate times after injection",
       y="Hormone concentration (mg/dl)", fill = "Red: placebo\nBlue: trtmnt")

hor1 = hor[which(hor$group == 1),]
hor2 = hor[which(hor$group == 2),]
g.means1 = tapply(hor1$conc, hor1$time, mean); g.means1
g.means2 = tapply(hor2$conc, hor2$time, mean); g.means2
plot(1:4,g.means1, col = "red", ylim = c(25,40))
points(1:4,g.means2, col = "green")


fullmod.T.cat = aov(conc ~ factor(time)*factor(group), data = hor)
summary(fullmod.T.cat) # No interaction
MEmod.T.cat = aov(conc ~ factor(time)+factor(group), data = hor)
summary(MEmod.T.cat)

Groupmeans = emmeans(MEmod.T.cat, ~ group | time) # doesn't make any difference if you factor() them or not
plot(Groupmeans)

Groupmeans
Gmeans.df = summary(Groupmeans)
plot(Gmeans.df$time, Gmeans.df$emmean, col = "orange", ylim = c(24,40),
     main = "Why are the emmeans adjusted means spaced\n apart so evenly, unlike the real means?",
     xlab = "Times 1-4", ylab = "Concentration")
points(1:4,g.means1, col = "red", ylim = c(25,40))
points(1:4,g.means2, col = "blue")
trtmnt = lm(conc ~ time, data = hor2)
placebo = lm(conc ~ time, data = hor1)
abline(trtmnt, col = "blue")
abline(placebo, col = "red")

Gmeans.df[1:2,]

# TRYING TO SEE THE DAMN ERROR BARS (for #9):
mod1.time = aov(conc ~ factor(time), data = hor1)
mod2.time = aov(conc ~ factor(time), data = hor2)
ls.1 = lsmeans(mod1.time, specs = ~ factor(time))
ls.2 = lsmeans(mod2.time, specs = ~ factor(time))
ls.1plot = summary(ls.1)
ls.2plot = summary(ls.2)

plot(1:4,ls.1plot$lsmean, col = "red", ylim = c(20,45), xlim = c(1,4.3),
     main = "The differences between the mean concentrations\n of the two groups at each time interval,\n with fitted regression curves",
     xlab = "Times 1-4", ylab = "Serum concentration of hormone (mg/dl)")
points(seq(1.1,4.1,1),ls.2plot$lsmean, col = "blue")
segments(x0=1:4,y0=ls.1plot$lower.CL,x1=1:4,y1=ls.1plot$upper.CL, col = "red")
segments(x0=seq(1.1,4.1,1),y0=ls.2plot$lower.CL,x1=seq(1.1,4.1,1),y1=ls.2plot$upper.CL, col = "blue")
red = as.data.frame(cbind(1:4,ls.1plot$lsmean))
blue = as.data.frame(cbind(1:4,ls.2plot$lsmean))
red.reg = lm(V2 ~ V1, data = red)
blue.reg = lm(V2 ~ V1, data = blue)
abline(red.reg, col = "red")
abline(blue.reg, col = "blue")

x = 1:nrow(dep2)
plot(x,dep2$Mean, col = "blue", ylim = c(0,1))
segments(x,dep2$val2.5,x,dep2$val97.5, col = "blue") # AMAZING! 

# ls.1plot = summary(ls.1)
# ls.2plot = summary(ls.2)
ggplot() +
  geom_line(data = ls.1plot, mapping = aes(x=factor(time),y=lsmean)) +
  geom_line(data = ls.2plot, mapping = aes(x=factor(time),y=lsmean)) +
  geom_errorbar(data = ls.1plot, mapping = aes(x=factor(time),ymin = lower.CL, ymax = upper.CL)) +
  geom_errorbar(data = ls.2plot, mapping = aes(x=factor(time),ymin = lower.CL, ymax = upper.CL)) +
  geom_point(data = ls.1plot, mapping = aes(x=factor(time),y = lsmean), size = 3,
             shape = 21, fill = "red") +
  geom_point(data = ls.2plot, mapping = aes(x=factor(time),y = lsmean), size = 3,
             shape = 21, fill = "blue")

ggplot(hor, aes(factor(time),conc, col = factor(group))) +
  geom_jitter(width = .1) + 
  stat_summary(fun.y = "mean", geom = "line", aes(group = factor(group))) +
  labs(title = "Change in hormone concentration over time, by group",
       x="Times 1-4",y="Serum concentration of hormone (mg/dl)")




# We can't do ANOVA on this. The variances are not constant. The subjects in each group
## are not independent. It will have to be a linear regression model.
# Inthormod = lm(conc ~ time*group, data = hor)
# summary(Inthormod) # no interaction
Intmod = glm(conc ~ time*factor(group), data = hor) # IN THE INSTRUCTIONS, SHE HAS A MODEL LIKE THIS WHERE TIME ONLY HAS ONE ROW OF VALUES. THAT MEANS TIME HAS TO NOT BE A FACTOR IN THIS MODEL.
summary(Intmod) 
confint(Intmod)

Int2 = glm(conc ~ factor(group)+time+factor(group):time, data = hor) # EXACTLY THE SAME AS Intmod, above.
summary(Int2)
# MEhormod = lm(conc ~ time+group, data = hor)
# summary(MEhormod)

# But I'm supposed to treat time as a categorical variable:
MEfactors = lm(conc ~ factor(time)+factor(group), data = hor)
summary(MEfactors)
plot(fitted(MEfactors),resid(MEfactors))
ncvTest(MEfactors)

# STILL STUPID WRONG ESTIMATES. STUPID EMMEANS.
# Groupmeans = emmeans(MEfactors, specs = ~ factor(group) | factor(time))
# plot(Groupmeans)

ls.1plot$lsmean[4]
ls.2plot$lsmean[4] - ls.1plot$lsmean[4]


# 10. Did group 1 change ever?
mod1 = lm(conc ~ factor(time), data = hor1)
summary(mod1)

?cov
cov(hor$time,hor$conc)
cov(hor[,2:4])

pairs(hor[,2:4])
(sd(hor$time))^2


# PART II:

mu11 = 26.8 - 2.65 + 4.4 - 2.99; mu11
mu21 = 26.8 + 4.4; mu21

my11 = 25.19 + 1.16; my11
my21 = 25.19 - 2.1 + 1.16 + 2.7; my21

25.56/4
31.2/4

curve((.7)*x + 26.8, xlim = c(0,4), ylim = c(25,45))
curve((4.5)*x + 26.8, col = "red", add = TRUE)
abline(h = 30, col = "gray", lty = 2)
abline(v = 1, col = "gray", lty = 2)
abline(h = 35, col = "gray", lty = 2)
abline(v = 2, col = "gray", lty = 2)
