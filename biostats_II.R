### Biostats Methods II

## Website for ALSM datasets:
# http://users.stat.ufl.edu/~rrandles/sta4210/Rclassnotes/data/textdatasets/Chapter%20%201%20Data%20Sets.html

## Website for R datasets:
# https://vincentarelbundock.github.io/Rdatasets/datasets.html 

# libraries:
library(ggplot2)
library(sas7bdat)


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






