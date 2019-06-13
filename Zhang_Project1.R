# James Cutler
# Project 1 

library(sas7bdat)
library(ggplot2)
library(dplyr)
library(qwraps2)
library(stargazer)
?stargazer


smoke = read.sas7bdat("/Users/jamescutler/Desktop/Biostats_II/SmokingAndFEV.sas7bdat",
                      debug = TRUE)

smoke$sex = factor(smoke$sex,
                   levels = c(0,1),
                   labels = c("female","male"))
smoke$smoke = factor(smoke$smoke,
                     levels = c(0,1),
                     labels = c("nonsmoker","smoker"))
head(smoke,5)

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
par(mfrow=c(2,2))
par(mar=c(2,2.1,1.5,1))
plot(smoke$fev ~ smoke$smoke, main = "FEV by smoking status")
smkMeans = tapply(smoke$fev, smoke$sex, mean)
points(smkMeans, col = "red", pch = 18)
plot(smoke$fev ~ smoke$sex, main = "FEV by sex")
sxMeans = tapply(smoke$fev, smoke$sex, mean)
points(sxMeans, col = "red", pch = 18)
plot(smoke$age,smoke$fev, main = "FEV by age")
plot(smoke$height,smoke$fev, main = "FEV by height")
par(mar=c(5.1,4.1,4.1,2.1))
par(mfrow=c(1,1))
### There **appears** to be an association between smoking status and FEV, based on the relevant boxplot, above. There is also an apparent association between age/height and FEV, however. Sex and FEV might have a slight association as well.

## Hypothesis tests:
## Multiple regression (full and reduced models):
MRmod_full = lm(fev ~ smoke*sex*age*height, data = smoke); summary(MRmod_full)
MRmod_reduced = lm(fev ~ smoke+sex+age+height, data = smoke); summary(MRmod_reduced)
confint(MRmod_reduced, level = .95)

MRmod = lm(fev ~ sex+height+age+smoke+sex*smoke+height*smoke+age*smoke, data = smoke)
summary(MRmod)

plot(resid(MRmod))
plot(resid(MRmod),fitted(MRmod))

MRmod2 = lm(fev ~ sex+height+age+smoke+height*smoke+age*smoke, data = smoke)
summary(MRmod2)

plot(resid(MRmod2))
plot(resid(MRmod2),fitted(MRmod2))
?stargazer

### The full model indicates that there is are interactions that are highly significant:
#### - sex and age
#### - sex, age and height
### The reduced model indicates that sex, age, and height all have a highly significant association with FEV, but that smoking status does not.
### Given the interaction between sex and age, it would be interesting to see that visually:
### Interaction between age and sex on FEV:
ggplot(smoke, aes(age,fev, col = sex)) +
  geom_point() + 
  theme_bw()
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
