### Zhang_HW2

# James Cutler
# WK 2 homework Zhang 

# ALSM 1.43, 2.2, 2.17, 2.62

### 1.43
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



