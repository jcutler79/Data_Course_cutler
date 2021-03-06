# HW 3 James Cutler - Dr. Zhang Biostats II

library(ggplot2)
library(dplyr)

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
## It appears that a linear model is not a good fit because of the non-random patterns

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
abline(mod1.43_pop, col = "red")
# Regressed on hospital beds:
plot(CDI$Hosp_beds,CDI$Physicians, pch = 16, cex = .2, col = "red",
     main = "Physicians regressed on hospital beds",
     xlab = "Hospital beds", ylab = "Number of Physicians")
abline(mod1.43_beds, col = "blue")
# Regressed on personal income:
plot(CDI$PersonalInc,CDI$Physicians, pch = 16, cex = .2, col = "red",
     main = "Physicians regressed on personal income",
     xlab = "Personal Income", ylab = "Number of Physicians")
abline(mod1.43_inc, col = "purple")

# What if we remove the two outliers?
sorted = head(arrange(CDI[,c(1,4)],desc(Pop)),30)
par(mar=c(8,4,1,1))
barplot(sorted$Pop, names.arg = sorted$County, las = 2)
# The two outliers are Los Angeles and Cook
C2 = CDI[-c(1,2),]
# Plots without those two outliers, but with the models from the data including them:
plot(C2$Pop, C2$Physicians, pch = 16, cex = .4, 
     main = "Physicians regressed on population",
     xlab = "Population", ylab = "Number of Physicians")
abline(mod1.43_pop, col = "red")
plot(C2$Hosp_beds, C2$Physicians, pch = 16, cex = .4, 
     main = "Physicians regressed on beds",
     xlab = "Beds", ylab = "Number of Physicians")
abline(mod1.43_beds, col = "blue")
plot(C2$PersonalInc, C2$Physicians, pch = 16, cex = .4, 
     main = "Physicians regressed on income",
     xlab = "Income", ylab = "Number of Physicians")
abline(mod1.43_inc, col = "purple")

# Compare regression models based on the NEW data to the old models 
## (barely any difference in any of them, as you can see): 
# Population:
newpop = lm(Physicians ~ Pop, data = C2)
plot(C2$Pop, C2$Physicians, pch = 16, cex = .4, 
     main = "Physicians regressed on population",
     xlab = "Population", ylab = "Number of Physicians")
abline(newpop, col = "red")
plot(C2$Pop, C2$Physicians, pch = 16, cex = .4, 
     main = "Physicians regressed on population",
     xlab = "Population", ylab = "Number of Physicians")
abline(mod1.43_pop, col = "red")
# Beds:
newbeds = lm(Physicians ~ Hosp_beds, data = C2)
plot(C2$Hosp_beds, C2$Physicians, pch = 16, cex = .4, 
     main = "Physicians regressed on beds",
     xlab = "Beds", ylab = "Number of Physicians")
abline(newbeds, col = "blue")
plot(C2$Hosp_beds, C2$Physicians, pch = 16, cex = .4, 
     main = "Physicians regressed on beds",
     xlab = "Beds", ylab = "Number of Physicians")
abline(mod1.43_beds, col = "blue")
# Income:
newinc = lm(Physicians ~ PersonalInc, data = C2)
plot(C2$PersonalInc, C2$Physicians, pch = 16, cex = .4, 
     main = "Physicians regressed on income",
     xlab = "Income", ylab = "Number of Physicians")
abline(newinc, col = "purple")
plot(C2$PersonalInc, C2$Physicians, pch = 16, cex = .4, 
     main = "Physicians regressed on income",
     xlab = "Income", ylab = "Number of Physicians")
abline(mod1.43_inc, col = "purple")

# 6. OMITTED VARIABLES:
# Plotting the residuals of each of the three models against 65 and up:
plot(CDI$Per65up,resid(mod1.43_pop))
abline(h = 0, col = "grey", lty = 2)
plot(CDI$Per65up,resid(mod1.43_beds))
abline(h = 0, col = "grey", lty = 2)
plot(CDI$Per65up,resid(mod1.43_inc))
abline(h = 0, col = "grey", lty = 2)
## Plotting residuals of each of the three models against the 65 and up 
## potential omitted variable shows no patterns suggesting that including 
## 65 and up as an additional predictor would contributed meaningfully to the models.











