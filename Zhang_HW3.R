# HW 3 James Cutler - Dr. Zhang
CDI = read.csv("/Users/jamescutler/Desktop/Biostats_II/APPENC02.csv", 
               header = FALSE)
CDI = CDI[,3:ncol(CDI)]
colnames(CDI) = c("County","State","S_Area","Pop","Per18_34","Per65up","Physicians",
                  "Hosp_beds","Crimes","PerHSgrads","PerBach","PerPoor","PerUnemp",
                  "PerCapInc","PersonalInc","GeoReg")

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