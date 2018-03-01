rm(list = ls())

library(dplyr)
library(tidyr)
library(ggplot2)
library(broom)
library(fitdistrplus)
library(MASS)
library(lme4)


df = CO2; df

plot(df)

plot(df$conc,df$uptake)

df$Treatment.fac = as.factor(df$Treatment)
plot(df$Treatment.fac, df$uptake)
df$Plant.fac = as.factor(df$Plant)
plot(df$Plant.fac, df$uptake)

Quebec.plants = df[which(df$Type == "Quebec"),]
Miss.plants = df[which(df$Type == "Mississippi"),]
class(Quebec.plants)
class(Miss.plants)

Q = Quebec.plants
M = Miss.plants
rm(Quebec.plants,Miss.plants)
Q$Plant.fac = as.factor(Q$Plant)
plot(Q$Plant.fac, Q$uptake)

Q.Qn3.fac = as.factor(which(Q$Plant == "Qn3"))
Q.Qc3.fac = as.factor(which(Q$Plant == "Qc3"))
plot(c(Q.Qn3.fac,Q.Qc3.fac),Q[c(15:21,36:42),5])

Q1 = as.data.frame(Q[c(which(Q$Plant == "Qn1"),which(Q$Plant == "Qc1")),])
plot(Q1$conc,Q1$uptake, col = Q1$Plant)
Q2 = as.data.frame(Q[c(which(Q$Plant == "Qn2"),which(Q$Plant == "Qc2")),])
plot(Q2$conc,Q2$uptake, col = Q2$Plant)
Q3 = as.data.frame(Q[c(which(Q$Plant == "Qn3"),which(Q$Plant == "Qc3")),])
plot(Q3$conc,Q3$uptake, col = Q3$Plant)
M1 = as.data.frame(M[c(which(M$Plant == "Mn1"),which(M$Plant == "Mc1")),])
plot(M1$conc,M1$uptake, col = M1$Plant)
M2 = as.data.frame(M[c(which(M$Plant == "Mn2"),which(M$Plant == "Mc2")),])
plot(M2$conc,M2$uptake, col = M2$Plant)
M3 = as.data.frame(M[c(which(M$Plant == "Mn3"),which(M$Plant == "Mc3")),])
plot(M3$conc,M3$uptake, col = M3$Plant)

t.test(Q1[which(Q1$Plant == "Qn1"),5],Q1[which(Q1$Plant == "Qc1"),5])
t.test(Q2[which(Q2$Plant == "Qn2"),5],Q2[which(Q2$Plant == "Qc2"),5])
t.test(Q3[which(Q3$Plant == "Qn3"),5],Q3[which(Q3$Plant == "Qc3"),5])

t.test(M1[which(M1$Plant == "Mn1"),5],M1[which(M1$Plant == "Mc1"),5])
t.test(M2[which(M2$Plant == "Mn2"),5],M2[which(M2$Plant == "Mc2"),5])
t.test(M3[which(M3$Plant == "Mn3"),5],M3[which(M3$Plant == "Mc3"),5])


fitdist(df$uptake)
plot(fitdist(df$uptake, distr = "norm"))

plot(fitdist(log10(df$uptake), distr = "norm"))



# Q1 c vs n
# Q2 c vs n
# Q3 c vs n
# M1, 2, 3, c vs n

# Q vs M
# all c vs all n


############################################################################
# Instructor's version:

rm(list = ls())

library(dplyr)
library(tidyr)
library(ggplot2)
library(broom)
library(fitdistrplus)
library(MASS)
library(lme4)
library(modelr)

df = CO2



##########
?fitdist

glimpse(df)
plot(fitdist(df$uptake, distr = "norm"))
plot(fitdist(df$uptake, distr = "lnorm"))
plot(fitdist(df$uptake, distr = "logis"))
plot(fitdist(df$uptake, distr = "gamma"))


plot(fitdist(log10(df$uptake), distr = "norm")) # this looks best; no it doesn't
plot(fitdist(sqrt(df$uptake), distr = "norm"))

denscomp(fitdist(df$log10_uptake, distr = "norm")) # pulls an error - "data must
# be a numeric vector of length greater than 1"


# # try box-cox transformation
# Box = boxcox(uptake ~ conc, data = df, lambda = seq(-6,6,0.1))
# Cox = data.frame(Box$x, Box$y)
# Cox2 = Cox[with(Cox, order(-Cox$Box.y)),]
# Cox2[1,]
# lambda = Cox2[1, "Box.x"]
# df$Turbidity_box = (df$uptake ^ lambda - 1)/lambda  
# 
# plot(fitdist(df$Turbidity_box, distr = "norm"))
# 
# #####
# log10 transformation looked like best strategy

df$log10_uptake = log10(df$uptake)

# exploratory plots

boxplot(df$uptake ~ df$Treatment*df$Type, col = c("Lightgrey","White"))

?ggplot
?aes
?geom_point

p1 = ggplot(df, aes(x=conc, y=log10_uptake, col=Treatment)) +
  geom_point() +
  stat_smooth() + ggtitle("CO2 Uptake")
p1

p.dog = ggplot(mtcars, aes(wt,mpg))
p.dog + geom_point(aes(col = factor(cyl))) # easiest to understand visually
p.dog + geom_point(aes(shape = factor(cyl))) # not nearly as easy to see
p.dog + geom_point(aes(size = qsec)) # That's cool!
p.dog + geom_point(aes(size = qsec)) + scale_size()
my.cars = mtcars
p.dog + geom_point(aes(col = cyl)) + scale_color_gradient(low="blue") # same concept as just raw color
# but not as easy to see! In other words, cool but inferior to the simpler application.
p.dog + geom_point(aes(shape = factor(cyl))) + scale_shape(solid = FALSE)
?scale_size
ggplot(mtcars, aes(wt,mpg)) + geom_point(col = "red", size = 5) + scale_shape(solid = FALSE)



p2 = ggplot(df, aes(x=conc, y=log10_uptake, col=Type)) +
  geom_point() +
  stat_smooth() + ggtitle("CO2 Uptake")
p2

p3 = ggplot(df, aes(x=conc, y=log10_uptake, col=Treatment)) +
  geom_point() +
  stat_smooth() + ggtitle("CO2 Uptake") +
  facet_grid(facets = ~ Type)
p3

# ANOVA model

mod1 = aov(log10_uptake ~ conc*Treatment*Type, data = df)
summary(mod1)
plot(mod1)
tidy(mod1)

# check model predictions
df = add_predictions(df,model = mod1)

plot(df$log10_uptake, df$pred)
abline(lm(df$pred ~ df$log10_uptake))

mean(sum((df$log10_uptake - df$pred)^2))

p3

TukeyHSD(mod1)

# Make some specific predictions
df2 = data.frame(conc = c(1200,1000,750,500), Type = factor(c("Quebec","Quebec","Mississippi","Mississippi")),
                 Treatment = factor(c("nonchilled","chilled","nonchilled","chilled")))
df2

mod2 = aov(log10_uptake ~ conc, data = df)

predictions = predict(mod1, newdata = df2)
predictions2 = predict(mod2, newdata = df2)

# plot predictions along with data
plot(df$log10_uptake ~ df$conc, xlim = c(0,1200), ylim = c(0,2))
points(df2$conc, predictions, col = "Red", pch = 19)
points(df2$conc, predictions2, col = "Blue", pch = 19)

ggplot() +
  geom_point(aes(x=df$conc,y=df$log10_uptake,col=df$Type)) +
  geom_point(aes(x=df2$conc,y=predictions,col=df2$Type), cex = 5, pch = 15) +
  geom_point(aes(x=df2$conc,y=predictions2,col=df2$Type),cex = 5, pch = 0) +
  geom_smooth(aes(x=df$conc,y=df$log10_uptake,col=df$Type)) +
  labs(title="CO2 uptake vs CO2 concentration", subtitle = "Boxes indicate model predictions for arbitrary values",
       x = "Log10 CO2 Uptake", y = "CO2 concentration")





######## Cross-Validation

# Any model trained on a data set will be baised toward good predictions for that data set!!!

c(1,4,4,4,2,3,4,5) %in% c(4,5,6,7,8)


'%ni%' = Negate('%in%')

df$Cross = rnorm(length(df$Plant))
df.sample = sample(df$Cross, 48)
df.train = df[which(df$Cross %in% df.sample),]
df.cross = df[which(df$Cross %ni% df.sample),]

mod3 = aov(log10_uptake ~ conc*Treatment*Type, data = df.train)
plot(mod3)
mod3
summary(aov(mod3))


(anova(mod1,mod2))


cross.predictions = add_predictions(model = mod3, data = df.cross)

mean((cross.predictions$pred - cross.predictions$log10_uptake)^2)

