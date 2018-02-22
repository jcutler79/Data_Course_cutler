# Assignment 4 for credit

library(fitdistrplus)
library(ggplot2)
library(modelr)

# Not sure what you want me to do with the address. I think you told us something about this once, but I forgot.
mushrooms = read.csv("/Users/jamescutler/Desktop/Course_Materials/Data_Course/data/mushroom_growth.csv")

# There are five factors tested here that might affect growth rate:
boxplot(mushrooms$GrowthRate ~ mushrooms$Nitrogen)
boxplot(mushrooms$GrowthRate ~ mushrooms$Light)
boxplot(mushrooms$GrowthRate ~ mushrooms$Humidity)
boxplot(mushrooms$GrowthRate ~ mushrooms$Temperature)
boxplot(mushrooms$GrowthRate ~ mushrooms$Species)
# 20-25 Nitrogen is best (barely)
# 20 light (highest level) is best ... we'll see more definitive analysis later on
# high humidity is best
# 20 temperature has more high growth outliers, similar mean as 25 temp
# cornucopiae is a slightly higher mean, Q3, max, and high outliers where ostreotus has no outliers

qqnorm(mushrooms$GrowthRate)
hist(mushrooms$GrowthRate) # Most of these guys aren't growing very much ...
plot(density(mushrooms$GrowthRate))
mean(mushrooms$GrowthRate) # ... in fact, the mean is 113.6 growth units.
plot(fitdist(log10(mushrooms$GrowthRate), distr = "norm")) # BEST FIT! YAY!
plot(fitdist(mushrooms$GrowthRate, "norm")) # worst fit
plot(fitdist(mushrooms$GrowthRate, "gamma")) # gamma is better norm and logistic
plot(fitdist(mushrooms$GrowthRate, "logis")) # worse than gamma, perhaps better than norm
length(which(mushrooms$Species == "P.ostreotus")) # Just wanted to see if it was half and half or not. It is.

shroom.model = aov(mushrooms$GrowthRate ~ mushrooms$Nitrogen + mushrooms$Light + 
                    mushrooms$Humidity + mushrooms$Temperature + mushrooms$Species)
anova(shroom.model) # light, humidity, and species have the greatest effect on growth.
# Nitrogen doesn't appear to have much of an effect, and temperature is marinally significant

# crappy model of interactions between ALL variables model:
crap.model = aov(mushrooms$GrowthRate ~ mushrooms$Nitrogen*mushrooms$Light*mushrooms$Humidity*mushrooms$Temperature*mushrooms$Species)
anova(crap.model) # Hoooooly crap ...
## There are 3-star interactions between:
# humidity and light
# humidity and temp
# humidity and species
# species and light
# species and temp
# humidity, species, light
# humidity, species, temp

# This is why we should plot graphs. The crap model won't catch everything:
ggplot(mushrooms, aes(x = Nitrogen, y = log10(GrowthRate), col = Humidity)) +
  geom_point() + stat_smooth() + ggtitle("mushroom growth rate") + 
  facet_grid(facets = ~ Species) 

## So now I'll put up some sweet ggplots of these variables (e.g. humidity or
## light as X variable, the other as color code, to see how both explain growth):
# Light as X, color-coded by humidity:
ggplot(mushrooms, mapping = aes(x = Light, y = GrowthRate, col = Humidity)) +
  geom_point() + geom_smooth(method = "aov", se = FALSE) + 
  ggtitle("Growth as a function of light") + 
  labs(subtitle = "... color-coded by humidity")
# Temp as X, color-coded by humidity:
ggplot(mushrooms, mapping = aes(x = Temperature, y = GrowthRate, col = Humidity)) +
  geom_point() + geom_smooth(method = "aov", se = FALSE) + 
  ggtitle("Growth as a function of temperature") + 
  labs(subtitle = "... color-coded by humidity") # 20 degrees is better than 25!
# Species as X, color-coded by humidity:
ggplot(mushrooms, mapping = aes(x = Species, y = GrowthRate, col = Humidity)) +
  geom_point() + geom_smooth(method = "aov", se = FALSE) + 
  ggtitle("Growth as a function of species") + 
  labs(subtitle = "... color-coded by humidity")
# Light as X, color-coded by species:
ggplot(mushrooms, mapping = aes(x = Light, y = GrowthRate, col = Species)) +
  geom_point() + geom_smooth(method = "aov", se = FALSE) + 
  ggtitle("Growth as a function of light") + 
  labs(subtitle = "... color-coded by species") # cornucopiae improves at a higher rate, duh! Could have predicted that
# Temperature as X, color-coded by species:
ggplot(mushrooms, mapping = aes(x = Temperature, y = GrowthRate, col = Species)) +
  geom_point() + geom_smooth(method = "aov") + 
  ggtitle("Growth as a function of temperature") + 
  labs(subtitle = "... color-coded by species") # Ah HAAA! Cornucopiae may have done worse 
# at 25 degrees than at 20, but temp does not appear to affect ostreotus.

# Not sure what to do with the triple interactions that are significant. I guess I'll
# do some summaries and tidies. 
mod1 = aov(GrowthRate ~ Humidity + Species + Light, data = mushrooms)
mod1
summary(mod1)
tidy(mod1)
mod2 = aov(GrowthRate ~ Humidity + Species + Temperature, data = mushrooms); mod2
summary(mod2)
tidy(mod2)
# But this didn't really add anything meaningful to my analysis that I didn't already 
# know, right? I'll try interactions between all three in each of the models below.

## In the meanwhile, here's some more good stuff we can do with ggplot:
# Compare the species in a cool way (light = X, colored by humidity, grid the species):
ggplot(mushrooms, aes(x = Light, y = log10(GrowthRate), col = Humidity)) +
  geom_point() + stat_smooth() + ggtitle("mushroom growth rate") + 
  facet_grid(facets = ~ Species) # Hell yeeeahh! I LOVE this stuff. Basically, we can
# see that cornucopiae does significantly worse in low humidity, but ostreotus does not
# do all that worse. My next question is, what would this graph look like without a log10
# transformation?
ggplot(mushrooms, aes(x = Light, y = GrowthRate, col = Humidity)) +
  geom_point() + stat_smooth() + ggtitle("mushroom growth rate") + 
  facet_grid(facets = ~ Species) # Whoa. That does look a little different, but the trends
# appear overall to be about the same.

# Temperature is not something I would predict to be as interesting to plot this way,
# but what the heck:
ggplot(mushrooms, aes(x = Temperature, y = log10(GrowthRate), col = Humidity)) +
  geom_point() + stat_smooth() + ggtitle("mushroom growth rate") + 
  facet_grid(facets = ~ Species) # HOLY! So, under high humidity, cornucopiae does slightly
# worse at 25 degrees than it does at 20; however, under low humidity, it does slightly
# BETTER at 25 than at 20. On the other hand, ostreotus does slightly better under high
# humidity at 25 degrees compared to 20, but shows the exact reverse trend under low 
# humidity. These shrooms real? Or did you artificially craft this data to be interesting?



## Now on to predictive models!
# I'm going to create 2 aov models--one for light*humidity*species, another for 
# nitrogen*humidty*species, because those two looked interesting in the plots:
int.mod1 = aov((GrowthRate) ~ Light*Humidity*Species, data = mushrooms)
summary(int.mod1)
plot(int.mod1) # Bunch of hairy stuff
tidy(int.mod1)

int.mod2 = aov((GrowthRate) ~ Nitrogen*Humidity*Species, data = mushrooms)
summary(int.mod2)
plot(int.mod2)
tidy(int.mod2)


anova(int.mod2,int.mod1)
summary(int.mod1)
summary(int.mod2)
dev.off() # I FORGET WHY I DO THIS ... ??????????
mushrooms2 = add_predictions(mushrooms, model = int.mod2)
mushrooms$Log10 = log10(mushrooms$GrowthRate)
plot(mushrooms$GrowthRate, mushrooms$pred) # Not very digestible
plot(mushrooms$pred, mushrooms$GrowthRate) # Same
mean(((mushrooms2$GrowthRate - mushrooms2$pred)^2)) # The mean is 6412.843 for the squared differences 
# between the Nitrogen/humidity/species model predictions, whereas the Light/humidity/species mean is ...
mean((mushrooms$GrowthRate - mushrooms$pred)^2) # 3726.525, almost twice as close. So I'll go with 
# int.mod1 as my model for cross validation:

mushrooms$Log10 = NULL

## Cross validation:
'%ni%' = Negate('%in%')
mushrooms$Cross = rnorm(length(mushrooms$Species))
mush.sample = sample(mushrooms$Cross, 108)
mush.train = mushrooms[which(mushrooms$Cross %in% mush.sample),]
mush.cross = mushrooms[which(mushrooms$Cross %ni% mush.sample),]

int.mod1.trained = aov(GrowthRate ~ Light*Humidity*Species, data = mush.train)
plot(int.mod1.trained)
summary(int.mod1.trained)

cross.predictions = add_predictions(model = int.mod1.trained, data = mush.cross)
mean((cross.predictions$pred - cross.predictions$GrowthRate)^2) # the mean is 5371. Not as good
# as the overfitted model based on the full data set, but still better than the Nitrogen model.
# Interesting stuff

# Thank you a ton for all your help Dr. Zahn!

